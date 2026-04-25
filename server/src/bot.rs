// Server-side AI ships. Port of "sid" from the original Elm xpilot — every
// tick the brain either dodges the most-dangerous incoming bullet or attacks
// the nearest other ship.
//
// Aim mode flips between LEAD (predict where the target will be using
// physics::lead_aim) and DIRECT (fire at where the target IS right now)
// on a per-Sid 4-second cycle, so the squad alternates between snipers
// and snap-shooters and the player can't memorize one behavior.
//
// All positions consumed by the brain go through `ego_pos` so the brain
// always sees the world from its own viewpoint — under torus wrap, a target
// "across the seam" appears at the nearest wrapped copy. Same shape we'll
// hand to client-side bots later: the bot doesn't need to know about wrap,
// it just sees offsets relative to itself.

use shared::constants::{BULLET_LIFETIME_SECONDS, BULLET_SPEED, SHOT_CHARGE_MAX};
use shared::entities::{forward, Bullet, Ship};
use shared::map::WallSegment;
use shared::math::{self, Vec2};
use shared::physics;
use shared::protocol::TickInput;
use shared::world::World;

/// Sid's normal stand-off range. Most of the time he'll hover here and
/// snipe; periodically he switches to `CHARGE_DISTANCE` for close-quarters
/// pressure (see `preferred_distance`).
const NORMAL_DISTANCE: f32 = 350.0;
/// During the aggro cycle Sid closes to this range — lots of dodging,
/// missed shots, but it stops the player from kiting forever.
const CHARGE_DISTANCE: f32 = 120.0;
/// Aim must be within this many radians of the target before Sid pulls the
/// trigger. ~6° matches the Elm version's `0.1`.
const AIM_TOLERANCE_RAD: f32 = 0.1;
/// Don't fire below this fraction of full charge — keeps Sid from spamming
/// his energy bar dry and ending up unable to react.
const FIRE_CHARGE_FRACTION: f32 = 1.0 / 3.0;
/// Bullets with `bullet_danger` above this swap Sid into evade mode.
/// Range of `bullet_danger` is [0, 2]: ~0.5 at the edge of `NEAR_MISS_DIST`
/// arriving at the lookahead horizon, ~2 for a bullet about to hit dead-on.
const EVADE_DANGER_THRESHOLD: f32 = 1.0;
/// How far ahead in time Sid considers bullet trajectories. Bullets that
/// won't even reach their closest pass for longer than this aren't worth
/// reacting to — gives him room to keep aiming/shooting instead of jerking
/// at every distant projectile.
const BULLET_DANGER_LOOKAHEAD_SECONDS: f32 = 1.2;
/// How close (units) a bullet's closest-approach distance has to be to
/// register as dangerous. Ship triangle is ~24 units vertex-to-vertex; a
/// bit of slop above that catches grazing shots and the ship's velocity
/// uncertainty. Bullets that miss by more than this never trigger evade.
const BULLET_NEAR_MISS_DIST: f32 = 28.0;
/// Soft top speed. Sid won't thrust toward the target if already over this —
/// keeps him from pancaking into walls / overshooting. Velocity-correction
/// thrust ignores this cap because braking IS the goal there.
const MAX_CRUISE_SPEED: f32 = 180.0;
/// How long Sid stays in one aim mode (lead vs direct) before flipping.
/// 4s @ 60Hz — short enough that the player notices the variety, long
/// enough that aim doesn't oscillate mid-engagement.
const AIM_MODE_PERIOD_TICKS: u32 = 240;
/// Cadence of the charge cycle — every CHARGE_PERIOD_TICKS Sid re-rolls
/// his desired range. Offset per-Sid by entity id so the squad isn't
/// synchronized.
const CHARGE_PERIOD_TICKS: u32 = 600; // 10s
/// 1-in-N cycles spent charging vs sniping. 3 → ~33% aggro time.
const CHARGE_DUTY: u32 = 3;
/// Speed Sid wants to be moving toward (or away from) the target while
/// closing the gap. Below cruise so he doesn't overshoot his preferred range.
const APPROACH_SPEED: f32 = 110.0;
/// Distance band around the preferred range where Sid is "close enough" —
/// inside this band he wants velocity ≈ 0 (hold position and shoot).
const RANGE_BAND: f32 = 30.0;
/// Velocity error magnitude (units/s) above which Sid stops shooting and
/// turns to fix his velocity. Below it he tolerates the drift and engages.
/// Set high enough that small course wobble doesn't constantly interrupt
/// shooting; low enough that post-evade fly-away gets caught.
const VEL_CORRECTION_THRESHOLD: f32 = 80.0;
/// Below this speed Sid is assumed to be wall-pinned and falls out of
/// evade into attack mode — better to fight back than mash thrust into a
/// surface.
const STUCK_SPEED_THRESHOLD: f32 = 25.0;
/// Aim tolerance for the opportunistic fire-while-dodging check. Looser
/// than the attack-mode tolerance because Sid is mid-turn — if his nose
/// briefly sweeps past the target during evasion, take the shot.
const OPPORTUNISTIC_AIM_RAD: f32 = AIM_TOLERANCE_RAD * 2.0;
/// How far Sid probes when searching for a clear path around a wall.
/// Doesn't need to reach the target — far enough to commit to a side and
/// "fly past" the obstacle. Big maps with long winding walls won't fully
/// solve, which is fine; he'll re-probe each tick as he moves.
const NAV_PROBE_DIST: f32 = 200.0;
/// Fan offsets (degrees) tried in order when the direct line to target is
/// blocked. The smallest clear angle wins. Goes wide enough to handle
/// roughly L-shaped obstacles without trying to solve mazes.
const NAV_FAN_OFFSETS_DEG: &[f32] = &[15.0, 30.0, 50.0, 75.0, 105.0, 140.0];
/// Cruising speed Sid commits to when navigating around a wall. Higher
/// than `APPROACH_SPEED` because once he's behind cover he wants to
/// COMMIT to clearing the obstacle, not nibble at it. Above `MAX_CRUISE_SPEED`
/// since engagement will brake him back down once the line opens.
const NAV_SPEED: f32 = 200.0;

/// Per-tick brain. Pure function of (world, our ship) → input.
pub fn sid_tick(world: &World, ship: &Ship) -> TickInput {
    if let Some((danger, bullet)) = most_dangerous_bullet(ship, world) {
        if danger > EVADE_DANGER_THRESHOLD {
            return evade(ship, bullet, world);
        }
    }
    attack_nearest(ship, world)
}

/// Snap `target_pos` to the wrapped instance closest to `ship.pos` so the
/// brain reads the world from its own viewpoint. No-op for non-wrap maps.
fn ego_pos(ship: &Ship, target_pos: Vec2, world: &World) -> Vec2 {
    if !world.map.edge_wrap {
        return target_pos;
    }
    let w = world.map.width;
    let h = world.map.height;
    let mut p = target_pos;
    let dx = p.x - ship.pos.x;
    if dx > w * 0.5 {
        p.x -= w;
    } else if dx < -w * 0.5 {
        p.x += w;
    }
    let dy = p.y - ship.pos.y;
    if dy > h * 0.5 {
        p.y -= h;
    } else if dy < -h * 0.5 {
        p.y += h;
    }
    p
}

fn most_dangerous_bullet<'a>(ship: &Ship, world: &'a World) -> Option<(f32, &'a Bullet)> {
    let mut best: Option<(f32, &Bullet)> = None;
    for b in world.bullets.values() {
        if b.shooter == ship.entity_id {
            continue;
        }
        let bp = ego_pos(ship, b.pos, world);
        let d = bullet_danger(ship.pos, ship.vel, bp, b.vel, b.age_seconds);
        if d <= 0.0 {
            continue;
        }
        match best {
            None => best = Some((d, b)),
            Some((bd, _)) if d > bd => best = Some((d, b)),
            _ => {}
        }
    }
    best
}

/// Closest-approach predictor. Treats both ship and bullet as points
/// moving in straight lines at their current velocities and computes the
/// minimum future distance between them. Returns 0 if the bullet won't
/// pass within `BULLET_NEAR_MISS_DIST`, won't reach its closest point
/// within the lookahead horizon, will already have aged out before then,
/// or has already passed us. Higher score = closer + sooner.
///
/// Math: relative position P = bpos - spos, relative velocity V = bvel -
/// svel. Distance² at time t = |P + V·t|². Derivative = 0 → t* = -P·V / V·V.
/// At t* the distance is |P + V·t*|.
fn bullet_danger(spos: Vec2, svel: Vec2, bpos: Vec2, bvel: Vec2, bullet_age: f32) -> f32 {
    let p = bpos - spos;
    let v = bvel - svel;
    let v_sq = v.dot(v);
    if v_sq < 1e-6 {
        return 0.0; // bullet stationary in our frame — no closing
    }
    let t_close = -p.dot(v) / v_sq;
    if t_close <= 0.0 {
        return 0.0; // closest pass is in the past — bullet's already moving away
    }
    // Lookahead is min(constant horizon, bullet's remaining lifetime).
    // A bullet that'll vanish before reaching us isn't a threat.
    let remaining_life = (BULLET_LIFETIME_SECONDS - bullet_age).max(0.0);
    let lookahead = BULLET_DANGER_LOOKAHEAD_SECONDS.min(remaining_life);
    if lookahead <= 0.0 || t_close > lookahead {
        return 0.0;
    }
    let closest = p + v * t_close;
    let dist = closest.length();
    if dist > BULLET_NEAR_MISS_DIST {
        return 0.0;
    }
    // 0 at the edge of the near-miss radius, 1 dead-on. Same shape for
    // time: 0 at the lookahead horizon, 1 about to hit. Sum gives [0, 2].
    let dist_score = 1.0 - (dist / BULLET_NEAR_MISS_DIST);
    let time_score = 1.0 - (t_close / lookahead);
    dist_score + time_score
}

fn nearest_target<'a>(ship: &Ship, world: &'a World) -> Option<&'a Ship> {
    world
        .ships
        .values()
        .filter(|s| s.entity_id != ship.entity_id)
        .min_by(|a, b| {
            let da = (ego_pos(ship, a.pos, world) - ship.pos).length_squared();
            let db = (ego_pos(ship, b.pos, world) - ship.pos).length_squared();
            da.partial_cmp(&db).unwrap_or(core::cmp::Ordering::Equal)
        })
}

fn attack_nearest(ship: &Ship, world: &World) -> TickInput {
    let Some(target) = nearest_target(ship, world) else {
        // Empty arena — slow spin so Sid doesn't look frozen on screen.
        return TickInput {
            client_tick: 0,
            turn_left: false,
            turn_right: true,
            thrust: false,
            fire: false,
        };
    };

    let target_pos = ego_pos(ship, target.pos, world);
    let dx = target_pos.x - ship.pos.x;
    let dy = target_pos.y - ship.pos.y;
    let dist = (dx * dx + dy * dy).sqrt();

    // Desired range cycles between NORMAL and CHARGE so Sid mixes sniping
    // and rushing instead of just hovering at one stand-off.
    let charge_cycle = (world.tick / CHARGE_PERIOD_TICKS).wrapping_add(ship.entity_id);
    let desired_dist = if charge_cycle % CHARGE_DUTY == 0 {
        CHARGE_DISTANCE
    } else {
        NORMAL_DISTANCE
    };

    // LOS check first — if a wall is in the way, drop out of engagement
    // entirely and commit to flying around it. The full attack loop
    // (range hover, charge cycle, lead-aim) only makes sense when the
    // shot can actually land.
    let (nav_target, los_clear) = navigable_target(ship, target_pos, world);
    if !los_clear {
        return navigate_to(ship, nav_target);
    }
    let want_vel = want_velocity(ship.pos, nav_target, dist, desired_dist);
    let vel_err = want_vel - ship.vel;
    let vel_err_mag = vel_err.length();
    let need_vel_correction = vel_err_mag > VEL_CORRECTION_THRESHOLD;

    // Aim direction:
    //   - velocity correction → face the correction vector and burn
    //   - otherwise alternate lead-shot vs direct aim every few seconds
    let aim_dir = if need_vel_correction {
        aim_angle(vel_err.x, vel_err.y)
    } else {
        let cycle = (world.tick / AIM_MODE_PERIOD_TICKS).wrapping_add(ship.entity_id);
        let use_lead = cycle % 2 == 0;
        if use_lead {
            match physics::lead_aim(
                ship.pos,
                ship.vel,
                target_pos,
                target.vel,
                BULLET_SPEED,
            ) {
                Some((dir, _)) => aim_angle(dir.x, dir.y),
                None => aim_angle(dx, dy),
            }
        } else {
            aim_angle(dx, dy)
        }
    };
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aim_error = diff.abs();
    let aimed_close = aim_error < AIM_TOLERANCE_RAD * 3.0;

    let thrust = if need_vel_correction {
        // Burn through the speed cap — cap is for "don't snowball away
        // from the target", but here we ARE trying to fix the snowball.
        aimed_close
    } else {
        let want_close_distance = dist > desired_dist;
        let under_speed_cap =
            ship.vel.length_squared() < MAX_CRUISE_SPEED * MAX_CRUISE_SPEED;
        want_close_distance && under_speed_cap && aimed_close
    };

    // Only fire when actually aimed at the target, not when course-correcting.
    // (LOS is already known clear here — blocked path early-returns above.)
    let fire_ready = !need_vel_correction
        && aim_error < AIM_TOLERANCE_RAD
        && ship.shot_charge > SHOT_CHARGE_MAX * FIRE_CHARGE_FRACTION;

    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust,
        fire: fire_ready,
    }
}

/// Pure-navigation tick — used when the line of sight to the target is
/// blocked. Sid commits to flying toward `dest` at `NAV_SPEED`, computing
/// the correction vector from current velocity so he naturally brakes
/// when he's drifting the wrong way and accelerates when he's not. He
/// doesn't fire because there's no target in his sights, and the entire
/// engagement loop (range hover, lead-aim, charge cycle) is bypassed
/// until the path opens up.
fn navigate_to(ship: &Ship, dest: Vec2) -> TickInput {
    let to = dest - ship.pos;
    let to_dist = to.length();
    if to_dist < 1e-3 {
        return TickInput::default();
    }
    let want_vel = to * (NAV_SPEED / to_dist);
    let vel_err = want_vel - ship.vel;
    let aim_dir = aim_angle(vel_err.x, vel_err.y);
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aim_error = diff.abs();
    let aimed_close = aim_error < AIM_TOLERANCE_RAD * 3.0;
    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: aimed_close,
        fire: false,
    }
}

/// Velocity Sid wants to have right now, given his desired stand-off.
/// `head_toward` is the direction to steer (which may be a wall-skirting
/// waypoint, not the target itself). `range_to_target` is the actual
/// distance to the target — it controls the in-band hover behaviour.
/// Outside the band: move toward (or away from) the target at APPROACH_SPEED.
/// Inside the band: hold (zero) so he can plant his nose on the target.
fn want_velocity(
    ship_pos: Vec2,
    head_toward: Vec2,
    range_to_target: f32,
    desired_dist: f32,
) -> Vec2 {
    let to = head_toward - ship_pos;
    let to_dist = to.length();
    if to_dist < 1e-3 {
        return Vec2::ZERO;
    }
    let dir = to * (1.0 / to_dist);
    if range_to_target > desired_dist + RANGE_BAND {
        dir * APPROACH_SPEED
    } else if range_to_target < desired_dist - RANGE_BAND {
        dir * -APPROACH_SPEED
    } else {
        Vec2::ZERO
    }
}

/// True if the segment `from`→`to` doesn't intersect any wall. Bbox-rejects
/// each wall before the full segment-vs-segment test so big maps stay
/// cheap (newdarkhell has ~1500 walls — without the reject, this would
/// dominate the bot tick).
fn line_of_sight_clear(from: Vec2, to: Vec2, walls: &[WallSegment]) -> bool {
    let (rmin_x, rmax_x) = (from.x.min(to.x), from.x.max(to.x));
    let (rmin_y, rmax_y) = (from.y.min(to.y), from.y.max(to.y));
    for wall in walls {
        let (wmin_x, wmax_x) = (wall.a.x.min(wall.b.x), wall.a.x.max(wall.b.x));
        let (wmin_y, wmax_y) = (wall.a.y.min(wall.b.y), wall.a.y.max(wall.b.y));
        if wmax_x < rmin_x || wmin_x > rmax_x || wmax_y < rmin_y || wmin_y > rmax_y {
            continue;
        }
        if physics::segments_intersect(from, to, wall.a, wall.b) {
            return false;
        }
    }
    true
}

/// Wrap-aware LOS. On torus maps the ego-shifted segment may exit the
/// real world bounds; we test 9 shifted copies of the segment (offsets
/// (kW, jH) for k,j ∈ {-1,0,1}) so walls on the OTHER side of the seam
/// still get checked. Most shifts have no overlap with the world bbox
/// and skip cheaply; per-wall bbox rejection inside `line_of_sight_clear`
/// keeps the cost down on the shifts that DO overlap.
fn line_of_sight_clear_world(from: Vec2, to: Vec2, world: &World) -> bool {
    let walls = &world.map.walls;
    if !world.map.edge_wrap {
        return line_of_sight_clear(from, to, walls);
    }
    let w = world.map.width;
    let h = world.map.height;
    for kx in -1..=1_i32 {
        for ky in -1..=1_i32 {
            let off = Vec2::new(kx as f32 * w, ky as f32 * h);
            let f = from + off;
            let t = to + off;
            let (smin_x, smax_x) = (f.x.min(t.x), f.x.max(t.x));
            let (smin_y, smax_y) = (f.y.min(t.y), f.y.max(t.y));
            // Skip shifts whose bbox doesn't overlap the world bounds —
            // those copies can't possibly hit any real wall.
            if smax_x < 0.0 || smin_x > w || smax_y < 0.0 || smin_y > h {
                continue;
            }
            if !line_of_sight_clear(f, t, walls) {
                return false;
            }
        }
    }
    true
}

/// Where Sid should STEER to engage the target, plus whether the direct
/// line of sight is clear. Either the target itself (LOS clear) or a fan-out
/// probe point that skirts the obstacle. Even-id Sids try the left side
/// first, odd-id Sids try right first, so a squad doesn't all crowd around
/// the same side of an obstacle.
fn navigable_target(ship: &Ship, target_pos: Vec2, world: &World) -> (Vec2, bool) {
    if line_of_sight_clear_world(ship.pos, target_pos, world) {
        return (target_pos, true);
    }
    let direct = target_pos - ship.pos;
    let dist = direct.length();
    if dist < 1e-3 {
        return (target_pos, false);
    }
    let base_angle = aim_angle(direct.x, direct.y);
    let probe_dist = dist.min(NAV_PROBE_DIST);
    let try_left_first = ship.entity_id % 2 == 0;
    let signs: [f32; 2] = if try_left_first {
        [-1.0, 1.0]
    } else {
        [1.0, -1.0]
    };
    for &deg in NAV_FAN_OFFSETS_DEG {
        let off = deg * core::f32::consts::PI / 180.0;
        for sign in signs {
            let a = base_angle + sign * off;
            let probe_end = ship.pos + forward(a) * probe_dist;
            if line_of_sight_clear_world(ship.pos, probe_end, world) {
                return (probe_end, false);
            }
        }
    }
    // No clear angle — head toward the target directly. Velocity correction
    // and the ship-vs-wall physics will sort it out.
    (target_pos, false)
}

/// Reframe the world so Sid is moving along +y, then read the bullet's x
/// at the moment it crosses our altitude. Bullet to the right → veer left;
/// bullet to the left → veer right. Hard turn plus thrust to actually move
/// out of the way. Fires opportunistically if Sid's nose happens to sweep
/// past the nearest target mid-evade.
fn evade(ship: &Ship, bullet: &Bullet, world: &World) -> TickInput {
    let speed = ship.vel.length();
    // Wall-pinned (or never moved): thrusting just mashes against the
    // surface. Drop out of evade into attack — at least we'll be shooting
    // and the velocity-correction loop in attack might pry us free.
    if speed < STUCK_SPEED_THRESHOLD {
        return attack_nearest(ship, world);
    }
    let vel_angle = aim_angle(ship.vel.x, ship.vel.y);
    let cos_a = math::cos(-vel_angle);
    let sin_a = math::sin(-vel_angle);
    let rel_p = ego_pos(ship, bullet.pos, world) - ship.pos;
    let rel_v = bullet.vel - ship.vel;
    let bpos_x = rel_p.x * cos_a - rel_p.y * sin_a;
    let bpos_y = rel_p.x * sin_a + rel_p.y * cos_a;
    let bvel_x = rel_v.x * cos_a - rel_v.y * sin_a;
    let bvel_y = rel_v.x * sin_a + rel_v.y * cos_a;

    let bx_at_intercept = if bvel_y.abs() < 1e-6 {
        bpos_x
    } else {
        let t = bpos_y / bvel_y;
        bpos_x + bvel_x * t
    };

    // Opportunistic fire — if we happen to be currently aiming at the
    // nearest enemy (mid-spin or after the dodge swung us toward them),
    // squeeze the trigger. Doesn't change the dodge direction; just adds
    // a free shot when the geometry lines up.
    let opportunistic_fire = match nearest_target(ship, world) {
        Some(target) => {
            let target_pos = ego_pos(ship, target.pos, world);
            let dx = target_pos.x - ship.pos.x;
            let dy = target_pos.y - ship.pos.y;
            let aim_dir = aim_angle(dx, dy);
            let aim_err = shortest_angle_diff(aim_dir, ship.angle).abs();
            aim_err < OPPORTUNISTIC_AIM_RAD
                && ship.shot_charge > SHOT_CHARGE_MAX * FIRE_CHARGE_FRACTION
        }
        None => false,
    };

    TickInput {
        client_tick: 0,
        // Bullet to the right → turn left (decrease angle).
        turn_left: bx_at_intercept > 0.0,
        turn_right: bx_at_intercept <= 0.0,
        thrust: true,
        fire: opportunistic_fire,
    }
}

/// Angle (radians) such that `forward(angle)` points along (dx, dy).
/// Matches our `forward(a) = (sin a, -cos a)` convention.
fn aim_angle(dx: f32, dy: f32) -> f32 {
    math::atan2(dx, -dy)
}

fn shortest_angle_diff(target: f32, current: f32) -> f32 {
    use core::f32::consts::{PI, TAU};
    let mut d = (target - current) % TAU;
    if d > PI {
        d -= TAU;
    } else if d < -PI {
        d += TAU;
    }
    d
}
