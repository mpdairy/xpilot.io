// Server-side AI ships. Port of "sid" from the original Elm xpilot — every
// tick the brain either dodges the most-dangerous incoming bullet or attacks
// the nearest other ship. No prediction, no pathfinding, no leading; just
// aim at where the target IS right now.
//
// All positions consumed by the brain go through `ego_pos` so the brain
// always sees the world from its own viewpoint — under torus wrap, a target
// "across the seam" appears at the nearest wrapped copy. Same shape we'll
// hand to client-side bots later: the bot doesn't need to know about wrap,
// it just sees offsets relative to itself.

use shared::constants::SHOT_CHARGE_MAX;
use shared::entities::{Bullet, Ship};
use shared::math::{self, Vec2};
use shared::protocol::TickInput;
use shared::world::World;

/// How close Sid wants to stay to his target. Beyond this he'll thrust to
/// close the gap; inside it he holds position and keeps shooting.
const PREFERRED_DISTANCE: f32 = 350.0;
/// Aim must be within this many radians of the target before Sid pulls the
/// trigger. ~6° matches the Elm version's `0.1`.
const AIM_TOLERANCE_RAD: f32 = 0.1;
/// Don't fire below this fraction of full charge — keeps Sid from spamming
/// his energy bar dry and ending up unable to react.
const FIRE_CHARGE_FRACTION: f32 = 1.0 / 3.0;
/// Bullets with `bullet_danger` above this swap Sid into evade mode.
const EVADE_DANGER_THRESHOLD: f32 = 1.0;
/// Soft top speed. Sid won't thrust if he's already moving faster than this —
/// keeps him from pancaking into walls / overshooting his target.
const MAX_CRUISE_SPEED: f32 = 180.0;

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
        let d = bullet_danger(ship.pos, ship.vel, bp, b.vel);
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

/// Heuristic from the Elm version. Compute when the bullet's x and y match
/// the ship's, in the ship's frame; if both are in the future and roughly
/// equal, the bullet's line passes through us. Score = 1000 / |tx - ty|.
fn bullet_danger(spos: Vec2, svel: Vec2, bpos: Vec2, bvel: Vec2) -> f32 {
    let dvx = bvel.x - svel.x;
    let dvy = bvel.y - svel.y;
    if dvx.abs() < 1e-6 || dvy.abs() < 1e-6 {
        return 0.0;
    }
    let tx = -(bpos.x - spos.x) / dvx;
    let ty = -(bpos.y - spos.y) / dvy;
    if tx < 0.0 || ty < 0.0 {
        return 0.0;
    }
    1000.0 / (tx - ty).abs().max(0.001)
}

fn attack_nearest(ship: &Ship, world: &World) -> TickInput {
    let nearest = world
        .ships
        .values()
        .filter(|s| s.entity_id != ship.entity_id)
        .min_by(|a, b| {
            let da = (ego_pos(ship, a.pos, world) - ship.pos).length_squared();
            let db = (ego_pos(ship, b.pos, world) - ship.pos).length_squared();
            da.partial_cmp(&db).unwrap_or(core::cmp::Ordering::Equal)
        });

    let Some(target) = nearest else {
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
    let aim_dir = aim_angle(dx, dy);
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aim_error = diff.abs();

    let dist2 = dx * dx + dy * dy;
    let want_close_distance = dist2 > PREFERRED_DISTANCE * PREFERRED_DISTANCE;
    let under_speed_cap = ship.vel.length_squared() < MAX_CRUISE_SPEED * MAX_CRUISE_SPEED;

    let fire_ready = aim_error < AIM_TOLERANCE_RAD
        && ship.shot_charge > SHOT_CHARGE_MAX * FIRE_CHARGE_FRACTION;

    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        // Only thrust when already aimed (thrusting mid-turn swings the ship
        // sideways and ruins the shot) AND below the soft speed cap.
        thrust: want_close_distance
            && under_speed_cap
            && aim_error < AIM_TOLERANCE_RAD * 3.0,
        fire: fire_ready,
    }
}

/// Reframe the world so Sid is moving along +y, then read the bullet's x
/// at the moment it crosses our altitude. Bullet to the right → veer left;
/// bullet to the left → veer right. Hard turn (both flags? no — one) plus
/// thrust to actually move out of the way.
fn evade(ship: &Ship, bullet: &Bullet, world: &World) -> TickInput {
    let speed = ship.vel.length();
    if speed < 1e-3 {
        // No velocity to define a frame — break symmetry with a hard turn.
        return TickInput {
            client_tick: 0,
            turn_left: false,
            turn_right: true,
            thrust: true,
            fire: false,
        };
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

    TickInput {
        client_tick: 0,
        // Bullet to the right → turn left (decrease angle).
        turn_left: bx_at_intercept > 0.0,
        turn_right: bx_at_intercept <= 0.0,
        thrust: true,
        fire: false,
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
