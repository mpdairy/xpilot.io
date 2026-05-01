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

use shared::constants::{
    BULLET_LIFETIME_SECONDS, BULLET_SPEED, SHIP_RADIUS, SHOT_CHARGE_COST, SHOT_CHARGE_MAX,
};
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

/// Per-bot scratch space carried by the room across ticks. Lives on
/// `Player::bot_state`; default-initialised for humans (never read for them).
/// Add new fields here for any future stateful behaviour — keep it small,
/// it's serialised into the per-room sim hot path.
#[derive(Default, Clone)]
pub struct BotState {
    /// Server tick at which Wimpy's current evade run ends. `None` means
    /// "not currently evading". Set when bullet danger trips the threshold;
    /// cleared when the deadline passes.
    pub evade_until_tick: Option<u32>,
    /// Reaper's charge-cycle phase + a per-charge-run perpendicular offset
    /// side. Only meaningful for the Reaper personality.
    pub reaper: ReaperState,
}

#[derive(Default, Clone)]
pub struct ReaperState {
    pub phase: ReaperPhase,
    /// +1.0 or -1.0. Picked at the start of each Charge phase so the
    /// approach angle alternates instead of always coming in from the
    /// same side.
    pub offset_side: f32,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum ReaperPhase {
    /// Building speed toward the target with a perpendicular offset so
    /// the run isn't a straight charge. Doesn't fire — saving up bullets.
    #[default]
    Charge,
    /// Within engagement range, dumping the magazine using lead-aim.
    /// Doesn't thrust — coasting in from the charge.
    Burst,
    /// Charge depleted. Backing off to recharge before the next pass.
    Retreat,
}

/// Dispatch to a personality's per-tick brain by display name. Names not in
/// the table fall back to Sid (the default aggressive brain). Keeps the
/// per-bot personality decision in one place so room.rs only needs the name.
pub fn tick_for(name: &str, world: &World, ship: &Ship, state: &mut BotState) -> TickInput {
    match name {
        "Reaper" => reaper_tick(world, ship, state),
        "Cobra" => cobra_tick(world, ship),
        "Vega" => vega_tick(world, ship),
        "Wimpy" => wimpy_tick(world, ship, state),
        _ => sid_tick(world, ship),
    }
}

/// All bot display names. The first five map to specific personalities;
/// the rest fall through to `sid_tick` via `tick_for`. Sampled WITHOUT
/// replacement to fill a room — so a 4-bot room is 4 distinct names and
/// the natural Sid bias comes from the three Sid-fallback names in the
/// pool (Slugger / Spike / Diesel).
pub const BOT_NAMES: [&str; 8] = [
    "Sid", "Reaper", "Cobra", "Vega", "Wimpy", "Slugger", "Spike", "Diesel",
];

/// Pick `count` bot display names by random sampling WITHOUT replacement
/// from `BOT_NAMES` (Fisher-Yates partial shuffle). Seeded from system
/// time so every room composition is genuinely random — this isn't in
/// the deterministic sim path.
pub fn pick_bot_names(count: usize) -> Vec<String> {
    let mut state = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0xCAFEBABE_DEADBEEF);
    // splitmix64 — small, no deps, good enough for picking 1–8 indices.
    let mut next = || {
        state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = state;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    };

    let mut pool: Vec<&str> = BOT_NAMES.to_vec();
    let take = count.min(pool.len());
    // Fisher-Yates: at step i, swap pool[i] with a random pool[i..end].
    for i in 0..take {
        let remaining = pool.len() - i;
        let j = i + (next() as usize % remaining);
        pool.swap(i, j);
    }
    pool.into_iter().take(take).map(|s| s.to_string()).collect()
}

// ── Wimpy ──────────────────────────────────────────────────────────────────
// Plays like Sid most of the time. When a bullet trips the danger threshold
// he commits to a 3–5 s FLEE: continuously re-evaluates "directly opposite
// the player" each tick (so a chasing player makes him keep curving away,
// not pick once and fly straight), navigates with a swept-corridor wall
// lookahead to actually avoid walls, and once he's at top speed he turns
// AROUND and sprays shots back at the threat instead of just coasting.

/// Bullet danger score above which Wimpy enters flee mode. 0.3 vs Sid's
/// 1.0 — he reacts to bullets that Sid would happily ignore. Range of
/// `bullet_danger` is [0, 2] (see `bullet_danger` doc).
const WIMPY_FLEE_DANGER_THRESHOLD: f32 = 0.3;
/// Minimum flee duration in ticks (3 s @ 60 Hz). Long enough that he
/// commits — short enough that he doesn't disengage forever after a stray
/// shot.
const WIMPY_FLEE_MIN_TICKS: u32 = 180;
/// Random window above MIN — total flee is in [3 s, 5 s].
const WIMPY_FLEE_RANGE_TICKS: u32 = 120;
/// How far from current position the flee target is projected each tick
/// (along "opposite of shooter NOW"). Big enough that the target is well
/// across the map for any reasonable arena size; recomputed every tick so
/// a moving player makes Wimpy's aim curve naturally.
const WIMPY_FLEE_TARGET_DIST: f32 = 1500.0;
/// Soft top speed (units/sec) along the flee aim direction. Above this we
/// stop thrusting AND switch to spray-fire mode. 320 is faster than Sid's
/// MAX_CRUISE_SPEED of 180 → he gets away noticeably quicker than the
/// regular bots.
const WIMPY_FLEE_TOP_SPEED: f32 = 320.0;
/// Time horizon (seconds) for the wall-collision lookahead. Sized so the
/// detection fires while there's still room to actually REDIRECT velocity
/// (not just turn the nose). At top speed 320 with thrust accel 200, full
/// brake takes ~1.6 s; redirecting via velocity correction at 90° clears
/// the wall in ~1.0 s of thrust. 1.2 s of lookahead at 320 u/s is 384 u
/// of distance — plenty of margin to swing velocity sideways before
/// hitting anything.
const WIMPY_FLEE_REACT_TIME_S: f32 = 1.2;
/// Floor on the swept-path lookahead distance — so even at low speed
/// Wimpy still notices a wall right in front of him.
const WIMPY_FLEE_LOOKAHEAD_MIN: f32 = 80.0;
/// Speed (units/sec) below which wall-lookahead doesn't bother running.
/// Slow ships have plenty of time to react via the regular nav path.
const WIMPY_FLEE_LOOKAHEAD_MIN_SPEED: f32 = 25.0;
/// Half-width of the swept-corridor used for wall lookahead. Adds a
/// safety margin to the ship radius so a wall that grazes the side of
/// the trajectory still counts as a hit.
const WIMPY_FLEE_CORRIDOR_HALF_WIDTH: f32 = SHIP_RADIUS + 10.0;
/// Probe angles (degrees, ±) tried when the projected velocity hits a
/// wall. Closest clear angle wins. Goes wide enough to handle dead-ends.
const WIMPY_FLEE_DIVERT_DEG: &[f32] = &[
    45.0, -45.0, 70.0, -70.0, 100.0, -100.0, 130.0, -130.0, 160.0, -160.0, 180.0,
];
/// Aim tolerance for the spray-fire mode. Tighter than the flee-aim band
/// because we're trying to actually HIT, not just point that way.
const WIMPY_SPRAY_AIM_RAD: f32 = AIM_TOLERANCE_RAD * 2.0;

pub fn wimpy_tick(world: &World, ship: &Ship, state: &mut BotState) -> TickInput {
    let target = nearest_target(ship, world);

    // (Re)trigger flee if a dangerous bullet appeared this tick. Refreshing
    // the timer mid-flee means a sustained burst keeps him running without
    // accidentally shortening the deadline.
    let new_danger = match most_dangerous_bullet(ship, world) {
        Some((d, _)) => d > WIMPY_FLEE_DANGER_THRESHOLD,
        None => false,
    };
    if new_danger && target.is_some() {
        let dur = WIMPY_FLEE_MIN_TICKS + flee_jitter(world.tick, ship.entity_id);
        let new_until = world.tick.wrapping_add(dur);
        state.evade_until_tick = Some(match state.evade_until_tick {
            Some(prev) if (prev as i64) > (new_until as i64) => prev,
            _ => new_until,
        });
    }

    // Are we currently fleeing?
    if let Some(until) = state.evade_until_tick {
        if (world.tick as i64) < (until as i64) {
            if let Some(t) = target {
                return flee_path(ship, t, world);
            }
            // No target to flee from this tick — fall through to sid_tick,
            // which will slow-spin in an empty arena.
        } else {
            state.evade_until_tick = None;
        }
    }

    sid_tick(world, ship)
}

/// Pseudo-random flee jitter in ticks, so a squad of wimpys triggered at
/// the same instant don't all return to Sid at the exact same moment.
/// Doesn't need cross-platform determinism (server-only), and the bot AI
/// is excluded from the determinism contract anyway.
fn flee_jitter(world_tick: u32, entity_id: u32) -> u32 {
    let h = world_tick
        .wrapping_mul(2_654_435_761)
        .wrapping_add(entity_id.wrapping_mul(40_503));
    h % WIMPY_FLEE_RANGE_TICKS
}

/// One flee tick. Three layers of decisions, in priority order:
///   1. Wall-imminent override → divert away from the wall, thrust hard.
///      Beats everything else: dying on a wall ends the flee badly.
///   2. Below top speed → keep building speed in the "directly away from
///      player NOW" direction. Recomputed each tick so a chasing player
///      curves Wimpy's path naturally instead of him picking a heading at
///      entry and locking it in.
///   3. At top speed → turn back toward the threat and SPRAY. Coasting
///      while turning, no thrust (we're already fast), fire when on aim.
fn flee_path(ship: &Ship, target: &Ship, world: &World) -> TickInput {
    let target_pos = ego_pos(ship, target.pos, world);

    // Layer 1: wall avoidance — velocity correction, not just steering.
    // Aiming at the divert direction and thrusting only ADDS force in that
    // direction; it doesn't subtract from the wall-bound momentum. So we
    // aim at (want_vel - current_vel) instead, which simultaneously brakes
    // the bad component and pushes toward the safe one. That's what stops
    // him from sliding into the wall sideways.
    if let Some(divert) = wall_divert_angle(ship, world) {
        return brake_into_divert(ship, divert);
    }

    // Layer 2/3: pick a flee direction that's continuously the "opposite
    // of player NOW", routed around walls via Sid's nav fan.
    let to_us = ship.pos - target_pos;
    let len = to_us.length();
    let away_unit = if len > 1e-3 {
        to_us * (1.0 / len)
    } else {
        Vec2::new(1.0, 0.0)
    };
    let dest = ship.pos + away_unit * WIMPY_FLEE_TARGET_DIST;
    let (nav_target, _) = navigable_target(ship, dest, world);
    let to_nav = nav_target - ship.pos;
    let flee_aim = if to_nav.length_squared() < 1e-6 {
        ship.angle
    } else {
        aim_angle(to_nav.x, to_nav.y)
    };

    // Speed along the flee direction — drives the build-vs-spray choice.
    let flee_unit = forward(flee_aim);
    let speed_along_flee = ship.vel.dot(flee_unit);

    if speed_along_flee < WIMPY_FLEE_TOP_SPEED {
        // Layer 2: build speed.
        let diff = shortest_angle_diff(flee_aim, ship.angle);
        let aimed_close = diff.abs() < AIM_TOLERANCE_RAD * 4.0;
        let nav = TickInput {
            client_tick: 0,
            turn_left: diff < 0.0,
            turn_right: diff > 0.0,
            thrust: aimed_close,
            fire: false,
        };
        return add_opportunistic_fire(nav, ship, world);
    }

    // Layer 3: at top speed → spray. Aim back at the target with lead, fire
    // when on aim and charged. No thrust — we're coasting in the flee
    // direction, the turn is "free" (doesn't slow us much) and the spray
    // is gravy on the escape.
    let spray_aim = match physics::lead_aim(
        ship.pos,
        ship.vel,
        target_pos,
        target.vel,
        BULLET_SPEED,
    ) {
        Some((dir, _)) => aim_angle(dir.x, dir.y),
        None => aim_angle(target_pos.x - ship.pos.x, target_pos.y - ship.pos.y),
    };
    let diff = shortest_angle_diff(spray_aim, ship.angle);
    let aim_err = diff.abs();
    let charged = ship.shot_charge > SHOT_CHARGE_MAX * FIRE_CHARGE_FRACTION;
    let los = line_of_sight_clear_world(ship.pos, target_pos, world);
    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: false,
        fire: aim_err < WIMPY_SPRAY_AIM_RAD && charged && los,
    }
}

/// Velocity-correcting brake into a divert direction. Computes the thrust
/// vector that would simultaneously brake the ship's wall-bound velocity
/// component AND start pushing it along `divert_angle`. Aims at the THRUST
/// direction (not the divert direction itself) — at high speed the two
/// can be 90°+ apart, and aiming at the divert direction alone leaves the
/// ship sliding sideways into the wall while it slowly redirects.
///
/// `want_vel` magnitude: keep the current speed but rotated to the divert
/// direction. Using current speed (rather than zero) means we redirect
/// instead of trying to halt — halting takes too long and the wall is
/// closer than the stopping distance.
fn brake_into_divert(ship: &Ship, divert_angle: f32) -> TickInput {
    let cand_unit = forward(divert_angle);
    let speed = ship.vel.length();
    let want_vel = cand_unit * speed;
    let vel_err = want_vel - ship.vel;
    let err_len = vel_err.length();
    let aim_dir = if err_len > 1e-3 {
        aim_angle(vel_err.x, vel_err.y)
    } else {
        divert_angle
    };
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aimed_close = diff.abs() < AIM_TOLERANCE_RAD * 4.0;
    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: aimed_close,
        fire: false,
    }
}

/// If the current velocity would carry Wimpy into a wall within the
/// lookahead horizon (using a swept corridor, not just a centerline ray,
/// to catch grazing collisions), return a divert angle to turn toward.
/// `None` means "current heading is clear, no divert needed".
fn wall_divert_angle(ship: &Ship, world: &World) -> Option<f32> {
    let speed = ship.vel.length();
    if speed < WIMPY_FLEE_LOOKAHEAD_MIN_SPEED {
        return None;
    }
    let lookahead = (speed * WIMPY_FLEE_REACT_TIME_S).max(WIMPY_FLEE_LOOKAHEAD_MIN);
    if swept_corridor_clear(ship.pos, ship.vel, lookahead, world) {
        return None;
    }
    let vel_angle = aim_angle(ship.vel.x, ship.vel.y);
    for &deg in WIMPY_FLEE_DIVERT_DEG {
        let off = deg * core::f32::consts::PI / 180.0;
        let cand_angle = vel_angle + off;
        let cand_unit = forward(cand_angle);
        // Use the same swept-corridor check on the candidate direction so we
        // don't divert into a wall that grazes the side of the new path.
        let probe_vel = cand_unit * speed.max(WIMPY_FLEE_LOOKAHEAD_MIN_SPEED);
        if swept_corridor_clear(ship.pos, probe_vel, lookahead, world) {
            return Some(cand_angle);
        }
    }
    // Truly surrounded — let layer 2 try to route via the nav fan.
    None
}

/// Three parallel rays (center + ±half-width perpendicular rails) projected
/// `dist` units along `vel`. All three must be clear for the corridor to
/// pass — catches walls that would clip the side of the ship even if the
/// centerline misses them. Returns `true` if no wall is in the way.
fn swept_corridor_clear(pos: Vec2, vel: Vec2, dist: f32, world: &World) -> bool {
    let speed = vel.length();
    if speed < 1e-3 {
        return true;
    }
    let dir = vel * (1.0 / speed);
    let perp = Vec2::new(-dir.y, dir.x);
    let off = perp * WIMPY_FLEE_CORRIDOR_HALF_WIDTH;
    let center_end = pos + dir * dist;
    let left = pos + off;
    let right = pos - off;
    let left_end = left + dir * dist;
    let right_end = right + dir * dist;
    line_of_sight_clear_world(pos, center_end, world)
        && line_of_sight_clear_world(left, left_end, world)
        && line_of_sight_clear_world(right, right_end, world)
}

// ── Reaper ─────────────────────────────────────────────────────────────────
// Charge-and-burst attacker. Cycles through three phases:
//
//   1. CHARGE: Bullets stored, target identified. Fly toward the target
//      with a perpendicular OFFSET so the run comes in on a slant rather
//      than dead-on (harder to read). Doesn't fire — saving the shots.
//   2. BURST: Inside engagement range. Stop thrusting, lead-aim, dump as
//      many bullets as the cooldown allows until the magazine is empty.
//   3. RETREAT: Empty. Fly directly away from the target to recharge,
//      then transition back to CHARGE at full mag.
//
// Reuses Wimpy's wall-avoidance machinery (`wall_divert_angle` +
// `brake_into_divert`) — Reaper hits high speed during the charge and
// would smear himself on walls without it.

/// Charge level (fraction of max) above which Reaper considers himself
/// "loaded" enough to start a charge run. 0.85 = ~4 shots stored.
const REAPER_LOADED_FRACTION: f32 = 0.85;
/// Charge level below which the magazine is "spent" — switch to retreat.
/// 0.20 = below the fire threshold so attempts to fire would silently
/// fail anyway.
const REAPER_SPENT_FRACTION: f32 = 0.20;
/// Distance at which CHARGE flips to BURST. Inside this range Reaper stops
/// thrusting and starts firing. Sized so the burst window is ~0.5–1 s
/// before he overshoots the target.
const REAPER_BURST_RANGE: f32 = 280.0;
/// Distance above which an in-progress BURST flips back to CHARGE — wider
/// than `REAPER_BURST_RANGE` to give the transition hysteresis. Without
/// this, a target that drifts out of range mid-burst leaves Reaper stuck
/// in Burst forever (no firing means no charge drain → Retreat never
/// triggers either).
const REAPER_BURST_EXIT_RANGE: f32 = 380.0;
/// Soft top speed during the charge run. Faster than Sid's MAX_CRUISE_SPEED
/// but slower than Wimpy's flee — Reaper is going TOWARD danger, full sprint
/// would just splat him on the far wall.
const REAPER_CHARGE_SPEED: f32 = 280.0;
/// Soft top speed during retreat. Slightly slower; he's recharging, not
/// running for his life.
const REAPER_RETREAT_SPEED: f32 = 240.0;
/// Distance perpendicular to the target line that Reaper aims for during
/// charge. Computed as `min(dist * 0.30, max)` so far-away approaches
/// have a noticeable angle, close approaches don't aim wildly past the
/// target.
const REAPER_OFFSET_FRACTION: f32 = 0.30;
const REAPER_OFFSET_MAX: f32 = 140.0;
const REAPER_OFFSET_MIN: f32 = 40.0;

pub fn reaper_tick(world: &World, ship: &Ship, state: &mut BotState) -> TickInput {
    let Some(target) = nearest_target(ship, world) else {
        return sid_tick(world, ship);
    };
    let target_pos = ego_pos(ship, target.pos, world);

    // Wall between us and target → defer to Sid's brain. Sid's `attack_nearest`
    // navigates around walls (the fan probe in `navigable_target`) and resumes
    // engagement when LOS opens; Reaper's charge/burst loop assumes a clear
    // line and would happily plough into the wall otherwise.
    if !line_of_sight_clear_world(ship.pos, target_pos, world) {
        return sid_tick(world, ship);
    }

    // Default offset_side is 0 → first-ever Charge would be a dead-on run.
    // Pick a random side once on first tick so even the spawn charge has
    // a slant to it.
    if state.reaper.offset_side == 0.0 {
        state.reaper.offset_side = reaper_random_side(ship.entity_id, world.tick);
    }

    let dist = (target_pos - ship.pos).length();
    let charge_frac = ship.shot_charge / SHOT_CHARGE_MAX;
    let loaded = charge_frac >= REAPER_LOADED_FRACTION;
    let spent = charge_frac <= REAPER_SPENT_FRACTION;
    let in_burst_range = dist < REAPER_BURST_RANGE;
    let out_of_burst_range = dist > REAPER_BURST_EXIT_RANGE;

    // Phase transitions. Hysteresis everywhere: loaded vs spent for the
    // charge axis, BURST_RANGE vs BURST_EXIT_RANGE for the distance axis.
    state.reaper.phase = match state.reaper.phase {
        ReaperPhase::Charge if in_burst_range && !spent => ReaperPhase::Burst,
        ReaperPhase::Charge if spent => ReaperPhase::Retreat,
        ReaperPhase::Burst if spent => ReaperPhase::Retreat,
        ReaperPhase::Burst if out_of_burst_range => {
            // Target slipped out of range — start a fresh charge with a new
            // offset side instead of sitting in Burst doing nothing.
            state.reaper.offset_side = reaper_random_side(ship.entity_id, world.tick);
            ReaperPhase::Charge
        }
        ReaperPhase::Retreat if loaded => {
            state.reaper.offset_side = reaper_random_side(ship.entity_id, world.tick);
            ReaperPhase::Charge
        }
        p => p,
    };

    match state.reaper.phase {
        ReaperPhase::Charge => {
            reaper_charge(ship, target, target_pos, state.reaper.offset_side, world)
        }
        ReaperPhase::Burst => reaper_burst(ship, target, target_pos, world),
        ReaperPhase::Retreat => reaper_retreat(ship, target_pos, world),
    }
}

fn reaper_random_side(entity_id: u32, tick: u32) -> f32 {
    let h = tick
        .wrapping_mul(2_654_435_761)
        .wrapping_add(entity_id.wrapping_mul(40_503));
    if h % 2 == 0 {
        1.0
    } else {
        -1.0
    }
}

/// CHARGE phase: lead-aim using `physics::lead_aim` with our charge speed
/// (NOT bullet speed) — that solves "where will the target be when I get
/// there at my flight speed", not "where to point a bullet". Then add a
/// perpendicular offset so the run comes in slanted, not dead-on. Wall
/// avoidance always wins via `brake_into_divert`.
fn reaper_charge(
    ship: &Ship,
    target: &Ship,
    target_pos: Vec2,
    side: f32,
    world: &World,
) -> TickInput {
    if let Some(divert) = wall_divert_angle(ship, world) {
        return brake_into_divert(ship, divert);
    }

    // Predict intercept assuming we travel at REAPER_CHARGE_SPEED. Falls
    // back to a direct line + linear time estimate if the quadratic has
    // no positive root (target faster than us moving straight away).
    let (intercept_dir, intercept_t) = match physics::lead_aim(
        ship.pos,
        ship.vel,
        target_pos,
        target.vel,
        REAPER_CHARGE_SPEED,
    ) {
        Some((d, t)) => (d, t),
        None => {
            let to_p = target_pos - ship.pos;
            let len = to_p.length();
            if len < 1e-3 {
                return TickInput::default();
            }
            (to_p * (1.0 / len), len / REAPER_CHARGE_SPEED)
        }
    };
    let intercept_pos = target_pos + target.vel * intercept_t;
    let to_int = intercept_pos - ship.pos;
    let int_dist = to_int.length();
    if int_dist < 1e-3 {
        return TickInput::default();
    }

    // Perpendicular offset relative to the intercept direction (not the
    // current direct-to-target line) — keeps the slant correct as the
    // target's predicted position drifts.
    let perp = Vec2::new(-intercept_dir.y, intercept_dir.x);
    let offset =
        (int_dist * REAPER_OFFSET_FRACTION).clamp(REAPER_OFFSET_MIN, REAPER_OFFSET_MAX);
    let aim_target = intercept_pos + perp * (side * offset);

    let (nav_target, _) = navigable_target(ship, aim_target, world);
    let to_nav = nav_target - ship.pos;
    let aim_dir = if to_nav.length_squared() < 1e-6 {
        ship.angle
    } else {
        aim_angle(to_nav.x, to_nav.y)
    };
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aimed_close = diff.abs() < AIM_TOLERANCE_RAD * 4.0;

    let aim_unit = forward(aim_dir);
    let speed_along = ship.vel.dot(aim_unit);
    let under_cap = speed_along < REAPER_CHARGE_SPEED;

    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: aimed_close && under_cap,
        fire: false,
    }
}

/// BURST phase: lead-aim at the target with `physics::lead_aim`, fire as
/// soon as we're on aim, charged, and have LOS. No thrust — we're coasting
/// in from the charge run; thrusting now would just overshoot harder.
/// Wall avoidance still runs because momentum from the charge is still
/// carrying us forward, possibly into something behind the target.
fn reaper_burst(ship: &Ship, target: &Ship, target_pos: Vec2, world: &World) -> TickInput {
    if let Some(divert) = wall_divert_angle(ship, world) {
        return brake_into_divert(ship, divert);
    }
    let aim_dir = match physics::lead_aim(
        ship.pos,
        ship.vel,
        target_pos,
        target.vel,
        BULLET_SPEED,
    ) {
        Some((dir, _)) => aim_angle(dir.x, dir.y),
        None => aim_angle(target_pos.x - ship.pos.x, target_pos.y - ship.pos.y),
    };
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aim_err = diff.abs();
    let aimed_tight = aim_err < AIM_TOLERANCE_RAD;
    let los = line_of_sight_clear_world(ship.pos, target_pos, world);
    let can_fire = ship.shot_charge >= SHOT_CHARGE_COST;
    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: false,
        fire: aimed_tight && can_fire && los,
    }
}

/// RETREAT phase: fly DIRECTLY away from the target to put distance between
/// the engagement and Reaper's recharge window. Wall avoidance + soft speed
/// cap. Doesn't shoot — we're empty by definition when we entered this
/// phase, and keeping the trigger off lets the charge bar refill instead
/// of dribbling into low-ammo shots.
fn reaper_retreat(ship: &Ship, target_pos: Vec2, world: &World) -> TickInput {
    if let Some(divert) = wall_divert_angle(ship, world) {
        return brake_into_divert(ship, divert);
    }
    let away = ship.pos - target_pos;
    let len = away.length();
    let away_unit = if len > 1e-3 {
        away * (1.0 / len)
    } else {
        Vec2::new(1.0, 0.0)
    };
    let dest = ship.pos + away_unit * 1500.0;
    let (nav_target, _) = navigable_target(ship, dest, world);
    let to_nav = nav_target - ship.pos;
    let aim_dir = if to_nav.length_squared() < 1e-6 {
        ship.angle
    } else {
        aim_angle(to_nav.x, to_nav.y)
    };
    let diff = shortest_angle_diff(aim_dir, ship.angle);
    let aimed_close = diff.abs() < AIM_TOLERANCE_RAD * 4.0;

    let aim_unit = forward(aim_dir);
    let speed_along = ship.vel.dot(aim_unit);
    let under_cap = speed_along < REAPER_RETREAT_SPEED;

    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: aimed_close && under_cap,
        fire: false,
    }
}

// ── Cobra ──────────────────────────────────────────────────────────────────
// S-curve weaver. Approaches the target with a sinusoidal lateral wobble —
// `offset_side` flips on a tick timer (not per-engagement), so the
// approach is one continuous serpentine instead of a sequence of straight
// runs. Fires opportunistically whenever the weave happens to swing the
// muzzle through the bullet-lead direction. Stateless — the weave phase
// is derived from `world.tick + entity_id`.
//
// Wall-blocked target → defers to Sid (same as Reaper) so the wall-aware
// path probe handles routing instead of Cobra straight-lining into walls.

/// Half-period of the weave in ticks. 45 ticks @ 60 Hz = 0.75 s on each
/// side → ~1.5 s for a full S. Faster than this looks twitchy; slower
/// loses the "weaving" feel and just looks like a slanted approach.
const COBRA_WEAVE_HALF_PERIOD_TICKS: u32 = 45;
/// Soft cap on flight speed along the weave aim direction.
const COBRA_FLIGHT_SPEED: f32 = 260.0;
/// Lateral offset as a fraction of distance to target — bigger weave when
/// far, tighter when close. Clamped at the boundaries.
const COBRA_OFFSET_FRACTION: f32 = 0.40;
const COBRA_OFFSET_MAX: f32 = 180.0;
const COBRA_OFFSET_MIN: f32 = 60.0;
/// Aim tolerance for the opportunistic fire check. Wider than attack mode
/// because the alignment is incidental (we're aimed at the weave point,
/// not the target) — the muzzle only sweeps through the lead direction
/// briefly each pass, so the window has to be generous enough to catch.
const COBRA_FIRE_AIM_RAD: f32 = AIM_TOLERANCE_RAD * 2.0;

pub fn cobra_tick(world: &World, ship: &Ship) -> TickInput {
    let Some(target) = nearest_target(ship, world) else {
        return sid_tick(world, ship);
    };
    let target_pos = ego_pos(ship, target.pos, world);

    if !line_of_sight_clear_world(ship.pos, target_pos, world) {
        return sid_tick(world, ship);
    }
    if let Some(divert) = wall_divert_angle(ship, world) {
        return brake_into_divert(ship, divert);
    }

    // Stateless weave: the side flips every COBRA_WEAVE_HALF_PERIOD_TICKS,
    // offset per-bot by entity_id so a squad of Cobras isn't synchronized.
    let phase = (world.tick.wrapping_add(ship.entity_id.wrapping_mul(17)))
        % (COBRA_WEAVE_HALF_PERIOD_TICKS * 2);
    let side = if phase < COBRA_WEAVE_HALF_PERIOD_TICKS {
        1.0
    } else {
        -1.0
    };

    // Lead-aim with our flight speed for the navigation target — predicts
    // where the target will be when we arrive at COBRA_FLIGHT_SPEED.
    let (intercept_dir, intercept_t) = match physics::lead_aim(
        ship.pos,
        ship.vel,
        target_pos,
        target.vel,
        COBRA_FLIGHT_SPEED,
    ) {
        Some((d, t)) => (d, t),
        None => {
            let to_p = target_pos - ship.pos;
            let len = to_p.length();
            if len < 1e-3 {
                return TickInput::default();
            }
            (to_p * (1.0 / len), len / COBRA_FLIGHT_SPEED)
        }
    };
    let intercept_pos = target_pos + target.vel * intercept_t;
    let to_int = intercept_pos - ship.pos;
    let int_dist = to_int.length();
    if int_dist < 1e-3 {
        return TickInput::default();
    }

    let perp = Vec2::new(-intercept_dir.y, intercept_dir.x);
    let offset =
        (int_dist * COBRA_OFFSET_FRACTION).clamp(COBRA_OFFSET_MIN, COBRA_OFFSET_MAX);
    let aim_target = intercept_pos + perp * (side * offset);
    let to_aim = aim_target - ship.pos;
    let flight_aim = aim_angle(to_aim.x, to_aim.y);
    let diff = shortest_angle_diff(flight_aim, ship.angle);
    let aimed_close = diff.abs() < AIM_TOLERANCE_RAD * 4.0;

    let aim_unit = forward(flight_aim);
    let speed_along = ship.vel.dot(aim_unit);
    let under_cap = speed_along < COBRA_FLIGHT_SPEED;

    // Opportunistic fire — separate lead-aim with bullet speed for the
    // FIRING decision (movement uses flight-speed lead, firing uses bullet-
    // speed lead). Fires when current heading is near the bullet-lead
    // direction, which happens momentarily as the weave sweeps through.
    let bullet_aim = match physics::lead_aim(
        ship.pos,
        ship.vel,
        target_pos,
        target.vel,
        BULLET_SPEED,
    ) {
        Some((d, _)) => aim_angle(d.x, d.y),
        None => aim_angle(target_pos.x - ship.pos.x, target_pos.y - ship.pos.y),
    };
    let fire_err = shortest_angle_diff(bullet_aim, ship.angle).abs();
    let charged = ship.shot_charge >= SHOT_CHARGE_COST;

    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust: aimed_close && under_cap,
        fire: fire_err < COBRA_FIRE_AIM_RAD && charged,
    }
}

// ── Vega ───────────────────────────────────────────────────────────────────
// Patient sniper. Hovers at long range (PREFERRED_RANGE ± BAND) and only
// fires when the lead-aim angle is very tight AND the intercept time is
// short (high confidence). Almost never closes — the player has to come
// to him, OR he picks them off from across the room. Stateless.
//
// Wall-blocked target → defers to Sid so the wall navigation can find a
// firing line; Vega resumes sniping once LOS is clear.

const VEGA_PREFERRED_RANGE: f32 = 600.0;
/// Half-width of the "settled" range band. Wider than the original 80 so
/// the target's natural movement doesn't constantly knock Vega out of
/// the band and into reposition mode (which used to lock out firing).
const VEGA_RANGE_BAND: f32 = 150.0;
/// Soft top speed when adjusting position to stay in the range band.
const VEGA_HOVER_SPEED: f32 = 180.0;
/// Aim tolerance for the snipe shot. Loosened from 0.5× to 1.0× of
/// `AIM_TOLERANCE_RAD` (same as Sid's fire gate) — the previous tightness
/// combined with movement-induced re-aim meant the window almost never
/// opened.
const VEGA_FIRE_AIM_RAD: f32 = AIM_TOLERANCE_RAD;
/// Maximum intercept time (seconds) for a confident shot. Anything longer
/// gives the target too much room to change direction. 2.0 s covers the
/// far edge of his preferred range plus a margin.
const VEGA_MAX_INTERCEPT_T: f32 = 2.0;
/// Minimum charge fraction before Vega will pull the trigger. Lower than
/// Reaper's load threshold but still above Sid's fire threshold — fires
/// from a half-full bar so he gets multiple shots per engagement instead
/// of single ultra-rare snipes.
const VEGA_FIRE_CHARGE_FRAC: f32 = 0.4;
/// Forward drift speed Vega tries to maintain while sniping. Bullet
/// velocity inherits ship velocity (`bvel = ship.vel + dir * BULLET_SPEED`),
/// so firing while drifting BACKWARDS makes the shot ~40 u/s slower in
/// target-frame than firing while drifting forwards. A small forward
/// drift (40 u/s) keeps the muzzle moving toward the target between
/// repositions instead of sliding away from a recent flee.
const VEGA_SNIPE_DRIFT_SPEED: f32 = 40.0;

pub fn vega_tick(world: &World, ship: &Ship) -> TickInput {
    let Some(target) = nearest_target(ship, world) else {
        return sid_tick(world, ship);
    };
    let target_pos = ego_pos(ship, target.pos, world);

    if !line_of_sight_clear_world(ship.pos, target_pos, world) {
        return sid_tick(world, ship);
    }
    if let Some(divert) = wall_divert_angle(ship, world) {
        return brake_into_divert(ship, divert);
    }

    let to_t = target_pos - ship.pos;
    let dist = to_t.length();
    let too_far = dist > VEGA_PREFERRED_RANGE + VEGA_RANGE_BAND;
    let too_close = dist < VEGA_PREFERRED_RANGE - VEGA_RANGE_BAND;
    let in_band = !too_far && !too_close;

    // Chaser inside our preferred range → flee like Wimpy. flee_path
    // builds speed up to WIMPY_FLEE_TOP_SPEED (320, vs Vega's hover 180),
    // does the velocity-corrected wall avoidance, and at top speed turns
    // around to spray back at the chaser. Way nastier than a slow back-off.
    if too_close {
        return flee_path(ship, target, world);
    }

    // Lead-aim is computed every tick — used for both the firing decision
    // (always) and the in-band aim direction. lead_aim accounts for ship's
    // own velocity, so we don't need to brake first; firing while drifting
    // a bit is fine.
    let (lead_dir, lead_t) = match physics::lead_aim(
        ship.pos,
        ship.vel,
        target_pos,
        target.vel,
        BULLET_SPEED,
    ) {
        Some((d, t)) => (aim_angle(d.x, d.y), t),
        None => (aim_angle(to_t.x, to_t.y), f32::INFINITY),
    };

    // Aim direction: in-band → lead direction (so we're always pointed
    // ready to fire). too_far → toward target. too_close is handled by the
    // flee branch above. The FIRE check below uses lead-aim independently,
    // so a close-in turn that happens to sweep past lead lands a shot.
    let move_aim = if too_far {
        aim_angle(to_t.x, to_t.y)
    } else {
        lead_dir
    };
    let diff = shortest_angle_diff(move_aim, ship.angle);
    let aimed_close = diff.abs() < AIM_TOLERANCE_RAD * 4.0;

    // Thrust selection:
    //   - Out-of-band → close in, capped at HOVER_SPEED.
    //   - In-band → maintain a small FORWARD drift (toward target) so
    //     bullets don't inherit a backwards velocity from the previous
    //     reposition. Without this Vega ends up "sniping while flying
    //     backwards" — bullets slow in target-frame, target dodges easily.
    let move_unit = forward(move_aim);
    let speed_along = ship.vel.dot(move_unit);
    let thrust = if in_band {
        aimed_close && speed_along < VEGA_SNIPE_DRIFT_SPEED
    } else {
        aimed_close && speed_along < VEGA_HOVER_SPEED
    };

    // Fire whenever the muzzle is on the lead direction with sufficient
    // charge and a confident intercept. Decoupled from the movement aim
    // so a reposition that happens to sweep past the lead direction lands
    // a shot. (lead_diff equals diff when in_band but is independent
    // otherwise.)
    let lead_diff = shortest_angle_diff(lead_dir, ship.angle).abs();
    let charged = ship.shot_charge >= SHOT_CHARGE_MAX * VEGA_FIRE_CHARGE_FRAC;
    let high_confidence = lead_t < VEGA_MAX_INTERCEPT_T;

    TickInput {
        client_tick: 0,
        turn_left: diff < 0.0,
        turn_right: diff > 0.0,
        thrust,
        fire: lead_diff < VEGA_FIRE_AIM_RAD && charged && high_confidence,
    }
}

/// If `input` doesn't already fire, set `fire = true` whenever Wimpy's nose
/// happens to be pointed at the nearest enemy with clear LOS and a charged
/// gun. Lets the cover-seek and peek-seek paths take a free shot when the
/// turn-toward-destination sweep crosses the target — without changing the
/// movement decision the path produced. Tolerance is wider than attack mode's
/// because the alignment is incidental, not the goal.
fn add_opportunistic_fire(input: TickInput, ship: &Ship, world: &World) -> TickInput {
    if input.fire {
        return input;
    }
    let Some(target) = nearest_target(ship, world) else {
        return input;
    };
    let target_pos = ego_pos(ship, target.pos, world);
    if !line_of_sight_clear_world(ship.pos, target_pos, world) {
        return input;
    }
    let dx = target_pos.x - ship.pos.x;
    let dy = target_pos.y - ship.pos.y;
    let aim_dir = aim_angle(dx, dy);
    let aim_err = shortest_angle_diff(aim_dir, ship.angle).abs();
    let charged = ship.shot_charge > SHOT_CHARGE_MAX * FIRE_CHARGE_FRACTION;
    if aim_err < OPPORTUNISTIC_AIM_RAD && charged {
        TickInput {
            fire: true,
            ..input
        }
    } else {
        input
    }
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
