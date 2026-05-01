// Game tuning constants. Starting values from CLAUDE.md spec; feel-test then tune.
// Original Elm-version values (in `old/`) used per-millisecond rates — these are per-second.

// Tick / snapshot
pub const TICK_RATE_HZ: u32 = 60;
pub const TICK_DT_SECONDS: f32 = 1.0 / 60.0;
// Same as sim rate — 30Hz left visible kink between snapshots even with
// Hermite interpolation. Bandwidth is trivial for this game.
pub const SNAPSHOT_RATE_HZ: u32 = 60;

// World — small test arena for now; bump up once we have a proper map editor.
pub const WORLD_WIDTH: f32 = 1800.0;
pub const WORLD_HEIGHT: f32 = 1200.0;

// Ship
pub const SHIP_MASS: f32 = 1.0;
pub const SHIP_RADIUS: f32 = 12.0; // legacy / informational (real collision is triangle vs segment)
pub const SHIP_TURN_RATE: f32 = 6.0; // rad/s — matches Elm `shipMaxTurn = 6 / Time.second`
pub const SHIP_THRUST_ACCEL: f32 = 400.0;
pub const SHIP_LINEAR_DAMPING: f32 = 0.0;
pub const SHIP_MAX_HP: u32 = 100;
pub const SHIP_RESPAWN_SECONDS: f32 = 2.0;
/// Wall-crash kill thresholds (units/sec). If the velocity component going
/// INTO the wall exceeds the contact vertex's threshold, the ship blows up.
/// Nose impacts (ramming forward) kill at lower speed than back/side
/// impacts so a careless head-first thrust into a wall is fatal but a
/// sideways scrape isn't. Best-guess values for now — make them
/// per-server-room when the lobby ships.
pub const SHIP_NOSE_KILL_SPEED: f32 = 350.0;
pub const SHIP_BACK_KILL_SPEED: f32 = 600.0;

// Bullet
pub const BULLET_SPEED: f32 = 220.0; // units/s — matches Elm `shotVel = 220 / Time.second`
pub const BULLET_LIFETIME_SECONDS: f32 = 5.0; // matches Elm `shotLife = 5 * Time.second`
// TEMPORARY: any hit kills (testing). Tune down toward Elm ~20 once combat
// pacing is dialed in.
pub const BULLET_DAMAGE: u32 = 100;
pub const BULLET_MASS: f32 = 0.01;
pub const BULLET_COOLDOWN_SECONDS: f32 = 0.15;
/// How long after firing a bullet ignores its own shooter — prevents the
/// muzzle position from registering an immediate self-hit.
pub const BULLET_SELF_HIT_GRACE_SECONDS: f32 = 0.05;

// Energy / shot charge — matches Elm `shotCharge=1000, shotChargeRate=400/s,
// shotChargeCost=200`. Caps spam: ~5 shots burst, full recharge in 2.5s.
pub const SHOT_CHARGE_MAX: f32 = 1000.0;
pub const SHOT_CHARGE_RATE: f32 = 400.0;
pub const SHOT_CHARGE_COST: f32 = 200.0;

// Thruster wash (force cone behind a thrusting ship)
pub const WASH_CONE_LENGTH: f32 = 140.0;
pub const WASH_CONE_HALF_ANGLE_RAD: f32 = 0.35;
/// Per-second acceleration applied at the cone mouth, falling off linearly to
/// 0 at the tip. Treated as direct vel delta (NOT divided by mass) so bullets
/// don't blast off into the next county.
pub const WASH_FORCE_AT_MOUTH: f32 = 180.0;

// Death explosion — debris is now real sim particles. They spawn from the
// dead ship outward, fly with their initial velocity, die on walls, and
// transfer momentum into any ship they hit. No more separate radial force.
pub const EXPLOSION_PARTICLE_COUNT: u32 = 60;
pub const PARTICLE_LIFE_MIN: f32 = 1.0;
pub const PARTICLE_LIFE_MAX: f32 = 3.0;
pub const PARTICLE_SPEED_MIN: f32 = 120.0;
pub const PARTICLE_SPEED_MAX: f32 = 480.0;
/// Mass per particle. Tuned by feel — knob is per-particle so it stays
/// linear in "how big a push do you get per particle that hits."
pub const PARTICLE_MASS: f32 = 0.45;

// Cannons. A cannon is a wall block with a small white triangle on its
// firing-direction side. Each shot's delay and angle are randomized — the
// delay is uniformly drawn from [MIN, MAX] seconds (applies to the FIRST
// shot too, so a fresh map's cannons stagger naturally), and the firing
// angle is uniformly drawn from a fan of ±FAN_RAD around the cannon's
// nominal direction. One player-bullet hit kills it; it respawns after
// RESPAWN. Triangle visual height is TRIANGLE_FRAC of a block, base = full
// block. Ship-vs-cannon contact (ship enters the triangle's AABB) kills
// both — wall-block crashes go through normal ship-vs-wall physics.
pub const CANNON_HP: u32 = 100;
/// Wide range so a cannon can fire two shots back-to-back at MIN or pause
/// for several seconds at MAX — feels less metronomic.
pub const CANNON_FIRE_MIN_SECONDS: f32 = 0.3;
pub const CANNON_FIRE_MAX_SECONDS: f32 = 5.0;
pub const CANNON_FIRE_FAN_RAD: f32 = core::f32::consts::PI / 4.0; // ±45°
pub const CANNON_RESPAWN_SECONDS: f32 = 20.0;
/// Visual + collision height of the triangle as a fraction of block size.
pub const CANNON_TRIANGLE_FRAC: f32 = 0.3;
/// Inset in world-units (= screen px at no zoom) on each end of the
/// triangle base. 0 = base spans the full block width and overlaps the
/// wall outline (which is what we want — the cannon reads as part of the
/// wall, not a smaller decoration on it).
pub const CANNON_BASE_INSET: f32 = 0.0;
/// Distance the whole triangle is shifted OUTWARD (in the firing direction)
/// past the wall's edge, in world-units. 0 = base flush with the wall edge.
pub const CANNON_SIT_OFFSET: f32 = 0.0;
/// Cannon-bullet speed range — randomized per shot, slower than player
/// bullets so they read as lobs rather than snipes.
pub const CANNON_BULLET_SPEED_MIN: f32 = 100.0;
pub const CANNON_BULLET_SPEED_MAX: f32 = 200.0;
/// When a cannon has a target ship in its fan, the actual fire angle is
/// `aim_angle ± uniform(0, AIM_NOISE)` — keeps shots from being perfect
/// snipes while still being threatening.
pub const CANNON_AIM_NOISE_RAD: f32 = core::f32::consts::PI / 12.0; // ±15°

// Networking
pub const ROOM_PLAYER_CAP: usize = 8;
pub const ROOM_IDLE_TIMEOUT_SECONDS: f32 = 30.0;
/// Padding added to a client's reported viewport on each side when filtering
/// bullets/particles into per-player snapshots. Sized so the fastest entities
/// can't cross it within one interp window: BULLET_SPEED * INTERP_DELAY_S =
/// 220 * 0.1 ≈ 22u, and we round up generously so a brief stall on the wire
/// can't expose a pop-in either.
pub const AOI_MARGIN: f32 = 200.0;
