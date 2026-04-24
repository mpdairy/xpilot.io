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

// Networking
pub const ROOM_PLAYER_CAP: usize = 8;
pub const ROOM_IDLE_TIMEOUT_SECONDS: f32 = 30.0;
