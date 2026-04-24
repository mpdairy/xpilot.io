// shared/ compiles to both native and wasm32. Keep it dependency-light.
// All transcendentals must go through `math::` helpers (libm-backed) so the
// sim is bit-exact across targets. See CLAUDE.md "Determinism is a hard requirement".

pub mod constants;
pub mod entities;
pub mod map;
pub mod math;
pub mod physics;
pub mod protocol;
pub mod world;
pub mod xp_map;
