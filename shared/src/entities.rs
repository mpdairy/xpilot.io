use serde::{Deserialize, Serialize};

use crate::math::{self, Vec2};
use crate::protocol::TickInput;

pub type EntityId = u32;
pub type PlayerId = u32;

/// Convention: `angle = 0` points along -Y (up on screen). Positive `angle`
/// rotates clockwise on screen, matching the player's intuition of "turn right".
pub fn forward(angle: f32) -> Vec2 {
    Vec2::new(math::sin(angle), -math::cos(angle))
}

/// Ship triangle in body coordinates. Pivot is body `(0, 0)` — same as the
/// original Elm version: it rotates around a point 1 unit aft of the geometric
/// centroid `(0, -1)`, which gives the nose a longer swing arc than the tail.
/// Vertices: back-left, back-right, nose.
pub const SHIP_BODY: [Vec2; 3] = [
    Vec2::new(-7.0, 6.0),
    Vec2::new(7.0, 6.0),
    Vec2::new(0.0, -15.0),
];

/// Distance from pivot to nose vertex along the forward direction. Used as the
/// muzzle offset when firing.
pub const SHIP_NOSE_OFFSET: f32 = 15.0;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Ship {
    pub entity_id: EntityId,
    pub player_id: PlayerId,
    pub pos: Vec2,
    pub vel: Vec2,
    pub angle: f32,
    pub mass: f32,
    pub hp: u32,
    pub thrusting: bool,
    pub fire_cooldown: f32,
    /// Current shot energy (0..=SHOT_CHARGE_MAX). Regens each tick;
    /// firing costs SHOT_CHARGE_COST.
    pub shot_charge: f32,
    /// What input was last applied to this ship. Server sets this in
    /// `apply_ship_dynamics` and includes it in snapshots so the client can
    /// extrapolate remote ships forward by replaying the same input each tick
    /// — much smoother than interpolating sparse snapshots after the fact.
    pub last_input: TickInput,
}

impl Ship {
    /// Triangle vertices in world coords (rotated by `angle`, translated by `pos`).
    pub fn world_vertices(&self) -> [Vec2; 3] {
        let cos_a = math::cos(self.angle);
        let sin_a = math::sin(self.angle);
        let mut out = [Vec2::ZERO; 3];
        for i in 0..3 {
            let p = SHIP_BODY[i];
            out[i] = Vec2::new(
                self.pos.x + p.x * cos_a - p.y * sin_a,
                self.pos.y + p.x * sin_a + p.y * cos_a,
            );
        }
        out
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Bullet {
    pub entity_id: EntityId,
    pub shooter: EntityId,
    pub pos: Vec2,
    pub vel: Vec2,
    pub mass: f32,
    pub age_seconds: f32,
}
