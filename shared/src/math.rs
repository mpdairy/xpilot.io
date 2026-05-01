use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Copy, Clone, Debug, Default, PartialEq)]
pub struct Vec2 {
    pub x: f32,
    pub y: f32,
}

impl Vec2 {
    pub const ZERO: Vec2 = Vec2 { x: 0.0, y: 0.0 };

    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    pub fn length(self) -> f32 {
        libm::sqrtf(self.x * self.x + self.y * self.y)
    }

    pub fn length_squared(self) -> f32 {
        self.x * self.x + self.y * self.y
    }

    pub fn normalized(self) -> Self {
        let len = self.length();
        if len > 0.0 {
            Self {
                x: self.x / len,
                y: self.y / len,
            }
        } else {
            Self::ZERO
        }
    }

    pub fn dot(self, other: Self) -> f32 {
        self.x * other.x + self.y * other.y
    }

    pub fn rotate(self, angle_rad: f32) -> Self {
        let c = cos(angle_rad);
        let s = sin(angle_rad);
        Self {
            x: self.x * c - self.y * s,
            y: self.x * s + self.y * c,
        }
    }
}

impl core::ops::Add for Vec2 {
    type Output = Vec2;
    fn add(self, rhs: Vec2) -> Vec2 {
        Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

impl core::ops::Sub for Vec2 {
    type Output = Vec2;
    fn sub(self, rhs: Vec2) -> Vec2 {
        Vec2 { x: self.x - rhs.x, y: self.y - rhs.y }
    }
}

impl core::ops::Mul<f32> for Vec2 {
    type Output = Vec2;
    fn mul(self, rhs: f32) -> Vec2 {
        Vec2 { x: self.x * rhs, y: self.y * rhs }
    }
}

impl core::ops::AddAssign for Vec2 {
    fn add_assign(&mut self, rhs: Vec2) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

// Deterministic transcendentals via libm — same bits on x86 and wasm32.
// Never call std::f32::sin/cos/etc. inside the sim.

pub fn sin(x: f32) -> f32 {
    libm::sinf(x)
}

pub fn cos(x: f32) -> f32 {
    libm::cosf(x)
}

pub fn atan2(y: f32, x: f32) -> f32 {
    libm::atan2f(y, x)
}

pub fn sqrt(x: f32) -> f32 {
    libm::sqrtf(x)
}

const TWO_PI: f32 = core::f32::consts::TAU;

pub fn wrap_angle(a: f32) -> f32 {
    let r = a - libm::floorf(a / TWO_PI) * TWO_PI;
    if r < 0.0 { r + TWO_PI } else { r }
}

/// Per-axis torus distance: shortest distance between `a` and `b` modulo
/// `world_dim`, ignoring sign. For non-wrapping axes, pass the same value
/// you would pass for a wrap one — the caller picks whether to use this or
/// plain `(a - b).abs()` based on the map's `edge_wrap` flag.
fn torus_axis_dist(a: f32, b: f32, world_dim: f32) -> f32 {
    let d = (a - b).abs();
    if d > world_dim * 0.5 { world_dim - d } else { d }
}

/// Is point `p` inside the axis-aligned rectangle centered at `center` with
/// half-extents `(half_w, half_h)`, with optional torus wrap? Used by the
/// server's per-player snapshot filter so an entity sitting just past the
/// world seam is still considered "in view" of a camera near the opposite
/// seam.
pub fn point_in_rect_torus(
    p: Vec2,
    center: Vec2,
    half_w: f32,
    half_h: f32,
    world_w: f32,
    world_h: f32,
    edge_wrap: bool,
) -> bool {
    let (dx, dy) = if edge_wrap {
        (
            torus_axis_dist(p.x, center.x, world_w),
            torus_axis_dist(p.y, center.y, world_h),
        )
    } else {
        ((p.x - center.x).abs(), (p.y - center.y).abs())
    };
    dx <= half_w && dy <= half_h
}
