// Cosmetic thruster embers. Client-only, purely visual — NOT part of the
// sim. Explosion debris lives in the shared sim now (`World::particles`)
// and arrives via snapshot.

use shared::entities::{forward, Ship};
use shared::map::WallSegment;
use shared::math::{self, Vec2};
use shared::physics;

const SPAWN_PER_SECOND: f32 = 30.0; // matches Elm `shipEmberRate = 30 / Time.second`
const PARTICLE_LIFE_SECONDS: f32 = 0.85;
const PARTICLE_SPEED: f32 = 180.0; // ~Elm `shipEmberSpeed`
const PARTICLE_JITTER_SPEED: f32 = 60.0;
const PARTICLE_ANGLE_JITTER: f32 = 0.8;
/// Body-coord y offset for spawn point (just behind the back edge at y=6).
const TAIL_BACK_Y: f32 = 8.0;
/// Half-width of spawn spread along the back edge (back edge runs from x=-7 to x=+7).
const TAIL_HALF_WIDTH: f32 = 6.5;

#[derive(Clone, Copy)]
pub struct Particle {
    pub pos: Vec2,
    pub vel: Vec2,
    pub age: f32,
    pub life: f32,
}

#[derive(Default)]
pub struct ParticleField {
    pub items: Vec<Particle>,
    accumulator: f32,
    rng_state: u32,
}

impl ParticleField {
    pub fn new() -> Self {
        Self {
            items: Vec::with_capacity(256),
            accumulator: 0.0,
            rng_state: 0x9E3779B1,
        }
    }

    fn rand_unit(&mut self) -> f32 {
        let mut x = self.rng_state;
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        self.rng_state = x;
        (x as f32) / (u32::MAX as f32)
    }

    /// Age + cull, then spawn from any thrusting ship. Particles that swept
    /// through a wall this tick are dropped.
    pub fn step<'a>(
        &mut self,
        ships: impl IntoIterator<Item = &'a Ship>,
        walls: &[WallSegment],
        dt: f32,
    ) {
        self.items.retain_mut(|p| {
            p.age += dt;
            if p.age >= p.life {
                return false;
            }
            let prev = p.pos;
            p.pos += p.vel * dt;
            for wall in walls {
                if physics::segments_intersect(prev, p.pos, wall.a, wall.b) {
                    return false;
                }
            }
            true
        });

        let mut any_thrusting = false;
        for ship in ships {
            if !ship.thrusting {
                continue;
            }
            any_thrusting = true;
            let cos_a = math::cos(ship.angle);
            let sin_a = math::sin(ship.angle);
            let back = forward(ship.angle) * -1.0;

            self.accumulator += SPAWN_PER_SECOND * dt;
            while self.accumulator >= 1.0 {
                self.accumulator -= 1.0;
                let r1 = self.rand_unit();
                let r2 = self.rand_unit();
                let r3 = self.rand_unit();
                let r4 = self.rand_unit();

                let body_x = (r1 * 2.0 - 1.0) * TAIL_HALF_WIDTH;
                let body_y = TAIL_BACK_Y;
                let world_pos = Vec2::new(
                    ship.pos.x + body_x * cos_a - body_y * sin_a,
                    ship.pos.y + body_x * sin_a + body_y * cos_a,
                );

                let jitter_angle = (r2 - 0.5) * PARTICLE_ANGLE_JITTER;
                let cj = math::cos(jitter_angle);
                let sj = math::sin(jitter_angle);
                let dir = Vec2::new(back.x * cj - back.y * sj, back.x * sj + back.y * cj);
                let speed = PARTICLE_SPEED + (r3 - 0.5) * PARTICLE_JITTER_SPEED;

                self.items.push(Particle {
                    pos: world_pos,
                    vel: ship.vel * 0.5 + dir * speed,
                    age: 0.0,
                    life: PARTICLE_LIFE_SECONDS * (0.7 + 0.6 * r4),
                });
            }
        }
        if !any_thrusting {
            self.accumulator = 0.0;
        }
    }

}
