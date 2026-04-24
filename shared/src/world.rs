// Authoritative-style sim state. `World::step` is deterministic given
// identical inputs — same on server and client prediction.
//
// The per-ship and per-bullet pieces are exposed as free fns so the client
// can drive prediction by calling `apply_ship_dynamics` on just its own ship,
// without spawning bullets locally (the server is authoritative for those).

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::constants::*;
use crate::entities::{forward, Bullet, EntityId, PlayerId, Ship, SHIP_NOSE_OFFSET};
use crate::map::{Block, Map};
use crate::math::{self, Vec2};
use crate::physics;
use crate::protocol::{GameEvent, TickInput};

const WALL_RESTITUTION: f32 = 0.5;
/// Iterations per tick of the wall-resolution pass — enough to settle a ship
/// trapped in a corner where two walls overlap with it on the same tick.
const COLLISION_ITERS: u32 = 3;

/// Wrap a position into [0, w) × [0, h) for toroidal maps. Uses `rem_euclid`
/// so negative inputs wrap correctly (a ship that just passed x=0 lands at
/// x≈w, not x≈-1).
fn wrap_pos(p: Vec2, w: f32, h: f32) -> Vec2 {
    Vec2::new(p.x.rem_euclid(w), p.y.rem_euclid(h))
}

/// If `ship.pos` lies inside a wall block, teleport it to the center of the
/// nearest non-wall block (Chebyshev BFS) and zero its velocity. No-op when
/// the map has no block grid (hand-built test maps).
fn unstick_if_embedded(ship: &mut Ship, map: &Map) {
    let Some(grid) = map.blocks.as_ref() else { return };
    let bs = grid.block_size;
    let bx = (ship.pos.x / bs).floor() as i64;
    let by = (ship.pos.y / bs).floor() as i64;
    let here = if map.edge_wrap {
        grid.get(
            bx.rem_euclid(grid.width as i64),
            by.rem_euclid(grid.height as i64),
        )
    } else {
        grid.get(bx, by)
    };
    if !here.is_wall() {
        return;
    }
    // Search outward by Chebyshev distance for the nearest non-wall cell.
    // Cap at 8 blocks — beyond that something is very wrong and a kick
    // anywhere is preferable to an infinite scan.
    for r in 1..=8i64 {
        for dy in -r..=r {
            for dx in -r..=r {
                if dx.abs().max(dy.abs()) != r {
                    continue;
                }
                let nbx = bx + dx;
                let nby = by + dy;
                let cell = if map.edge_wrap {
                    grid.get(
                        nbx.rem_euclid(grid.width as i64),
                        nby.rem_euclid(grid.height as i64),
                    )
                } else {
                    grid.get(nbx, nby)
                };
                if !matches!(cell, Block::Wall | Block::TriUL | Block::TriUR | Block::TriLL | Block::TriLR) {
                    let cx = (nbx as f32 + 0.5) * bs;
                    let cy = (nby as f32 + 0.5) * bs;
                    let mut p = Vec2::new(cx, cy);
                    if map.edge_wrap {
                        p = wrap_pos(p, map.width, map.height);
                    }
                    ship.pos = p;
                    ship.vel = Vec2::ZERO;
                    return;
                }
            }
        }
    }
}

pub struct World {
    pub tick: u32,
    pub ships: BTreeMap<EntityId, Ship>,
    pub bullets: BTreeMap<EntityId, Bullet>,
    /// Live explosion debris. Each particle is a real sim entity: it moves,
    /// dies on walls, and pushes any ship it touches by transferring its
    /// momentum. No more radial blast field — what you see is exactly what
    /// applies the force.
    pub particles: Vec<Particle>,
    pub map: Map,
    next_entity_id: EntityId,
    /// Deterministic xorshift32 RNG state. Drives explosion-particle spawn
    /// directions/speeds/lifetimes so server runs are reproducible.
    rng_state: u32,
}

/// One piece of explosion debris. When a particle's swept path this tick
/// crosses a ship's triangle, the particle dies and the ship absorbs
/// `vel * mass / ship.mass` of velocity. Walls kill particles outright.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Particle {
    pub pos: Vec2,
    pub vel: Vec2,
    pub age: f32,
    pub life: f32,
    pub mass: f32,
}

impl World {
    pub fn new(mut map: Map) -> Self {
        // The wire form of Map ships an empty `walls` (skipped in serde) —
        // recompute from blocks now so the sim has something to collide
        // against. No-op for hand-built maps that don't have blocks.
        map.rebuild_walls();
        Self {
            tick: 0,
            ships: BTreeMap::new(),
            bullets: BTreeMap::new(),
            particles: Vec::new(),
            map,
            next_entity_id: 1,
            rng_state: 0x9E37_79B1, // arbitrary nonzero
        }
    }

    /// xorshift32, kept inline so this whole sim stays free of `rand` deps.
    /// Returns f32 in [0, 1).
    fn next_rand(&mut self) -> f32 {
        let mut x = self.rng_state;
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        self.rng_state = x;
        (x as f32) / (u32::MAX as f32)
    }

    /// Spawn `count` explosion particles at `center`, each with `base_vel`
    /// (the dead ship's velocity) plus a random outward burst.
    fn spawn_explosion_particles(&mut self, center: Vec2, base_vel: Vec2, count: u32) {
        for _ in 0..count {
            let r1 = self.next_rand();
            let r2 = self.next_rand();
            let r3 = self.next_rand();
            let angle = r1 * core::f32::consts::TAU;
            let speed = PARTICLE_SPEED_MIN + r2 * (PARTICLE_SPEED_MAX - PARTICLE_SPEED_MIN);
            let life = PARTICLE_LIFE_MIN + r3 * (PARTICLE_LIFE_MAX - PARTICLE_LIFE_MIN);
            let outward = Vec2::new(math::cos(angle), math::sin(angle)) * speed;
            self.particles.push(Particle {
                pos: center,
                vel: base_vel + outward,
                age: 0.0,
                life,
                mass: PARTICLE_MASS,
            });
        }
    }

    fn alloc_id(&mut self) -> EntityId {
        let id = self.next_entity_id;
        self.next_entity_id += 1;
        id
    }

    pub fn spawn_ship(&mut self, player_id: PlayerId, pos: Vec2, angle: f32) -> EntityId {
        let entity_id = self.alloc_id();
        self.insert_ship(entity_id, player_id, pos, angle);
        entity_id
    }

    /// Re-insert a ship at a fixed entity_id. Used on respawn so the same
    /// player keeps the same id across deaths (clients reference it for
    /// prediction and HUD attribution).
    pub fn respawn_ship(&mut self, entity_id: EntityId, player_id: PlayerId, pos: Vec2, angle: f32) {
        self.insert_ship(entity_id, player_id, pos, angle);
    }

    fn insert_ship(&mut self, entity_id: EntityId, player_id: PlayerId, pos: Vec2, angle: f32) {
        let ship = Ship {
            entity_id,
            player_id,
            pos,
            vel: Vec2::ZERO,
            angle,
            mass: SHIP_MASS,
            hp: SHIP_MAX_HP,
            thrusting: false,
            fire_cooldown: 0.0,
            shot_charge: SHOT_CHARGE_MAX,
            last_input: TickInput::default(),
        };
        self.ships.insert(entity_id, ship);
    }

    /// Advance the sim by one fixed tick. `inputs` is keyed by ship entity id.
    /// Iteration order is BTreeMap-stable for determinism. Returns the events
    /// that fired this tick so the caller (server room) can broadcast them.
    pub fn step(&mut self, inputs: &BTreeMap<EntityId, TickInput>) -> Vec<GameEvent> {
        let mut events: Vec<GameEvent> = Vec::new();
        let dt = TICK_DT_SECONDS;

        // 1. Per-ship dynamics + collect new-bullet intents.
        let mut new_bullets: Vec<NewBullet> = Vec::new();
        for (id, ship) in self.ships.iter_mut() {
            let input = inputs.get(id).copied().unwrap_or_default();
            apply_ship_dynamics(ship, &input, &self.map);
            if let Some(b) = try_fire(ship, &input) {
                new_bullets.push(b);
            }
        }
        for b in new_bullets {
            let entity_id = self.alloc_id();
            self.bullets.insert(
                entity_id,
                Bullet {
                    entity_id,
                    shooter: b.shooter,
                    pos: b.pos,
                    vel: b.vel,
                    mass: BULLET_MASS,
                    age_seconds: 0.0,
                },
            );
        }

        // 2. Thruster wash — every thrusting ship emits a force cone behind
        //    it that pushes other ships and bullets in that cone.
        apply_thruster_wash(self, dt);

        // 3. Ship-ship collision: any two triangles that overlap, both die.
        let ship_ids: Vec<EntityId> = self.ships.keys().copied().collect();
        let mut pairs: Vec<(EntityId, EntityId)> = Vec::new();
        for i in 0..ship_ids.len() {
            for j in (i + 1)..ship_ids.len() {
                let a_id = ship_ids[i];
                let b_id = ship_ids[j];
                let a_verts = self.ships[&a_id].world_vertices();
                let b_verts = self.ships[&b_id].world_vertices();
                if physics::triangles_overlap(&a_verts, &b_verts) {
                    pairs.push((a_id, b_id));
                }
            }
        }
        for (a_id, b_id) in pairs {
            let a_player = self.ships.get(&a_id).map(|s| s.player_id);
            let b_player = self.ships.get(&b_id).map(|s| s.player_id);
            if let Some(s) = self.ships.remove(&a_id) {
                let (p, v) = (s.pos, s.vel);
                events.push(GameEvent::ShipDied {
                    entity_id: a_id,
                    killer: b_player,
                    pos: p,
                    vel: v,
                });
                self.spawn_explosion_particles(p, v, EXPLOSION_PARTICLE_COUNT);
            }
            if let Some(s) = self.ships.remove(&b_id) {
                let (p, v) = (s.pos, s.vel);
                events.push(GameEvent::ShipDied {
                    entity_id: b_id,
                    killer: a_player,
                    pos: p,
                    vel: v,
                });
                self.spawn_explosion_particles(p, v, EXPLOSION_PARTICLE_COUNT);
            }
        }

        // 4. Step bullets. Capture each bullet's prev pos for the swept hit test.
        let mut bullet_prev: BTreeMap<EntityId, Vec2> = BTreeMap::new();
        let mut dead_bullets: Vec<EntityId> = Vec::new();
        for (id, bullet) in self.bullets.iter_mut() {
            bullet_prev.insert(*id, bullet.pos);
            if step_bullet(bullet, &self.map) {
                dead_bullets.push(*id);
            }
        }
        for id in &dead_bullets {
            self.bullets.remove(id);
        }

        // 5. Bullet vs ship hits (swept). Iterate in id order for determinism.
        let mut hits: Vec<(EntityId, EntityId)> = Vec::new();
        'bullets: for (b_id, bullet) in &self.bullets {
            let prev = match bullet_prev.get(b_id) {
                Some(p) => *p,
                None => bullet.pos,
            };
            for (s_id, ship) in &self.ships {
                if ship.entity_id == bullet.shooter
                    && bullet.age_seconds < BULLET_SELF_HIT_GRACE_SECONDS
                {
                    continue;
                }
                let verts = ship.world_vertices();
                if physics::segment_hits_triangle(prev, bullet.pos, &verts) {
                    hits.push((*b_id, *s_id));
                    continue 'bullets;
                }
            }
        }

        // 5b. Step explosion debris. Walls absorb particles outright; ships
        // absorb them and gain `vel * mass / ship.mass` of velocity. No more
        // radial blast field — what you see is what pushes you.
        step_particles(self, dt);

        // 6. Apply bullet hits.
        for (b_id, s_id) in hits {
            let bullet = match self.bullets.remove(&b_id) {
                Some(b) => b,
                None => continue,
            };
            let shooter_player = self.ships.get(&bullet.shooter).map(|s| s.player_id);
            let victim_player = match self.ships.get(&s_id) {
                Some(s) => s.player_id,
                None => continue,
            };

            let new_hp = self
                .ships
                .get(&s_id)
                .map(|s| s.hp)
                .unwrap_or(0)
                .saturating_sub(BULLET_DAMAGE);
            if new_hp == 0 {
                let (death_pos, death_vel) = self
                    .ships
                    .get(&s_id)
                    .map(|s| (s.pos, s.vel))
                    .unwrap_or((Vec2::ZERO, Vec2::ZERO));
                self.ships.remove(&s_id);
                events.push(GameEvent::ShipDied {
                    entity_id: s_id,
                    killer: shooter_player,
                    pos: death_pos,
                    vel: death_vel,
                });
                self.spawn_explosion_particles(death_pos, death_vel, EXPLOSION_PARTICLE_COUNT);
            } else {
                if let Some(s) = self.ships.get_mut(&s_id) {
                    s.hp = new_hp;
                }
                events.push(GameEvent::HitScored {
                    shooter: shooter_player.unwrap_or(0),
                    victim: victim_player,
                    damage: BULLET_DAMAGE,
                });
            }
        }

        self.tick = self.tick.wrapping_add(1);
        events
    }
}

/// Apply the per-tick thruster-wash impulse from each thrusting ship to
/// every other ship and bullet inside that ship's cone.
fn apply_thruster_wash(world: &mut World, dt: f32) {
    // Gather sources to dodge the borrow checker (we can't iterate ships
    // immutably while pushing on ships mutably below).
    let sources: Vec<(EntityId, Vec2, Vec2)> = world
        .ships
        .values()
        .filter(|s| s.thrusting)
        .map(|s| (s.entity_id, s.pos, forward(s.angle) * -1.0))
        .collect();

    for (source_id, apex, axis) in sources {
        // Push other ships.
        for ship in world.ships.values_mut() {
            if ship.entity_id == source_id {
                continue;
            }
            if let Some(force) = physics::cone_force(
                ship.pos,
                apex,
                axis,
                WASH_CONE_LENGTH,
                WASH_CONE_HALF_ANGLE_RAD,
                WASH_FORCE_AT_MOUTH,
            ) {
                ship.vel += force * dt;
            }
        }
        // Push bullets — including bullets from the source ship; the wash
        // reasonably blows your own muzzle bullets sideways too.
        for bullet in world.bullets.values_mut() {
            if let Some(force) = physics::cone_force(
                bullet.pos,
                apex,
                axis,
                WASH_CONE_LENGTH,
                WASH_CONE_HALF_ANGLE_RAD,
                WASH_FORCE_AT_MOUTH,
            ) {
                bullet.vel += force * dt;
            }
        }
    }
}

/// Per-tick particle update: move, age, then check this tick's swept path
/// against walls (kill on contact) and ship triangles (kill on contact and
/// transfer momentum into the ship). Iterating ships in BTreeMap order keeps
/// the result deterministic when one particle could hit two ships.
fn step_particles(world: &mut World, dt: f32) {
    // Snapshot ship triangles up front so we don't hold a borrow on
    // world.ships while iterating world.particles below.
    let ship_tris: Vec<(EntityId, [Vec2; 3])> = world
        .ships
        .iter()
        .map(|(id, s)| (*id, s.world_vertices()))
        .collect();

    let walls = world.map.walls.clone();
    // Accumulate per-ship velocity deltas and apply at the end so iteration
    // order doesn't affect intermediate triangle positions (they were
    // captured above anyway, but applying mid-loop would still be racy if we
    // ever add ship-ship coupling).
    let mut ship_dv: BTreeMap<EntityId, Vec2> = BTreeMap::new();

    let edge_wrap = world.map.edge_wrap;
    let map_w = world.map.width;
    let map_h = world.map.height;
    world.particles.retain_mut(|p| {
        let prev = p.pos;
        p.pos += p.vel * dt;
        p.age += dt;
        if p.age >= p.life {
            return false;
        }
        for w in &walls {
            if physics::segments_intersect(prev, p.pos, w.a, w.b) {
                return false;
            }
        }
        for (id, tri) in &ship_tris {
            if physics::segment_hits_triangle(prev, p.pos, tri) {
                ship_dv
                    .entry(*id)
                    .and_modify(|dv| *dv += p.vel * p.mass)
                    .or_insert_with(|| p.vel * p.mass);
                return false;
            }
        }
        if edge_wrap {
            p.pos = wrap_pos(p.pos, map_w, map_h);
        }
        true
    });

    for (id, dv) in ship_dv {
        if let Some(ship) = world.ships.get_mut(&id) {
            // Divide by ship mass at apply time so the recorded dv reads as
            // pure momentum.
            ship.vel += dv * (1.0 / ship.mass);
        }
    }
}

/// New-bullet intent emitted by `try_fire`. The world assigns the entity id.
#[derive(Clone, Copy, Debug)]
pub struct NewBullet {
    pub shooter: EntityId,
    pub pos: Vec2,
    pub vel: Vec2,
}

/// Step a single ship: input → turn / thrust / integrate / wall collide /
/// cooldown decay (and recoil if firing). Does NOT spawn the bullet — see
/// `try_fire` for that. The split lets the client predict ship motion without
/// having to deal with bullet authority.
pub fn apply_ship_dynamics(ship: &mut Ship, input: &TickInput, map: &Map) {
    // Record what we're applying so receivers of snapshots can extrapolate
    // this ship forward identically.
    ship.last_input = *input;
    apply_ship_dynamics_dt(ship, input, map, TICK_DT_SECONDS);
}

/// Same dynamics step as `apply_ship_dynamics` but with arbitrary `dt`. Used
/// by the client to do a sub-tick extrapolation step when rendering remote
/// ships at a fractional point between snapshots. Does NOT update
/// `ship.last_input` — extrapolation is "this is what's still happening," not
/// a new applied input.
pub fn apply_ship_dynamics_dt(ship: &mut Ship, input: &TickInput, map: &Map, dt: f32) {
    // 1. Turn.
    let turn_dir = (input.turn_right as i32 - input.turn_left as i32) as f32;
    ship.angle = math::wrap_angle(ship.angle + turn_dir * SHIP_TURN_RATE * dt);

    // 2. Thrust.
    ship.thrusting = input.thrust;
    let mut accel = Vec2::ZERO;
    if input.thrust {
        accel = forward(ship.angle) * SHIP_THRUST_ACCEL;
    }

    // 3. Apply velocity change from accel + damping (once per tick — only
    //    the position step is sub-stepped).
    ship.vel += accel * dt;
    if SHIP_LINEAR_DAMPING > 0.0 {
        let damp = (1.0 - SHIP_LINEAR_DAMPING * dt).max(0.0);
        ship.vel = ship.vel * damp;
    }

    // 4. Sub-stepped position update. At high speed (e.g. post-explosion or
    // a long thrust burn), a single move could carry the ship straight
    // through a wall block — `polygon_segment_collide` is a static overlap
    // test, not a swept one. We split the move into chunks no bigger than
    // SHIP_RADIUS so the collide pass catches it on every substep.
    let speed = ship.vel.length();
    let max_step: f32 = SHIP_RADIUS;
    let n_sub = (((speed * dt) / max_step).ceil() as u32).max(1);
    let sub_dt = dt / n_sub as f32;
    for _ in 0..n_sub {
        ship.pos += ship.vel * sub_dt;

        // Wall collisions (multi-pass for corners). Resolved before wrap so
        // a wall on the edge still reflects; wrap is against the post-pose.
        for _ in 0..COLLISION_ITERS {
            let mut any = false;
            let verts = ship.world_vertices();
            for wall in &map.walls {
                if let Some((normal, depth)) =
                    physics::polygon_segment_collide(&verts, wall.a, wall.b)
                {
                    ship.pos += normal * depth;
                    ship.vel = physics::reflect(ship.vel, normal, WALL_RESTITUTION);
                    any = true;
                    break;
                }
            }
            if !any {
                break;
            }
        }

        if map.edge_wrap {
            ship.pos = wrap_pos(ship.pos, map.width, map.height);
        }
    }

    // 4b. Safety net: if the ship's center somehow ended up inside a wall
    // block (corner-of-corner case the substep+SAT loop can't always fix —
    // when the ship is fully embedded, polygon_segment_collide finds no
    // overlap because nothing straddles the segment plane), kick it to the
    // nearest non-wall block.
    unstick_if_embedded(ship, map);

    // 5. Cooldown decay, charge regen, predicted recoil. Bullet emission
    // itself is the server's job (see `try_fire`), but the client mirrors
    // the cooldown reset, charge deduction, and recoil so prediction stays
    // in lockstep with the server.
    ship.fire_cooldown = (ship.fire_cooldown - dt).max(0.0);
    ship.shot_charge = (ship.shot_charge + SHOT_CHARGE_RATE * dt).min(SHOT_CHARGE_MAX);
    if input.fire && ship.fire_cooldown <= 0.0 && ship.shot_charge >= SHOT_CHARGE_COST {
        ship.fire_cooldown = BULLET_COOLDOWN_SECONDS;
        ship.shot_charge -= SHOT_CHARGE_COST;
        let dir = forward(ship.angle);
        ship.vel = ship.vel - dir * (BULLET_SPEED * BULLET_MASS / ship.mass);
    }
}

/// Returns the new-bullet intent if this ship just fired this tick. Caller
/// (the world) assigns the entity id and inserts.
///
/// MUST be called AFTER `apply_ship_dynamics` for the same tick — that fn
/// resets the cooldown when fire is held, so we'd never see `cooldown == max`
/// here if we re-checked. The convention: the cooldown that's `≈ max` directly
/// after `apply_ship_dynamics` is the signal that this tick is the firing tick.
pub fn try_fire(ship: &Ship, input: &TickInput) -> Option<NewBullet> {
    // We detect "this is the firing tick" by checking that fire is held AND
    // the cooldown is at (or just below) max — apply_ship_dynamics set it
    // exactly to BULLET_COOLDOWN_SECONDS this tick if we fired.
    if !input.fire {
        return None;
    }
    // Sentinel: float-compare against the max with a tiny epsilon.
    if (ship.fire_cooldown - BULLET_COOLDOWN_SECONDS).abs() > 1e-5 {
        return None;
    }
    let dir = forward(ship.angle);
    let muzzle = ship.pos + dir * (SHIP_NOSE_OFFSET + 2.0);
    // Bullet velocity = ship vel + forward * speed. Note: ship.vel here
    // already includes the recoil from `apply_ship_dynamics`. That's fine —
    // we want the bullet's frame of reference to be the post-recoil ship.
    let bvel = ship.vel + dir * BULLET_SPEED;
    Some(NewBullet { shooter: ship.entity_id, pos: muzzle, vel: bvel })
}

/// Step a bullet by one fixed tick. Returns `true` if the bullet should be
/// despawned (lifetime expired or hit a wall). Wall test uses the unwrapped
/// trajectory so we don't false-positive against walls "in the middle" of
/// the swept path after a wrap.
pub fn step_bullet(bullet: &mut Bullet, map: &Map) -> bool {
    let dt = TICK_DT_SECONDS;
    let prev = bullet.pos;
    bullet.pos += bullet.vel * dt;
    bullet.age_seconds += dt;
    if bullet.age_seconds >= BULLET_LIFETIME_SECONDS {
        return true;
    }
    for wall in &map.walls {
        if physics::segments_intersect(prev, bullet.pos, wall.a, wall.b) {
            return true;
        }
    }
    if map.edge_wrap {
        bullet.pos = wrap_pos(bullet.pos, map.width, map.height);
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::map::{arena_map, WallSegment};

    fn empty_inputs() -> BTreeMap<EntityId, TickInput> {
        BTreeMap::new()
    }

    #[test]
    fn ship_drifts_when_no_input() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(50.0, 0.0);
        let p0 = w.ships[&id].pos;
        for _ in 0..60 {
            w.step(&empty_inputs());
        }
        let p1 = w.ships[&id].pos;
        assert!((p1.x - p0.x - 50.0).abs() < 1.0, "drifted {}", p1.x - p0.x);
        assert!((p1.y - p0.y).abs() < 0.01);
    }

    #[test]
    fn thrust_adds_velocity_in_forward_direction() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { thrust: true, ..Default::default() });
        for _ in 0..30 {
            w.step(&inputs);
        }
        let v = w.ships[&id].vel;
        assert!(v.y < -50.0, "expected negative-Y vel, got {:?}", v);
        assert!(v.x.abs() < 0.01, "no x velocity expected, got {}", v.x);
    }

    #[test]
    fn ship_bounces_off_top_wall() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 200.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(0.0, -500.0);
        for _ in 0..120 {
            w.step(&empty_inputs());
        }
        let v = w.ships[&id].vel;
        assert!(v.y > 0.0, "vel.y should reverse, got {:?}", v);
        assert!(v.y < 500.0, "vel.y should be smaller than initial");
    }

    #[test]
    fn rotating_into_wall_pushes_ship_out() {
        let mut w = World::new(arena_map());
        w.map.walls.clear();
        w.map.walls.push(WallSegment {
            a: Vec2::new(0.0, 594.0),
            b: Vec2::new(1800.0, 594.0),
        });
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { turn_right: true, ..Default::default() });
        let y0 = w.ships[&id].pos.y;
        for _ in 0..30 {
            w.step(&inputs);
        }
        let y1 = w.ships[&id].pos.y;
        assert!(y1 > y0, "ship should be pushed downward by rotation; y0={} y1={}", y0, y1);
    }

    #[test]
    fn fire_creates_bullet_with_cooldown_and_mass() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { fire: true, ..Default::default() });
        w.step(&inputs);
        assert_eq!(w.bullets.len(), 1);
        let b = w.bullets.values().next().unwrap();
        assert!(b.mass > 0.0);
        assert_eq!(b.shooter, id);
        // Cooldown blocks immediate refire.
        w.step(&inputs);
        assert_eq!(w.bullets.len(), 1);
    }

    #[test]
    fn bullet_dies_after_lifetime() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { fire: true, ..Default::default() });
        w.step(&inputs);
        assert_eq!(w.bullets.len(), 1);
        let extra_ticks = (BULLET_LIFETIME_SECONDS / TICK_DT_SECONDS) as u32 + 60;
        for _ in 0..extra_ticks {
            w.step(&empty_inputs());
        }
        assert_eq!(w.bullets.len(), 0);
    }

    #[test]
    fn bullet_kills_target_at_close_range() {
        // Shooter facing up (angle 0 → forward = (0, -1)). Target 100 units
        // above (smaller y). Bullet at 220 u/s reaches in ~30 ticks.
        let mut w = World::new(arena_map());
        let shooter = w.spawn_ship(1, Vec2::new(900.0, 700.0), 0.0);
        let target = w.spawn_ship(2, Vec2::new(900.0, 600.0), 0.0);

        let mut inputs = BTreeMap::new();
        inputs.insert(shooter, TickInput { fire: true, ..Default::default() });

        let mut killed = false;
        for _ in 0..240 {
            let events = w.step(&inputs);
            for ev in &events {
                if let crate::protocol::GameEvent::ShipDied { entity_id, .. } = ev {
                    if *entity_id == target {
                        killed = true;
                    }
                }
            }
            if killed {
                break;
            }
        }
        assert!(killed, "target should have died");
    }

    #[test]
    fn deterministic_replay() {
        let make = || {
            let mut w = World::new(arena_map());
            let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.5);
            (w, id)
        };
        let (mut a, ida) = make();
        let (mut b, idb) = make();
        for tick in 0..240 {
            let mut inputs_a = empty_inputs();
            let mut inputs_b = empty_inputs();
            let i = TickInput {
                thrust: tick % 7 != 0,
                turn_left: tick % 13 == 0,
                turn_right: tick % 11 == 0,
                fire: tick % 5 == 0,
                ..Default::default()
            };
            inputs_a.insert(ida, i);
            inputs_b.insert(idb, i);
            a.step(&inputs_a);
            b.step(&inputs_b);
        }
        assert_eq!(a.ships[&ida].pos, b.ships[&idb].pos);
        assert_eq!(a.ships[&ida].vel, b.ships[&idb].vel);
        assert_eq!(a.ships[&ida].angle, b.ships[&idb].angle);
        assert_eq!(a.bullets.len(), b.bullets.len());
    }

    #[test]
    fn client_prediction_matches_server_replay() {
        // Property: stepping a ship via apply_ship_dynamics N times produces
        // the same ship state as stepping a single-ship World N times. This
        // is what makes M3 reconciliation work — re-applying unacked inputs
        // onto a snapshotted ship gives the same answer the server got.
        let map = arena_map();
        let mut server = World::new(map.clone());
        let id = server.spawn_ship(1, Vec2::new(900.0, 600.0), 0.7);

        let mut client_ship = server.ships[&id].clone();

        let inputs: Vec<TickInput> = (0..120)
            .map(|t| TickInput {
                client_tick: t,
                thrust: t % 3 != 0,
                turn_left: t % 11 == 0,
                turn_right: t % 7 == 0,
                fire: false,
                ..Default::default()
            })
            .collect();

        for input in &inputs {
            let mut server_inputs = BTreeMap::new();
            server_inputs.insert(id, *input);
            server.step(&server_inputs);
            apply_ship_dynamics(&mut client_ship, input, &map);
        }

        assert_eq!(client_ship.pos, server.ships[&id].pos);
        assert_eq!(client_ship.vel, server.ships[&id].vel);
        assert_eq!(client_ship.angle, server.ships[&id].angle);
    }
}
