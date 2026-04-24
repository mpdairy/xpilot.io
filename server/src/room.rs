// Room — owns the authoritative World and the fixed-tick simulation loop.
//
// One tokio task per Room. Inputs and lifecycle commands arrive via an mpsc
// channel; per-player outbound channels are stored so we can push Snapshots
// (and PlayerJoined/Left events) without blocking the sim.

use std::collections::BTreeMap;
use std::time::Duration;

use shared::constants::{
    SHIP_RESPAWN_SECONDS, SNAPSHOT_RATE_HZ, TICK_DT_SECONDS, TICK_RATE_HZ,
};
use shared::entities::{EntityId, PlayerId};
use shared::map::Map;
use shared::math::Vec2;
use shared::protocol::{
    ClientTick, GameEvent, PlayerInfo, RoomId, ServerMessage, ServerTick, Snapshot, TickInput,
};
use tokio::sync::{mpsc, oneshot};
use tokio::time::{interval, MissedTickBehavior};

const ROOM_ID: RoomId = 1;
const SIM_PERIOD: Duration = Duration::from_nanos((1_000_000_000.0 / TICK_RATE_HZ as f64) as u64);
const SNAPSHOTS_EVERY_N_TICKS: u32 = TICK_RATE_HZ / SNAPSHOT_RATE_HZ;
const RESPAWN_TICKS: u32 = (SHIP_RESPAWN_SECONDS / TICK_DT_SECONDS) as u32;

pub enum RoomCommand {
    AddPlayer {
        player_id: PlayerId,
        name: String,
        outbound: mpsc::Sender<ServerMessage>,
        ack: oneshot::Sender<JoinAck>,
    },
    Input {
        player_id: PlayerId,
        input: TickInput,
    },
    RemovePlayer {
        player_id: PlayerId,
    },
}

pub struct JoinAck {
    pub joined_message: ServerMessage,
}

struct Player {
    name: String,
    entity_id: EntityId,
    last_input_tick: ClientTick,
    /// `None` for server-side bots — they have ships and scores like humans
    /// but no network destination. Every broadcast site checks before send.
    outbound: Option<mpsc::Sender<ServerMessage>>,
    kills: u32,
    deaths: u32,
    /// `Some(tick)` if dead and waiting to respawn at `tick`.
    respawn_at_tick: Option<ServerTick>,
}

impl Player {
    fn alive(&self) -> bool {
        self.respawn_at_tick.is_none()
    }
}

/// Bot ID space lives well above any human player id (lobby allocates from 1
/// upward) so there's no collision risk from this little mini-counter.
const BOT_PLAYER_ID_BASE: PlayerId = 1_000_000;

pub async fn run(map: Map, mut commands: mpsc::Receiver<RoomCommand>) {
    let mut world = shared::world::World::new(map.clone());
    let mut players: BTreeMap<PlayerId, Player> = BTreeMap::new();
    let mut latest_inputs: BTreeMap<EntityId, TickInput> = BTreeMap::new();
    /// PlayerIds of bots in this room. Brains run before each sim step.
    let mut bots: Vec<PlayerId> = Vec::new();

    // Spawn one Sid bot up front so the arena's never empty.
    let sid_pid = BOT_PLAYER_ID_BASE;
    spawn_bot(sid_pid, "Sid".into(), &mut world, &mut players);
    bots.push(sid_pid);

    let mut sim = interval(SIM_PERIOD);
    sim.set_missed_tick_behavior(MissedTickBehavior::Burst);

    let mut tick_counter: u32 = 0;

    loop {
        tokio::select! {
            cmd = commands.recv() => {
                match cmd {
                    Some(c) => handle_command(c, &mut world, &mut players, &mut latest_inputs).await,
                    None => break,
                }
            }
            _ = sim.tick() => {
                // Bot brains: each alive bot gets fresh input this tick.
                for pid in &bots {
                    let Some(p) = players.get(pid) else { continue };
                    if !p.alive() { continue }
                    let Some(ship) = world.ships.get(&p.entity_id) else { continue };
                    let input = crate::bot::sid_tick(&world, ship);
                    latest_inputs.insert(p.entity_id, input);
                }

                let bullets_before = world.bullets.len();
                let events = world.step(&latest_inputs);
                let bullets_after = world.bullets.len();
                process_events(&events, &mut world, &mut players, &mut latest_inputs);
                handle_respawns(&mut world, &mut players);

                if !events.is_empty() {
                    for ev in &events {
                        match ev {
                            GameEvent::HitScored { shooter, victim, damage } => {
                                tracing::info!(?shooter, ?victim, damage, "HIT");
                            }
                            GameEvent::ShipDied { entity_id, killer, .. } => {
                                tracing::info!(entity_id, ?killer, "DIED");
                            }
                            GameEvent::ShipSpawned { entity_id, player_id, .. } => {
                                tracing::info!(entity_id, player_id, "SPAWNED");
                            }
                            _ => {}
                        }
                        broadcast_event(ev, &players);
                    }
                }

                if bullets_after > bullets_before {
                    tracing::debug!("bullets: {} -> {}", bullets_before, bullets_after);
                }

                tick_counter = tick_counter.wrapping_add(1);
                if tick_counter % SNAPSHOTS_EVERY_N_TICKS == 0 {
                    broadcast_snapshot(&world, &players);
                }
                if tick_counter % (TICK_RATE_HZ * 5) == 0 {
                    tracing::debug!(
                        tick = world.tick,
                        ships = world.ships.len(),
                        bullets = world.bullets.len(),
                        players = players.len(),
                        "state"
                    );
                }
            }
        }
    }
}

async fn handle_command(
    cmd: RoomCommand,
    world: &mut shared::world::World,
    players: &mut BTreeMap<PlayerId, Player>,
    latest_inputs: &mut BTreeMap<EntityId, TickInput>,
) {
    match cmd {
        RoomCommand::AddPlayer { player_id, name, outbound, ack } => {
            let outbound = Some(outbound);
            let (pos, angle) = pick_safe_spawn(world);
            let entity_id = world.spawn_ship(player_id, pos, angle);
            let player_infos: Vec<PlayerInfo> = std::iter::once(PlayerInfo {
                player_id,
                name: name.clone(),
                kills: 0,
                deaths: 0,
                dead: false,
            })
            .chain(players.iter().map(|(pid, p)| PlayerInfo {
                player_id: *pid,
                name: p.name.clone(),
                kills: p.kills,
                deaths: p.deaths,
                dead: !p.alive(),
            }))
            .collect();
            let joined = ServerMessage::JoinedRoom {
                room_id: ROOM_ID,
                map: world.map.clone(),
                players: player_infos,
                your_ship_id: entity_id,
            };
            // Notify existing players of the new joiner.
            let pj = ServerMessage::PlayerJoined(PlayerInfo {
                player_id,
                name: name.clone(),
                kills: 0,
                deaths: 0,
                dead: false,
            });
            for p in players.values() {
                if let Some(out) = p.outbound.as_ref() {
                    let _ = out.try_send(pj.clone());
                }
            }
            players.insert(
                player_id,
                Player {
                    name,
                    entity_id,
                    last_input_tick: 0,
                    outbound,
                    kills: 0,
                    deaths: 0,
                    respawn_at_tick: None,
                },
            );
            let _ = ack.send(JoinAck {
                joined_message: joined,
            });
            tracing::info!(player_id, entity_id, "player joined");
        }
        RoomCommand::Input { player_id, input } => {
            if let Some(p) = players.get_mut(&player_id) {
                latest_inputs.insert(p.entity_id, input);
                if input.client_tick > p.last_input_tick {
                    p.last_input_tick = input.client_tick;
                }
            }
        }
        RoomCommand::RemovePlayer { player_id } => {
            if let Some(p) = players.remove(&player_id) {
                world.ships.remove(&p.entity_id);
                latest_inputs.remove(&p.entity_id);
                tracing::info!(player_id, "player left");
                let pl = ServerMessage::PlayerLeft(player_id);
                for other in players.values() {
                    if let Some(out) = other.outbound.as_ref() {
                        let _ = out.try_send(pl.clone());
                    }
                }
            }
        }
    }
}

fn process_events(
    events: &[GameEvent],
    _world: &mut shared::world::World,
    players: &mut BTreeMap<PlayerId, Player>,
    latest_inputs: &mut BTreeMap<EntityId, TickInput>,
) {
    for ev in events {
        match ev {
            GameEvent::ShipDied { entity_id, killer, .. } => {
                // Find the victim by entity_id (slow — small N), bump deaths,
                // schedule respawn, drop their queued input.
                let victim_pid = players
                    .iter()
                    .find(|(_, p)| p.entity_id == *entity_id)
                    .map(|(pid, _)| *pid);
                if let Some(pid) = victim_pid {
                    if let Some(p) = players.get_mut(&pid) {
                        p.deaths = p.deaths.saturating_add(1);
                        // current world.tick will be set to "now" below since
                        // step has just run. We approximate with respawn ticks
                        // remaining = RESPAWN_TICKS; the room loop checks
                        // each tick.
                        p.respawn_at_tick = Some(_world.tick.saturating_add(RESPAWN_TICKS));
                        latest_inputs.remove(&p.entity_id);
                        tracing::info!(
                            victim = pid,
                            killer = ?killer,
                            "ship died — respawn in {}s",
                            SHIP_RESPAWN_SECONDS
                        );
                    }
                }
                if let Some(killer_pid) = killer {
                    if let Some(p) = players.get_mut(killer_pid) {
                        // Self-kill (e.g., bouncing your own bullet) shouldn't
                        // count.
                        if Some(*killer_pid) != victim_pid {
                            p.kills = p.kills.saturating_add(1);
                        }
                    }
                }
            }
            GameEvent::HitScored { .. } | GameEvent::BulletFired { .. } | GameEvent::ShipSpawned { .. } => {}
        }
    }
}

fn handle_respawns(
    world: &mut shared::world::World,
    players: &mut BTreeMap<PlayerId, Player>,
) {
    let now = world.tick;
    let mut to_respawn: Vec<(PlayerId, EntityId)> = Vec::new();
    for (pid, p) in players.iter() {
        if let Some(rt) = p.respawn_at_tick {
            if now >= rt {
                to_respawn.push((*pid, p.entity_id));
            }
        }
    }
    for (pid, entity_id) in to_respawn {
        let (pos, angle) = pick_safe_spawn(world);
        world.respawn_ship(entity_id, pid, pos, angle);
        if let Some(p) = players.get_mut(&pid) {
            p.respawn_at_tick = None;
        }
        let event = GameEvent::ShipSpawned {
            entity_id,
            player_id: pid,
            pos,
        };
        broadcast_event(&event, players);
        tracing::info!(player_id = pid, entity_id, "respawned");
    }
}

fn broadcast_event(event: &GameEvent, players: &BTreeMap<PlayerId, Player>) {
    let msg = ServerMessage::Event(event.clone());
    for p in players.values() {
        if let Some(out) = p.outbound.as_ref() {
            let _ = out.try_send(msg.clone());
        }
    }
}

fn broadcast_snapshot(world: &shared::world::World, players: &BTreeMap<PlayerId, Player>) {
    if players.is_empty() {
        return;
    }
    let ships: Vec<_> = world.ships.values().cloned().collect();
    let bullets: Vec<_> = world.bullets.values().cloned().collect();
    let particles = world.particles.clone();
    let server_tick: ServerTick = world.tick;
    let player_infos: Vec<PlayerInfo> = players
        .iter()
        .map(|(pid, p)| PlayerInfo {
            player_id: *pid,
            name: p.name.clone(),
            kills: p.kills,
            deaths: p.deaths,
            dead: !p.alive(),
        })
        .collect();
    for p in players.values() {
        let Some(out) = p.outbound.as_ref() else { continue };
        let snap = Snapshot {
            server_tick,
            your_last_processed_input: p.last_input_tick,
            ships: ships.clone(),
            bullets: bullets.clone(),
            particles: particles.clone(),
            players: player_infos.clone(),
        };
        let _ = out.try_send(ServerMessage::Snapshot(snap));
    }
}

/// Spawn a bot — same setup as a human player joining, just no outbound
/// channel since a bot doesn't render anywhere. The brain (in `crate::bot`)
/// runs each tick from the room loop.
fn spawn_bot(
    player_id: PlayerId,
    name: String,
    world: &mut shared::world::World,
    players: &mut BTreeMap<PlayerId, Player>,
) {
    let (pos, angle) = pick_safe_spawn(world);
    let entity_id = world.spawn_ship(player_id, pos, angle);
    players.insert(
        player_id,
        Player {
            name,
            entity_id,
            last_input_tick: 0,
            outbound: None,
            kills: 0,
            deaths: 0,
            respawn_at_tick: None,
        },
    );
    tracing::info!(player_id, entity_id, "bot spawned");
}

/// Pick the spawn point furthest from any currently-alive ship. Falls back to
/// the first spawn for an empty world / one-spawn map.
fn pick_safe_spawn(world: &shared::world::World) -> (Vec2, f32) {
    let spawns = &world.map.spawns;
    if spawns.is_empty() {
        return (Vec2::ZERO, 0.0);
    }
    let mut best = &spawns[0];
    let mut best_min_dist_sq = -1.0_f32;
    for s in spawns {
        let mut min_dist_sq = f32::MAX;
        for ship in world.ships.values() {
            let d = (ship.pos - s.pos).length_squared();
            if d < min_dist_sq {
                min_dist_sq = d;
            }
        }
        // Empty world → all spawns are equally far (infinity); first wins.
        if min_dist_sq > best_min_dist_sq {
            best_min_dist_sq = min_dist_sq;
            best = s;
        }
    }
    (best.pos, best.angle)
}
