// Room — owns the authoritative World and the fixed-tick simulation loop.
//
// One tokio task per Room. Inputs and lifecycle commands arrive via an mpsc
// channel; per-player outbound channels are stored so we can push Snapshots
// (and PlayerJoined/Left events) without blocking the sim.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use shared::constants::{
    AOI_MARGIN, ROOM_IDLE_TIMEOUT_SECONDS, ROOM_PLAYER_CAP, SHIP_RESPAWN_SECONDS,
    SNAPSHOT_RATE_HZ, TICK_DT_SECONDS, TICK_RATE_HZ,
};
use shared::entities::{EntityId, PlayerId};
use shared::map::Map;
use shared::math::{point_in_rect_torus, Vec2};
use shared::protocol::{
    ClientTick, GameEvent, PlayerInfo, RoomId, ServerMessage, ServerTick, Snapshot, TickInput,
};
use tokio::sync::{mpsc, oneshot};
use tokio::time::{interval, MissedTickBehavior};

use crate::lobby::RoomRegistry;

const SIM_PERIOD: Duration = Duration::from_nanos((1_000_000_000.0 / TICK_RATE_HZ as f64) as u64);
const SNAPSHOTS_EVERY_N_TICKS: u32 = TICK_RATE_HZ / SNAPSHOT_RATE_HZ;
const RESPAWN_TICKS: u32 = (SHIP_RESPAWN_SECONDS / TICK_DT_SECONDS) as u32;
const IDLE_DESTROY: Duration = Duration::from_secs(ROOM_IDLE_TIMEOUT_SECONDS as u64);

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
    /// Per-player AOI hint. Sticky: the latest value wins and is used for
    /// every snapshot until replaced. `None` half-extents means "no filter".
    Viewport {
        player_id: PlayerId,
        half_width: f32,
        half_height: f32,
    },
    /// Free-text chat. Server trims + length-caps then rebroadcasts as
    /// `GameEvent::Chat`. Empty strings are dropped silently.
    Chat {
        player_id: PlayerId,
        text: String,
    },
    RemovePlayer {
        player_id: PlayerId,
    },
}

pub enum JoinResult {
    Joined(ServerMessage),
    Full,
}

pub struct JoinAck {
    pub result: JoinResult,
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
    /// Latest reported viewport from the client. `None` until the first
    /// `ClientMessage::Viewport` arrives — snapshots are unfiltered until
    /// then so a slow client never sees an empty world.
    viewport: Option<(f32, f32)>,
    /// Last position of this player's ship while alive — used as the AOI
    /// center while the player is dead/respawning so they keep seeing the
    /// area where they died instead of an empty (0,0) corner.
    last_alive_pos: Option<Vec2>,
    /// Server-spawned bot. Surfaced on PlayerInfo so clients can decorate
    /// the name distinctly from humans.
    is_bot: bool,
    /// Per-bot AI scratch space (evade timers, cached flee targets, …).
    /// `Default` for humans; populated/used by `bot::tick_for`.
    bot_state: crate::bot::BotState,
}

impl Player {
    fn alive(&self) -> bool {
        self.respawn_at_tick.is_none()
    }
}

/// Bot ID space lives well above any human player id (lobby allocates from 1
/// upward) so there's no collision risk. `+room_id*N` keeps Sid IDs unique
/// across rooms — otherwise two rooms' Sids would clash if both lived in any
/// global structure (none today, but cheap insurance).
const BOT_PLAYER_ID_BASE: PlayerId = 1_000_000;
const BOT_IDS_PER_ROOM: PlayerId = 100;
/// Names lifted from classic xpilot.org screenshots — flavour only. Sliced
/// to `bot_count` at room start; an enclosing call clamps to `<= 8`.
/// First five are the personality bots (Sid + the named-personality
/// variants) so even a small-bot-count room shows off the variety; the
/// rest fall back to Sid via `bot::tick_for`.
const BOT_NAMES: [&str; 8] = [
    "Sid", "Reaper", "Cobra", "Vega", "Wimpy", "Slugger", "Spike", "Diesel",
];

pub async fn run(
    room_id: RoomId,
    map: Map,
    mut commands: mpsc::Receiver<RoomCommand>,
    human_count: Arc<AtomicUsize>,
    registry: RoomRegistry,
    bot_count: u32,
) {
    let mut world = shared::world::World::new(map.clone());
    let mut players: BTreeMap<PlayerId, Player> = BTreeMap::new();
    let mut latest_inputs: BTreeMap<EntityId, TickInput> = BTreeMap::new();
    // PlayerIds of bots in this room. Brains run before each sim step.
    let mut bots: Vec<PlayerId> = Vec::new();

    // Pre-populate with the requested number of bots. All share the same
    // brain (`bot::sid_tick`) — names are just for HUD distinction.
    let bot_id_base = BOT_PLAYER_ID_BASE + room_id as PlayerId * BOT_IDS_PER_ROOM;
    let n = (bot_count as usize).min(BOT_NAMES.len());
    for (i, name) in BOT_NAMES.iter().take(n).enumerate() {
        let pid = bot_id_base + i as PlayerId;
        spawn_bot(pid, (*name).into(), &mut world, &mut players);
        bots.push(pid);
    }

    let mut sim = interval(SIM_PERIOD);
    // Real-time game state should not replay missed ticks in a burst after the
    // process is descheduled. A burst catches sim time up, but it also emits a
    // clump of snapshots that looks like visual strobe after a VM/host pause.
    sim.set_missed_tick_behavior(MissedTickBehavior::Skip);

    let mut tick_counter: u32 = 0;
    // Set on construction and reset whenever humans drops to zero. Cleared
    // when a human joins. Once it's been Some(t) longer than IDLE_DESTROY,
    // the room removes itself from the registry and exits.
    let mut empty_since: Option<Instant> = Some(Instant::now());

    // Tick timing telemetry — lets us tell server-side stutter (slow sim or
    // CPU starvation by the host) apart from network jitter. Rolled up every
    // 5 s alongside the existing state log. Why two metrics:
    //   work_us: how long the sim+broadcast took. >TICK_DT means we can't keep up.
    //   gap_us:  wall time between consecutive ticks. >>SIM_PERIOD with low work
    //            implies the OS didn't schedule us — i.e. CPU steal from a noisy
    //            VM neighbour or a tokio task hogging the runtime.
    let mut last_tick_at: Option<Instant> = None;
    let mut max_work_us: u64 = 0;
    let mut max_gap_us: u64 = 0;
    let mut slow_ticks: u32 = 0;
    // Lateness counters: a single late tick is invisible; a *cluster* is stutter.
    // Two thresholds so we can tell "tokio jitter" (~25 ms occasionally) apart
    // from "missed a whole tick" (>=33 ms — gap big enough to skip a frame).
    let mut late_ticks: u32 = 0; // >= 1.5 * period (~25 ms)
    let mut very_late_ticks: u32 = 0; // >= 2.0 * period (~33 ms)
    let slow_threshold = SIM_PERIOD; // anything >= the budget means we slipped
    let late_threshold_us = (SIM_PERIOD.as_micros() as u64 * 3) / 2;
    let very_late_threshold_us = SIM_PERIOD.as_micros() as u64 * 2;

    loop {
        // Idle-shutdown check. Done at the top so a room with zero humans
        // and no incoming commands will wake up via the sim tick and exit.
        if let Some(t) = empty_since {
            if t.elapsed() >= IDLE_DESTROY {
                registry.write().await.remove(&room_id);
                tracing::info!(room_id, "destroyed (idle)");
                break;
            }
        }

        tokio::select! {
            cmd = commands.recv() => {
                match cmd {
                    Some(c) => {
                        let was_empty = human_count.load(Ordering::Relaxed) == 0;
                        handle_command(
                            c,
                            room_id,
                            &mut world,
                            &mut players,
                            &mut latest_inputs,
                            &human_count,
                        ).await;
                        let now_empty = human_count.load(Ordering::Relaxed) == 0;
                        if !now_empty {
                            empty_since = None;
                        } else if !was_empty {
                            empty_since = Some(Instant::now());
                        }
                    }
                    None => break,
                }
            }
            _ = sim.tick() => {
                let tick_start = Instant::now();
                let gap_us = last_tick_at
                    .map(|t| tick_start.saturating_duration_since(t).as_micros() as u64)
                    .unwrap_or(0);
                last_tick_at = Some(tick_start);
                if gap_us > max_gap_us { max_gap_us = gap_us; }
                if gap_us >= very_late_threshold_us { very_late_ticks += 1; }
                else if gap_us >= late_threshold_us { late_ticks += 1; }

                // Bot brains: each alive bot gets fresh input this tick.
                for pid in &bots {
                    let Some(p) = players.get_mut(pid) else { continue };
                    if !p.alive() { continue }
                    let Some(ship) = world.ships.get(&p.entity_id) else { continue };
                    let input = crate::bot::tick_for(&p.name, &world, ship, &mut p.bot_state);
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
                    broadcast_snapshot(&world, &mut players);
                }

                let work_us = tick_start.elapsed().as_micros() as u64;
                if work_us > max_work_us { max_work_us = work_us; }
                if work_us as u128 >= slow_threshold.as_micros() { slow_ticks += 1; }

                if tick_counter % (TICK_RATE_HZ * 5) == 0 {
                    tracing::info!(
                        room_id,
                        tick = world.tick,
                        ships = world.ships.len(),
                        bullets = world.bullets.len(),
                        players = players.len(),
                        max_work_us,
                        max_gap_us,
                        slow_ticks,
                        late_ticks,
                        very_late_ticks,
                        "tick_stats"
                    );
                    max_work_us = 0;
                    max_gap_us = 0;
                    slow_ticks = 0;
                    late_ticks = 0;
                    very_late_ticks = 0;
                }
            }
        }
    }
}

async fn handle_command(
    cmd: RoomCommand,
    room_id: RoomId,
    world: &mut shared::world::World,
    players: &mut BTreeMap<PlayerId, Player>,
    latest_inputs: &mut BTreeMap<EntityId, TickInput>,
    human_count: &Arc<AtomicUsize>,
) {
    match cmd {
        RoomCommand::AddPlayer {
            player_id,
            name,
            outbound,
            ack,
        } => {
            // Cap is humans-only; bots don't compete for slots.
            if human_count.load(Ordering::Relaxed) >= ROOM_PLAYER_CAP {
                let _ = ack.send(JoinAck {
                    result: JoinResult::Full,
                });
                tracing::info!(player_id, room_id, "join rejected — room full");
                return;
            }
            let outbound = Some(outbound);
            let (pos, angle) = pick_safe_spawn(world);
            let entity_id = world.spawn_ship(player_id, pos, angle);
            let player_infos: Vec<PlayerInfo> = std::iter::once(PlayerInfo {
                player_id,
                name: name.clone(),
                kills: 0,
                deaths: 0,
                dead: false,
                is_bot: false,
            })
            .chain(players.iter().map(|(pid, p)| PlayerInfo {
                player_id: *pid,
                name: p.name.clone(),
                kills: p.kills,
                deaths: p.deaths,
                dead: !p.alive(),
                is_bot: p.is_bot,
            }))
            .collect();
            let joined = ServerMessage::JoinedRoom {
                room_id,
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
                is_bot: false,
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
                    viewport: None,
                    last_alive_pos: None,
                    is_bot: false,
                    bot_state: Default::default(),
                },
            );
            human_count.fetch_add(1, Ordering::Relaxed);
            let _ = ack.send(JoinAck {
                result: JoinResult::Joined(joined),
            });
            tracing::info!(player_id, entity_id, room_id, "player joined");
        }
        RoomCommand::Input { player_id, input } => {
            if let Some(p) = players.get_mut(&player_id) {
                latest_inputs.insert(p.entity_id, input);
                if input.client_tick > p.last_input_tick {
                    p.last_input_tick = input.client_tick;
                }
            }
        }
        RoomCommand::Viewport {
            player_id,
            half_width,
            half_height,
        } => {
            if let Some(p) = players.get_mut(&player_id) {
                // Sanity-cap to world dim/2 — anything larger filters nothing
                // anyway, and saves us from a buggy client claiming millions.
                // Floor at 1.0 so a zero/negative value can't accidentally
                // collapse the AOI to a point.
                let hw = half_width.clamp(1.0, world.map.width * 0.5);
                let hh = half_height.clamp(1.0, world.map.height * 0.5);
                p.viewport = Some((hw, hh));
            }
        }
        RoomCommand::Chat { player_id, text } => {
            // Reject if not in the room (stale message after Leave) or if the
            // body is empty after trim. 200-char cap so a misbehaving client
            // can't dump megabytes — chars(), not bytes(), so multi-byte
            // characters count once.
            if !players.contains_key(&player_id) {
                return;
            }
            let trimmed = text.trim();
            if trimmed.is_empty() {
                return;
            }
            const CHAT_MAX_CHARS: usize = 200;
            let bounded: String = trimmed.chars().take(CHAT_MAX_CHARS).collect();
            let event = GameEvent::Chat {
                player_id,
                text: bounded,
            };
            broadcast_event(&event, players);
        }
        RoomCommand::RemovePlayer { player_id } => {
            if let Some(p) = players.remove(&player_id) {
                world.ships.remove(&p.entity_id);
                latest_inputs.remove(&p.entity_id);
                human_count.fetch_sub(1, Ordering::Relaxed);
                tracing::info!(player_id, room_id, "player left");
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
            GameEvent::ShipDied {
                entity_id, killer, ..
            } => {
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
            GameEvent::HitScored { .. }
            | GameEvent::BulletFired { .. }
            | GameEvent::ShipSpawned { .. }
            | GameEvent::Chat { .. } => {}
        }
    }
}

fn handle_respawns(world: &mut shared::world::World, players: &mut BTreeMap<PlayerId, Player>) {
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

fn broadcast_snapshot(world: &shared::world::World, players: &mut BTreeMap<PlayerId, Player>) {
    if players.is_empty() {
        return;
    }
    // Ships are *always* sent in full — clients need them for the radar (out-
    // of-view dots) and to anchor thruster particles for enemies thrusting
    // onto the edge of the screen.
    let ships: Vec<_> = world.ships.values().cloned().collect();
    let dead_cannons: Vec<(u32, u32)> = world
        .cannons
        .iter()
        .filter(|(_, c)| !c.alive)
        .map(|(cell, _)| *cell)
        .collect();
    let server_tick: ServerTick = world.tick;
    let player_infos: Vec<PlayerInfo> = players
        .iter()
        .map(|(pid, p)| PlayerInfo {
            player_id: *pid,
            name: p.name.clone(),
            kills: p.kills,
            deaths: p.deaths,
            dead: !p.alive(),
            is_bot: p.is_bot,
        })
        .collect();
    let world_w = world.map.width;
    let world_h = world.map.height;
    let edge_wrap = world.map.edge_wrap;
    for p in players.values_mut() {
        // Refresh last_alive_pos every tick the player has a live ship; used
        // as the AOI anchor while they're respawning so they keep seeing the
        // area where they died.
        if let Some(ship) = world.ships.get(&p.entity_id) {
            p.last_alive_pos = Some(ship.pos);
        }
        let Some(out) = p.outbound.as_ref() else {
            continue;
        };
        // Build the per-player snapshot. AOI center is current ship pos if
        // alive, else last-alive pos. If neither exists (brand-new joiner
        // mid-tick) or no viewport reported yet, send unfiltered — better an
        // oversized snapshot than an empty one.
        let center = world
            .ships
            .get(&p.entity_id)
            .map(|s| s.pos)
            .or(p.last_alive_pos);
        let (bullets, particles) = match (center, p.viewport) {
            (Some(c), Some((hw, hh))) => {
                let hw = hw + AOI_MARGIN;
                let hh = hh + AOI_MARGIN;
                let in_view = |pos: Vec2| {
                    point_in_rect_torus(pos, c, hw, hh, world_w, world_h, edge_wrap)
                };
                let bullets: Vec<_> = world
                    .bullets
                    .values()
                    .filter(|b| in_view(b.pos))
                    .cloned()
                    .collect();
                let particles: Vec<_> = world
                    .particles
                    .iter()
                    .filter(|pt| in_view(pt.pos))
                    .cloned()
                    .collect();
                (bullets, particles)
            }
            _ => (
                world.bullets.values().cloned().collect(),
                world.particles.clone(),
            ),
        };
        let snap = Snapshot {
            server_tick,
            your_last_processed_input: p.last_input_tick,
            ships: ships.clone(),
            bullets,
            particles,
            players: player_infos.clone(),
            dead_cannons: dead_cannons.clone(),
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
            viewport: None,
            last_alive_pos: None,
            is_bot: true,
            bot_state: Default::default(),
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
