# xpilot.io (v2) — Multiplayer Browser XPilot in Rust + WASM

## What this is

A browser-playable, server-authoritative, real-time multiplayer space combat game inspired by XPilot and the original Elm-based [xpilot.io](http://xpilot.io). Ships are triangles with mass, thrust, inertia, and limited-velocity bullets. No asteroids. Shoot other ships, blow them around with your thruster wash, don't die.

Secondary goal: this is a teaching platform for game AI. Bots connect over the same protocol as human clients, which means the observation/action interface is a first-class API, not an afterthought.

## Non-goals (for now)

- 3D anything
- Persistence / accounts / matchmaking service / ranked play
- Anti-cheat beyond "server is authoritative so clients can't lie about their state"
- Pretty graphics — canvas 2D triangles and particles is the target aesthetic
- Mobile / touch controls
- Spectator mode (can come later, trivially, since server already has all state)

## Architecture at a glance

```
┌──────────────────┐   WebRTC DataChannel   ┌──────────────────┐
│  Browser client  │ ◄──── unreliable ────► │   Game server    │
│  (Rust → WASM)   │       unordered        │   (Rust native)  │
│                  │                        │                  │
│                  │   WebSocket (signal)   │                  │
│                  │ ◄──── short-lived ───► │                  │
│ • Render (canvas)│   (SDP offer/answer,   │ • Auth sim @60Hz │
│ • Input capture  │    ICE candidates)     │ • Snapshots @30Hz│
│ • Prediction     │                        │ • Lag comp hits  │
│ • Reconciliation │                        │ • Room management│
│ • Interpolation  │                        │ • Map loading    │
└──────────────────┘                        └──────────────────┘
         ▲                                            ▲
         │              same protocol                 │
         │         (WS for bots, RTC optional)        │
┌──────────────────┐                          ┌──────────────────┐
│  External bot    │ ◄──────── same ────────► │  (same server)   │
│  (any language)  │         protocol         │                  │
└──────────────────┘                          └──────────────────┘
```

Bots are indistinguishable from human clients at the protocol layer. A bot is a program that opens a connection, sends `ClientMessage`s, and receives `ServerMessage`s. Bots may use WebSocket instead of WebRTC for simplicity — the server accepts both.

## Workspace layout

Single Cargo workspace:

```
xpilot/
├── Cargo.toml                  # workspace
├── shared/                     # compiles to native AND wasm
│   ├── src/
│   │   ├── lib.rs
│   │   ├── physics.rs          # deterministic sim step
│   │   ├── entities.rs         # Ship, Bullet (server-side particle = force cone)
│   │   ├── map.rs              # Map struct, wall segments, spawn points, loader
│   │   ├── protocol.rs         # ClientMessage, ServerMessage, Snapshot, Input
│   │   ├── constants.rs        # TICK_RATE_HZ, SHIP_MASS, THRUST_FORCE, etc.
│   │   └── math.rs             # Vec2, rotations, collision helpers
│   └── Cargo.toml
├── server/                     # native binary
│   ├── src/
│   │   ├── main.rs             # tokio + axum entry
│   │   ├── room.rs             # one game room; fixed-tick sim loop
│   │   ├── lobby.rs            # room list, join/leave, create
│   │   ├── connection.rs       # per-socket task, parses ClientMessage
│   │   ├── transport/
│   │   │   ├── mod.rs          # Transport trait
│   │   │   ├── websocket.rs    # WebSocket impl (simple, for bots & fallback)
│   │   │   ├── webrtc.rs       # WebRTC DataChannel impl via webrtc-rs
│   │   │   └── signaling.rs    # WS endpoint that negotiates RTC sessions
│   │   ├── laggomp.rs          # lag compensation: rewind + re-test hits
│   │   └── snapshot.rs         # build per-client snapshots, delta encoding (later)
│   └── Cargo.toml
├── client/                     # wasm binary
│   ├── src/
│   │   ├── lib.rs              # wasm entry, main loop via requestAnimationFrame
│   │   ├── render.rs           # canvas 2D draw: ships, bullets, particles, walls
│   │   ├── input.rs            # keyboard capture, input buffer
│   │   ├── prediction.rs       # run local ship sim ahead of server
│   │   ├── reconcile.rs        # on server snapshot: rewind + replay
│   │   ├── interpolate.rs      # render other entities ~100ms in the past
│   │   ├── transport/
│   │   │   ├── mod.rs          # Transport trait (mirrors server)
│   │   │   ├── websocket.rs    # fallback / bot-friendly
│   │   │   └── webrtc.rs       # RTCPeerConnection via web-sys
│   │   └── particles.rs        # client-only cosmetic thruster sparks
│   ├── index.html
│   └── Cargo.toml
├── bots/
│   ├── rust-reference/         # idiomatic Rust bot using shared/ types, over WS
│   ├── python-reference/       # uses websockets + json debug protocol
│   └── js-reference/           # browser-runnable bot, no build step
├── maps/
│   ├── empty.json              # no walls, just a bounded arena
│   ├── arena.json              # a few central walls
│   └── og-xpilot.json          # eventual: closer to classic xpilot map
└── CLAUDE.md                   # this file
```

## Core game constants (starting values, tune later)

In `shared/src/constants.rs`:

```rust
pub const TICK_RATE_HZ: u32 = 60;
pub const TICK_DT_SECONDS: f32 = 1.0 / 60.0;
pub const SNAPSHOT_RATE_HZ: u32 = 30;

pub const WORLD_WIDTH: f32 = 4000.0;   // in game units
pub const WORLD_HEIGHT: f32 = 3000.0;

// Ship
pub const SHIP_MASS: f32 = 1.0;
pub const SHIP_RADIUS: f32 = 12.0;            // collision circle
pub const SHIP_TURN_RATE: f32 = 3.5;          // rad/sec
pub const SHIP_THRUST_ACCEL: f32 = 200.0;     // units/sec^2 when thrusting
pub const SHIP_LINEAR_DAMPING: f32 = 0.0;     // space = no drag; tune if feels bad
pub const SHIP_MAX_HP: u32 = 100;
pub const SHIP_RESPAWN_SECONDS: f32 = 2.0;

// Bullet
pub const BULLET_SPEED: f32 = 600.0;          // units/sec, added to ship velocity
pub const BULLET_LIFETIME_SECONDS: f32 = 2.5;
pub const BULLET_DAMAGE: u32 = 20;
pub const BULLET_MASS: f32 = 0.01;            // for recoil calc on shooter
pub const BULLET_COOLDOWN_SECONDS: f32 = 0.15;

// Thruster wash (force cone behind a thrusting ship)
pub const WASH_CONE_LENGTH: f32 = 140.0;
pub const WASH_CONE_HALF_ANGLE_RAD: f32 = 0.35;
pub const WASH_FORCE_AT_MOUTH: f32 = 180.0;   // falls off linearly to 0 at tip
```

These are starting points. Feel-test, then tune.

## Physics & determinism

### Determinism is a hard requirement

Client-side prediction only works if client and server produce identical state given identical inputs. This means:

1. **Fixed timestep**. Server runs at exactly `TICK_DT_SECONDS` per tick. Client prediction also steps at exactly `TICK_DT_SECONDS`. No variable-dt integration.
2. **Deterministic iteration order**. Never iterate a `HashMap` in the sim. Use `BTreeMap<EntityId, _>` or `Vec` with stable sort. Entity IDs are `u32` assigned by the server, monotonically increasing per room.
3. **No ambient randomness in the sim**. If randomness is needed (e.g. spawn point selection), seed a `SmallRng` from a room-level seed + tick number. Visual particles for thruster effects are client-only and may use any RNG.
4. **f32 is acceptable** for this game's precision needs, but transcendentals (`sin`, `cos`) must use the `libm` crate on both sides, not `std::f32`, to guarantee bit-exact results across x86 and wasm32.

### Integration

Semi-implicit Euler (symplectic Euler): update velocity from acceleration first, then update position from the new velocity. Simple, stable at 60 Hz for these forces, deterministic.

```rust
for ship in ships {
    ship.vel += ship.acc * dt;
    ship.pos += ship.vel * dt;
    // angular similarly
}
```

### Collision model

- **Ship vs wall**: circle (ship) vs line segment (wall). Reflect velocity with some energy loss (coefficient of restitution ~0.5). No bounce damage in v1.
- **Ship vs ship**: circle vs circle. Elastic-ish collision, apply impulse, split damage between both.
- **Bullet vs wall**: point vs line segment. Bullet dies.
- **Bullet vs ship**: point vs circle. Bullet dies, ship takes damage. Shooter does not collide with own bullets for the first few ticks after firing (prevents self-hits from the muzzle position being inside own radius).
- **Ship vs thruster wash**: point (ship center) vs cone. Apply force toward the far end of the cone, falling off linearly with distance.

### Thruster wash — the important compromise

Original Elm version: individual particles pushed other ships. Cute but miserable to network (hundreds of particles per ship, all physics interactions). Compromise:

- **Server-side**: while a ship's thrust input is held, it emits a force cone behind it each tick. Other ships/bullets whose positions lie inside the cone receive force this tick. No particle entities on the server.
- **Client-side**: render particles as pure eye candy, spawning randomly from the thruster each frame, purely cosmetic, do not affect sim. They are not networked.

This keeps the "blow enemies around with your jet exhaust" feature intact while making netcode sane.

## Network transport

### Goal: UDP-like semantics in the browser

Gameplay traffic must be unreliable + unordered. A dropped snapshot should NOT stall the next snapshot. WebSocket (TCP) has head-of-line blocking that causes visible stutter under packet loss, which is why we're going with WebRTC DataChannels.

### The `Transport` trait

Both client and server abstract the wire behind:

```rust
pub trait Transport {
    fn send(&mut self, msg: &[u8], reliability: Reliability);
    fn poll_recv(&mut self) -> Option<Vec<u8>>;
    fn is_open(&self) -> bool;
    fn close(&mut self);
}

pub enum Reliability {
    Unreliable,     // per-tick snapshots, inputs
    Reliable,       // GameEvents, JoinRoom, Hello
}
```

Two implementations per side:

- **`WebSocketTransport`**: everything is "reliable ordered" (TCP gives us no choice). Used by bots in other languages, by dev/debug tooling, and as fallback if WebRTC fails to connect (corporate firewalls, picky NATs).
- **`WebRtcTransport`**: DataChannel configured with `ordered: false, maxRetransmits: 0` for Unreliable messages. A second DataChannel on the same peer connection configured `ordered: true` for Reliable messages. One `PeerConnection` with two channels is lighter than two peer connections.

### WebRTC connection flow

1. Client opens a WebSocket to `wss://server/signal`. This WS is *only* for signaling.
2. Client sends `Hello { client_kind, ... }` over the WS. If `client_kind` is `Bot` and the bot hasn't indicated WebRTC support, the server keeps them on WebSocket and skips signaling. For `Human` clients, proceed.
3. Client creates `RTCPeerConnection`, creates two DataChannels (`unreliable`, `reliable`), creates an SDP offer, sends it over the signaling WS.
4. Server (using `webrtc-rs`) creates its own `RTCPeerConnection`, applies the remote offer, generates an answer, sends answer back over WS. ICE candidates trickle both directions on the WS.
5. Once both DataChannels open, all further `ClientMessage`/`ServerMessage` traffic flows over the DataChannels. The signaling WS can close.
6. If DataChannel setup fails or times out (say 5 seconds), client falls back to using the WS for gameplay. Log this so we know who's falling back in practice.

### STUN / TURN

For LAN testing: no STUN needed. For internet play: need at minimum a public STUN server (use `stun:stun.l.google.com:19302` for dev). For NATs that STUN can't punch through: need a TURN relay, which is stateful and bandwidth-heavy. Start with STUN-only, add TURN when we have real users who can't connect. `coturn` is the standard TURN implementation; can self-host.

### Codec

Two codecs, feature-flagged, for the same message types:

- **Binary**: `postcard` for compact wire format. Default in release.
- **JSON**: `serde_json` for debugging. Default in debug, or opt-in via query string `?codec=json` on the signaling URL.

All messages `#[derive(Serialize, Deserialize)]`.

### Message types

In `shared/src/protocol.rs`:

```rust
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ClientMessage {
    /// First message after connect.
    Hello { name: String, client_kind: ClientKind, supports_webrtc: bool },
    /// Create or join a room.
    JoinRoom { room_id: Option<RoomId>, map_name: Option<String> },
    /// Per-tick input. Sent unreliably.
    Input(TickInput),
    Leave,
    /// WebRTC signaling messages (only over WS).
    RtcOffer { sdp: String },
    RtcAnswer { sdp: String },
    RtcIceCandidate { candidate: String, sdp_mid: Option<String>, sdp_m_line_index: Option<u16> },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ServerMessage {
    Welcome { player_id: PlayerId, server_tick: u32 },
    JoinedRoom { room_id: RoomId, map: Map, players: Vec<PlayerInfo>, your_ship_id: EntityId },
    PlayerJoined(PlayerInfo),
    PlayerLeft(PlayerId),
    /// Full snapshot at SNAPSHOT_RATE_HZ. Sent unreliably.
    Snapshot(Snapshot),
    /// Events that shouldn't be missed. Sent reliably.
    Event(GameEvent),
    Error { message: String },
    /// WebRTC signaling.
    RtcOffer { sdp: String },
    RtcAnswer { sdp: String },
    RtcIceCandidate { candidate: String, sdp_mid: Option<String>, sdp_m_line_index: Option<u16> },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ClientKind {
    Human,
    Bot { author: String },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TickInput {
    pub client_tick: u32,       // client's local sim tick when this was produced
    pub turn_left: bool,
    pub turn_right: bool,
    pub thrust: bool,
    pub fire: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Snapshot {
    pub server_tick: u32,
    /// Last client tick the server has processed for *you*. Lets you discard
    /// already-acked inputs and only re-predict unacked ones.
    pub your_last_processed_input: u32,
    pub ships: Vec<ShipState>,
    pub bullets: Vec<BulletState>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum GameEvent {
    ShipSpawned { entity_id: EntityId, player_id: PlayerId, pos: Vec2 },
    ShipDied { entity_id: EntityId, killer: Option<PlayerId> },
    BulletFired { entity_id: EntityId, shooter: EntityId, pos: Vec2, vel: Vec2 },
    HitScored { shooter: PlayerId, victim: PlayerId, damage: u32 },
}
```

### Sending rules

- `ClientMessage::Input` → Unreliable. Even if dropped, next tick's input will arrive and the server can extrapolate one tick.
- `ServerMessage::Snapshot` → Unreliable. Dropped snapshots are simply skipped; the next arrives 33 ms later.
- `ServerMessage::Event`, `Welcome`, `JoinedRoom`, `PlayerJoined/Left`, `Error` → Reliable.
- Signaling messages only flow over the WS pre-DataChannel.

### Tick / bandwidth budget

- Client sends `Input` every tick (60 Hz, ~16 bytes = ~1 KB/s up).
- Server sends `Snapshot` at 30 Hz; 8 players + ~50 bullets ≈ 2–4 KB per snapshot = ~60–120 KB/s down per client. Delta-encode later if tight.
- `Event`s small and infrequent.

## Client-side prediction + reconciliation

Flow for the local player's ship:

1. Each tick, capture input, append to ring buffer `unacked_inputs: VecDeque<TickInput>`.
2. Send input to server immediately (Unreliable).
3. Run shared physics step locally on the local ship using this input — this is the "predicted" state the player sees.
4. When a `Snapshot` arrives with `your_last_processed_input = N`:
   - Drop entries in `unacked_inputs` with `client_tick <= N`.
   - Snap local ship to snapshot's state.
   - Re-apply every remaining unacked input in order, stepping physics each time.
   - Resulting state is the new predicted "now" — should match on-screen, unless server disagreed (hit, wall bounce).
5. Other players' ships and bullets: **do not predict**. Render ~`INTERP_DELAY_MS` (100 ms) behind real time, interpolating between the two most recent snapshots straddling that time. Jitter-free, hides packet loss automatically.

Prediction errors show as a "pop" when server disagrees. If visible in practice, smooth with 100 ms lerp between old predicted state and reconciled state. Not a v1 problem — ship naive first.

## Lag compensation (hit detection)

When a client fires at server tick T but their view is ~100 ms behind real-time plus ~RTT/2 upstream latency, server must rewind to test the hit against where the victim *was* from shooter's perspective.

v1 approach:

1. Server maintains a ring buffer of ~1 second of entity positions.
2. When a client sends `fire=true` in a `TickInput`, server computes `effective_tick = server_tick - estimated_one_way_latency_ticks`.
3. Spawn bullet at shooter's position as of `effective_tick` (rewound), with velocity added to shooter's velocity at that tick.
4. From then on the bullet lives in real time — only the spawn is rewound.

Simpler than full rewind-and-retest. Works well for projectiles because the bullet has to travel and hit in real time anyway.

## Maps

### Format

JSON (hand-editable; binary later if we add an editor).

```json
{
  "name": "arena",
  "width": 4000,
  "height": 3000,
  "walls": [
    { "a": [500, 500], "b": [500, 2500] },
    { "a": [500, 2500], "b": [3500, 2500] }
  ],
  "spawns": [
    { "pos": [1000, 1500], "angle": 0.0 },
    { "pos": [3000, 1500], "angle": 3.14159 }
  ],
  "metadata": { "author": "donivan", "recommended_players": [2, 8] }
}
```

Line segments are the wall primitive. Closed shapes are loops of segments. Spawn points chosen at spawn time preferring ones farthest from enemies.

### Loading

Server loads maps from `./maps/` on startup, hot-reloadable in dev. Map sent to client in `JoinedRoom` so client can render walls.

## Rooms & lobby

- Server holds a `Lobby` with list of `Room`s.
- Each room is a `tokio::task` running a fixed-tick loop with `tokio::time::interval`.
- Room has a bounded mpsc channel for incoming `(PlayerId, ClientMessage)` pairs.
- Per-player outbound mpsc (bounded, drop-oldest on overflow) so slow clients don't block others.
- v1: room created on first join, destroyed when empty for 30 seconds.
- v1 room cap: 8 players.
- Room selects map by name from its `JoinRoom` message; defaults to `arena.json`.

## Bot API

Bots use the **exact same protocol**. Differences are conventional, not structural:

- Bots send `Hello { client_kind: ClientKind::Bot { author: "..." }, supports_webrtc: false, ... }` so server + spectators can label them and skip RTC negotiation.
- Bots encouraged to use JSON codec for ease of implementation in other languages (`?codec=json`).
- Bots stay on WebSocket — reliable-ordered is fine for them, and it's way simpler than standing up WebRTC in Python/JS.

### Reference bots

- `bots/rust-reference/`: uses `shared/` directly, real types. Simple "fly toward nearest enemy, shoot when aimed" bot.
- `bots/python-reference/`: `websockets` + `json`. Same behavior. Well-commented, what students copy-paste from.
- `bots/js-reference/`: runs in Node or browser. Useful for demoing alongside a playing human.

### Observation for bots

`Snapshot` already contains everything: all ships, all bullets, the map (from `JoinedRoom`). For partial observability later, add `visibility_radius` room setting and the server filters snapshots per-player. v1: full observability.

## Dev workflow

### Build / run

- Server: `cargo run -p server --release` — binds to `0.0.0.0:8080`.
- Client: `trunk serve client/index.html` — serves at `http://localhost:3000`, hot-reloads.
- Shared code changes trigger rebuilds of both.

### Testing

- `shared/`: unit tests for physics step (property-based via `proptest` for determinism — same inputs produce same outputs bit-exact across platforms).
- `server/`: integration test that spins up a server, connects two bot clients over WS, runs 60 ticks, asserts snapshots match expected.
- `client/`: `wasm-bindgen-test` for non-render logic (prediction, reconciliation, interpolation buffers). Rendering tested by eyeball.

### Determinism CI check

Critical: a CI job that runs a recorded input sequence through the shared sim on both x86 and wasm32-unknown-unknown (via `wasmtime` as a runner) and asserts resulting state hashes match. If this breaks, prediction breaks. Do this early.

## Milestones

Each milestone ends with something runnable and demoable. Don't skip ahead — each surfaces issues before they compound.

### M0 — Workspace skeleton
- Cargo workspace with empty `shared`, `server`, `client`, `bots/rust-reference`.
- `shared/constants.rs` with values above.
- `trunk` builds the client to a hello-world canvas.
- `cargo run -p server` prints "listening on 8080".
- CI runs `cargo check` and `cargo test` on all crates, including wasm target.

### M1 — Single-player local physics
- Implement `Ship`, `Bullet`, physics step, wall collision in `shared/`.
- Client loads a hardcoded map, spawns local ship, reads keyboard (`a`/`s` turn, `shift` thrust, `enter` fire), runs shared sim locally with no server. Renders ships, bullets, walls, cosmetic particles.
- **Deliverable**: `trunk serve`, fly around alone, shoot bullets, bounce off walls.
- No networking yet. Validates the shared sim feels right.

### M2 — WebSocket transport, server sim, one client, no prediction
- `Transport` trait + `WebSocketTransport` impl on both sides. WebRTC stubbed out but not implemented yet.
- Server accepts WebSocket, runs one room, ticks sim at 60 Hz.
- Client opens WS, sends `Hello`/`JoinRoom`, sends inputs, renders latest snapshot directly. **No prediction** — will feel laggy, expected.
- **Deliverable**: one player flies around, visibly laggy on local network.

### M3 — Client-side prediction + reconciliation for local ship
- Client runs shared sim ahead on its own ship using unacked inputs.
- On snapshot, reconcile via rewind-and-replay.
- **Deliverable**: same as M2 but controls feel instant. Measure: no perceptible input latency on localhost.

### M4 — Multiple players, interpolation for remote entities
- Two+ browser clients can join the same room.
- Remote ships and bullets render with ~100 ms interp delay, smoothly.
- **Deliverable**: two browser windows, both see each other fly around smoothly.

### M5 — Combat: shooting, damage, death, respawn
- Bullets do damage. Ships have HP. At 0 HP ship dies, respawn after 2s at safe spawn point.
- `GameEvent::ShipDied` / `HitScored` fire reliably.
- Lag-compensated bullet spawn per the spec above.
- Score tracking in `PlayerInfo` (kills, deaths).
- **Deliverable**: two players fight, one wins a round. Kills counted.

### M6 — WebRTC DataChannel transport
- `WebRtcTransport` impl on both sides. `webrtc-rs` on server, `RTCPeerConnection` via `web-sys` on client.
- Signaling flows over the existing WS.
- Two DataChannels per peer: `unreliable` + `reliable`.
- Messages routed to the right channel based on `Reliability`.
- Client falls back to WS if RTC setup fails within 5 seconds.
- **Deliverable**: Chrome devtools shows gameplay traffic on a DataChannel, not WS. Artificial packet loss (via `tc netem` on the server host or Chrome devtools network throttling) shows smooth degradation instead of stalling.

### M7 — Thruster wash
- Server computes force cone behind each thrusting ship each tick.
- Ships/bullets inside the cone receive force.
- Client adds cosmetic particle visuals (client-only, not networked).
- **Deliverable**: two ships thrusting at each other get blown around. Bullets near a thrusting ship get deflected.

### M8 — Bot protocol + reference bots
- Formalize bot `Hello` flag.
- `bots/rust-reference`: connects via WS, plays basic "chase & shoot" AI.
- `bots/python-reference`: same, in Python. `README.md` is a tutorial.
- **Deliverable**: `python bots/python-reference/bot.py --server ws://localhost:8080 --room default` and the bot shows up and plays.

### M9 — Map loader + multiple maps
- Server scans `./maps/` on startup, exposes available maps.
- Clients can pick a map when creating a room.
- Ship `maps/arena.json`, `maps/empty.json`, one more interesting one.
- **Deliverable**: create a room with a chosen map, walls render correctly, collision works.

### M10 — Lobby UI + room list
- Client has a pre-game screen: list of rooms, create-room button, map picker.
- Simple, functional, no style polish.
- **Deliverable**: land on `http://localhost:3000`, see rooms, join one, play.

### Beyond M10 (not yet speccable)

- Delta-compressed snapshots
- Partial observability / radar
- Headless training mode (server runs at N× real time with rendering off, for bot training)
- TURN relay for internet play
- Replays (server already has authoritative tick history)
- Spectator mode
- Better visuals (WebGL via `wgpu`)

## Notes for claude-code working on this

- **Respect milestones**. Finish one before starting the next. Each should leave the project in a runnable state.
- **Don't premature-optimize**. Especially: no delta compression, no entity interpolation fanciness, no SIMD in the physics until after M10 and only if profiling demands it.
- **Determinism is sacred**. Any PR that adds nondeterminism to the shared sim (a `HashMap` iteration, a `std::f32::sin` call, a `thread_rng` in sim code) is a regression.
- **Test transport swaps early**. After M6, run every subsequent milestone's dev-testing over both WS and WebRTC. Bugs in transport abstraction will hide for weeks otherwise.
- **Keep `shared/` dependency-minimal**. It compiles to wasm. `tokio`, `axum`, `web-sys` etc. do NOT belong in `shared/`. Only `serde`, `libm`, `postcard`, small math/rand crates.
- **Log liberally on the server, less on the client**. `tracing` on server with structured fields; `log` + `console_log` on client, mostly at warn/error.
- **When in doubt, look at the original Elm source at https://github.com/mpdairy/xpilot.io/tree/master/src** for feel/tuning reference. The physics feel we want is the Elm version's feel.
