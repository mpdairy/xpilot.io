// Wire protocol — shapes shared by client, server, and bots.
// JSON is the only codec for M2; postcard slot reserved for M6.

use serde::{Deserialize, Serialize};

use crate::entities::{Bullet, EntityId, PlayerId, Ship};
use crate::map::Map;
use crate::world::Particle;

pub type RoomId = u32;
pub type ClientTick = u32;
pub type ServerTick = u32;

/// Wire-protocol version. Bumped on any incompatible change to client/server
/// messages (additions of new variants are still backwards-compatible because
/// of postcard's enum discriminants, but field reorders, removals, or type
/// changes are not). Federated/community servers in the future use this so a
/// client can grey out servers it can't talk to.
pub const PROTOCOL_VERSION: u32 = 1;

#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default)]
pub struct TickInput {
    pub client_tick: ClientTick,
    pub turn_left: bool,
    pub turn_right: bool,
    pub thrust: bool,
    pub fire: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ClientKind {
    Human,
    Bot { author: String },
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub enum Reliability {
    Unreliable,
    Reliable,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PlayerInfo {
    pub player_id: PlayerId,
    pub name: String,
    pub kills: u32,
    pub deaths: u32,
    /// `true` while the player is in the respawn timer.
    pub dead: bool,
    /// `true` for server-spawned bot players (Sid, Cobra, etc.). Clients
    /// decorate these names so humans can tell who's a bot at a glance.
    pub is_bot: bool,
}

/// Summary of a room shown in the lobby's room list. Cheap to compute and
/// stable enough that we can safely cache `player_count` via an atomic
/// updated by the room.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RoomSummary {
    pub room_id: RoomId,
    pub name: String,
    pub map_name: String,
    pub player_count: u32,
    pub cap: u32,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum GameEvent {
    ShipSpawned {
        entity_id: EntityId,
        player_id: PlayerId,
        pos: crate::math::Vec2,
    },
    ShipDied {
        entity_id: EntityId,
        /// PlayerId of the dead ship. Carried in the event itself so clients
        /// can label kill notifications without racing the next snapshot
        /// (snapshots may arrive out of order with reliable events).
        victim_player_id: PlayerId,
        killer: Option<PlayerId>,
        /// Where the ship died. Lets clients spawn explosion particles even
        /// though the ship is gone from the next snapshot.
        pos: crate::math::Vec2,
        /// The dead ship's velocity at the moment of death — used so client
        /// explosion particles drift with its momentum.
        vel: crate::math::Vec2,
    },
    BulletFired {
        entity_id: EntityId,
        shooter: EntityId,
        pos: crate::math::Vec2,
        vel: crate::math::Vec2,
    },
    HitScored {
        shooter: PlayerId,
        victim: PlayerId,
        damage: u32,
    },
    /// Chat message broadcast to everyone in the room. Author looked up by
    /// `player_id` against the most recent PlayerInfo on the client side —
    /// no need to bake the name into every chat event.
    Chat {
        player_id: PlayerId,
        text: String,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Snapshot {
    pub server_tick: ServerTick,
    /// Last `client_tick` the server has processed for *this* recipient.
    /// Lets the client drop already-acked inputs and re-predict only unacked ones (M3+).
    pub your_last_processed_input: ClientTick,
    pub ships: Vec<Ship>,
    pub bullets: Vec<Bullet>,
    /// Live explosion debris. Authoritative on the server; client renders
    /// these directly.
    pub particles: Vec<Particle>,
    /// Current scoreboard. Cheap (~30 bytes/player) and lets the HUD always
    /// be in sync without an extra reliable channel.
    pub players: Vec<PlayerInfo>,
    /// Grid cells of cannons currently destroyed. Empty when all are alive
    /// (the common case). Client looks each up in the map's BlockGrid to
    /// know which firing triangle to suppress.
    pub dead_cannons: Vec<(u32, u32)>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ClientMessage {
    /// First message after connect. `protocol_version` lets the server reject
    /// incompatible clients up front rather than letting state get weird.
    Hello {
        name: String,
        client_kind: ClientKind,
        supports_webrtc: bool,
        protocol_version: u32,
    },
    /// Update the player's display name. Sent before each lobby action so
    /// the name shown in `JoinedRoom`/`PlayerJoined` reflects what the
    /// player typed in the name field. Ignored once the player is in a room.
    SetName {
        name: String,
    },
    /// Request the current list of rooms. Server responds with `RoomList`.
    ListRooms,
    /// Create a new room with the given name and map, then auto-join it.
    /// `bot_count` is clamped to 0..=8 server-side.
    CreateRoom {
        name: String,
        map_name: String,
        bot_count: u32,
    },
    /// Join a specific room by id, or quick-join (`None`) which picks a
    /// non-full room or creates one with the default map if none exist.
    JoinRoom {
        room_id: Option<RoomId>,
    },
    /// Per-tick input. Sent unreliably (when WebRTC is wired up in M6).
    Input(TickInput),
    /// Reports the visible-world rectangle around the player's ship in world
    /// units. Server uses it (plus AOI_MARGIN) to filter bullets/particles
    /// out of snapshots that aren't visible to this player. Sent reliably:
    /// the value is sticky on the server until the next one arrives. Send
    /// after JoinedRoom and again whenever the canvas/zoom changes (debounce
    /// rapid changes — e.g. mobile pinch-zoom — client-side).
    Viewport {
        half_width: f32,
        half_height: f32,
    },
    /// Free-text chat from the player. Server validates length + trims, then
    /// rebroadcasts as `GameEvent::Chat` to everyone in the room. Reliable.
    Chat {
        text: String,
    },
    Leave,
    /// WebRTC signaling — stub for M6.
    RtcOffer { sdp: String },
    RtcAnswer { sdp: String },
    RtcIceCandidate {
        candidate: String,
        sdp_mid: Option<String>,
        sdp_m_line_index: Option<u16>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ServerMessage {
    Welcome {
        player_id: PlayerId,
        server_tick: ServerTick,
        protocol_version: u32,
    },
    /// Names of maps the server has loaded. Sent once after Welcome so the
    /// client's create-room form can populate its picker without a round trip.
    AvailableMaps {
        names: Vec<String>,
    },
    /// Snapshot of currently-active rooms. Sent in response to `ListRooms`.
    RoomList {
        rooms: Vec<RoomSummary>,
    },
    JoinedRoom {
        room_id: RoomId,
        map: Map,
        players: Vec<PlayerInfo>,
        your_ship_id: EntityId,
    },
    PlayerJoined(PlayerInfo),
    PlayerLeft(PlayerId),
    /// Full snapshot at SNAPSHOT_RATE_HZ. Sent unreliably.
    Snapshot(Snapshot),
    /// Events that shouldn't be missed. Sent reliably.
    Event(GameEvent),
    Error {
        message: String,
    },
    /// WebRTC signaling — stub for M6.
    RtcOffer { sdp: String },
    RtcAnswer { sdp: String },
    RtcIceCandidate {
        candidate: String,
        sdp_mid: Option<String>,
        sdp_m_line_index: Option<u16>,
    },
}

// Binary wire codec — postcard. Roughly a third the bytes of JSON for our
// snapshot shapes, no whitespace, no field names.
pub fn encode<T: Serialize>(msg: &T) -> Result<Vec<u8>, postcard::Error> {
    postcard::to_stdvec(msg)
}

pub fn decode<'a, T: Deserialize<'a>>(bytes: &'a [u8]) -> Result<T, postcard::Error> {
    postcard::from_bytes(bytes)
}

// JSON codec — kept around for bot SDKs in other languages and for hand-eyeball
// debugging. Not on the gameplay path anymore.
pub fn encode_json<T: Serialize>(msg: &T) -> Result<String, serde_json::Error> {
    serde_json::to_string(msg)
}

pub fn decode_json<'a, T: Deserialize<'a>>(text: &'a str) -> Result<T, serde_json::Error> {
    serde_json::from_str(text)
}
