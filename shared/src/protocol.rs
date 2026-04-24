// Wire protocol — shapes shared by client, server, and bots.
// JSON is the only codec for M2; postcard slot reserved for M6.

use serde::{Deserialize, Serialize};

use crate::entities::{Bullet, EntityId, PlayerId, Ship};
use crate::map::Map;
use crate::world::Particle;

pub type RoomId = u32;
pub type ClientTick = u32;
pub type ServerTick = u32;

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
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ClientMessage {
    /// First message after connect.
    Hello {
        name: String,
        client_kind: ClientKind,
        supports_webrtc: bool,
    },
    /// Create or join a room.
    JoinRoom {
        room_id: Option<RoomId>,
        map_name: Option<String>,
    },
    /// Per-tick input. Sent unreliably (when WebRTC is wired up in M6).
    Input(TickInput),
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
