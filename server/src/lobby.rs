// Lobby — owns the registry of active rooms, the player-id allocator, and
// the loaded map catalog.
//
// `Lobby` is held behind `Arc` in axum's app state. Connections call into it
// to list/create/quick-join/get rooms. Each room runs as its own tokio task
// and carries an `Arc<AtomicUsize>` human-count plus a clone of the lobby's
// rooms map, so it can advertise its size cheaply via `RoomSummary` and
// remove itself when it's been empty long enough.

use std::collections::HashMap;
use std::path::Path;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
use std::sync::Arc;

use shared::constants::ROOM_PLAYER_CAP;
use shared::entities::PlayerId;
use shared::map::Map;
use shared::protocol::{RoomId, RoomSummary};
use tokio::sync::{mpsc, RwLock};

use crate::room::{self, RoomCommand};

const ROOM_COMMAND_BUFFER: usize = 256;

/// In-memory catalog of `.xp` map files loaded at startup. The source bytes
/// are kept around so each new room re-parses to a fresh `Map` (rooms mutate
/// `walls`, can't share).
pub struct MapRegistry {
    sources: HashMap<String, String>,
    pub names_sorted: Vec<String>,
}

impl MapRegistry {
    pub fn load_from_dir(dir: &Path) -> Result<Self, String> {
        let entries = std::fs::read_dir(dir)
            .map_err(|e| format!("read_dir {}: {}", dir.display(), e))?;
        let mut sources = HashMap::new();
        for entry in entries {
            let entry = entry.map_err(|e| format!("read_dir entry: {}", e))?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) != Some("xp") {
                continue;
            }
            let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };
            let bytes = std::fs::read(&path)
                .map_err(|e| format!("read {}: {}", path.display(), e))?;
            // Lossy decode tolerates legacy 8-bit chars in classic map headers
            // (e.g. globe.xp's "Björn"). We only ever scan ASCII/printable
            // out of these strings.
            let src = String::from_utf8_lossy(&bytes).into_owned();
            sources.insert(stem.to_string(), src);
        }
        if sources.is_empty() {
            return Err(format!("no .xp maps found in {}", dir.display()));
        }
        let mut names_sorted: Vec<String> = sources.keys().cloned().collect();
        names_sorted.sort();
        Ok(Self {
            sources,
            names_sorted,
        })
    }

    pub fn parse(&self, name: &str) -> Result<Map, String> {
        let src = self
            .sources
            .get(name)
            .ok_or_else(|| format!("unknown map: {}", name))?;
        shared::xp_map::parse(src).map_err(|e| format!("parse {}: {}", name, e))
    }

    pub fn contains(&self, name: &str) -> bool {
        self.sources.contains_key(name)
    }
}

#[derive(Clone)]
pub struct RoomHandle {
    pub room_id: RoomId,
    pub name: String,
    pub map_name: String,
    pub room_tx: mpsc::Sender<RoomCommand>,
    /// Live count of HUMAN players in the room. Bots don't count toward the
    /// cap or the lobby's "X/8" display — they're decoration. Updated by the
    /// room task on AddPlayer / RemovePlayer.
    pub human_count: Arc<AtomicUsize>,
}

impl RoomHandle {
    pub fn summarize(&self) -> RoomSummary {
        RoomSummary {
            room_id: self.room_id,
            name: self.name.clone(),
            map_name: self.map_name.clone(),
            player_count: self.human_count.load(Ordering::Relaxed) as u32,
            cap: ROOM_PLAYER_CAP as u32,
        }
    }
}

pub type RoomRegistry = Arc<RwLock<HashMap<RoomId, RoomHandle>>>;

pub struct Lobby {
    next_player_id: AtomicU32,
    next_room_id: AtomicU32,
    rooms: RoomRegistry,
    maps: Arc<MapRegistry>,
    /// Map used by quick-join when no rooms exist. Validated to be present in
    /// `maps` at construction.
    default_map_name: String,
}

impl Lobby {
    pub fn new(maps: MapRegistry, default_map_name: String) -> Self {
        debug_assert!(maps.contains(&default_map_name), "default map missing");
        Self {
            next_player_id: AtomicU32::new(1),
            next_room_id: AtomicU32::new(1),
            rooms: Arc::new(RwLock::new(HashMap::new())),
            maps: Arc::new(maps),
            default_map_name,
        }
    }

    pub fn alloc_player_id(&self) -> PlayerId {
        self.next_player_id.fetch_add(1, Ordering::Relaxed)
    }

    pub fn maps(&self) -> &MapRegistry {
        &self.maps
    }

    pub async fn list_rooms(&self) -> Vec<RoomSummary> {
        let rooms = self.rooms.read().await;
        let mut out: Vec<RoomSummary> = rooms.values().map(RoomHandle::summarize).collect();
        out.sort_by_key(|r| r.room_id);
        out
    }

    pub async fn get_room(&self, room_id: RoomId) -> Option<RoomHandle> {
        self.rooms.read().await.get(&room_id).cloned()
    }

    /// Allocate a room id, parse the named map, spawn the room task, and
    /// register the handle. Returns the handle so the caller can immediately
    /// `AddPlayer` to join it. `bot_count` is clamped to 0..=8.
    pub async fn create_room(
        &self,
        name: String,
        map_name: String,
        bot_count: u32,
    ) -> Result<RoomHandle, String> {
        let map = self.maps.parse(&map_name)?;
        let room_id = self.next_room_id.fetch_add(1, Ordering::Relaxed);
        let (room_tx, room_rx) = mpsc::channel::<RoomCommand>(ROOM_COMMAND_BUFFER);
        let human_count = Arc::new(AtomicUsize::new(0));
        let handle = RoomHandle {
            room_id,
            name: name.clone(),
            map_name: map_name.clone(),
            room_tx,
            human_count: human_count.clone(),
        };
        self.rooms.write().await.insert(room_id, handle.clone());
        let registry = self.rooms.clone();
        let bots = bot_count.min(8);
        tokio::spawn(async move {
            room::run(room_id, map, room_rx, human_count, registry, bots).await;
        });
        tracing::info!(room_id, %name, %map_name, bots, "created room");
        Ok(handle)
    }

    /// Pick the most-populated non-full room (so quick-joiners fill rooms
    /// rather than fragment them), or create a new room with the default map
    /// if none have capacity. New rooms get the default bot count (4).
    pub async fn quick_join(&self) -> Result<RoomHandle, String> {
        let candidate = {
            let rooms = self.rooms.read().await;
            rooms
                .values()
                .filter(|rh| rh.human_count.load(Ordering::Relaxed) < ROOM_PLAYER_CAP)
                .max_by_key(|rh| rh.human_count.load(Ordering::Relaxed))
                .cloned()
        };
        match candidate {
            Some(rh) => Ok(rh),
            None => {
                let next_id = self.next_room_id.load(Ordering::Relaxed);
                let name = format!("Room {}", next_id);
                self.create_room(name, self.default_map_name.clone(), 4).await
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::room::{JoinResult, RoomCommand};
    use std::path::PathBuf;
    use tokio::sync::oneshot;

    fn locate_maps_dir() -> PathBuf {
        ["maps", "../maps"]
            .iter()
            .map(PathBuf::from)
            .find(|p| p.exists())
            .expect("maps dir not found from cwd — run from workspace or server crate")
    }

    /// Verify create_room → list_rooms → get_room → join → list (count=1) →
    /// full at cap → second join rejected.
    #[tokio::test]
    async fn lobby_lifecycle_create_join_full() {
        let dir = locate_maps_dir();
        let maps = MapRegistry::load_from_dir(&dir).expect("load maps");
        assert!(maps.contains("tournament"));
        let lobby = Lobby::new(maps, "tournament".into());

        // Initially no rooms.
        assert!(lobby.list_rooms().await.is_empty());

        // Create a room.
        let h = lobby
            .create_room("alpha".into(), "tournament".into(), 0)
            .await
            .expect("create");
        assert_eq!(h.name, "alpha");
        let rooms = lobby.list_rooms().await;
        assert_eq!(rooms.len(), 1);
        assert_eq!(rooms[0].player_count, 0);
        assert_eq!(rooms[0].cap, ROOM_PLAYER_CAP as u32);

        // Lookup by id matches.
        let h2 = lobby.get_room(h.room_id).await.unwrap();
        assert_eq!(h2.room_id, h.room_id);

        // Fill the room to capacity and verify the next join is rejected.
        // Use one outbound channel for everyone — we only care about the
        // ack result, not the actual snapshot stream.
        let (out_tx, mut out_rx) = mpsc::channel(64);
        for i in 0..ROOM_PLAYER_CAP {
            let pid = lobby.alloc_player_id();
            let (ack_tx, ack_rx) = oneshot::channel();
            h.room_tx
                .send(RoomCommand::AddPlayer {
                    player_id: pid,
                    name: format!("P{}", i),
                    outbound: out_tx.clone(),
                    ack: ack_tx,
                })
                .await
                .expect("send AddPlayer");
            let ack = ack_rx.await.expect("ack");
            assert!(matches!(ack.result, JoinResult::Joined(_)), "join {} should succeed", i);
        }
        // Drain the JoinedRoom + PlayerJoined broadcasts so the channel
        // doesn't fill and back-pressure subsequent sends.
        while out_rx.try_recv().is_ok() {}

        // The atomic should now be at cap.
        assert_eq!(h.human_count.load(Ordering::Relaxed), ROOM_PLAYER_CAP);
        let rooms = lobby.list_rooms().await;
        assert_eq!(rooms[0].player_count, ROOM_PLAYER_CAP as u32);

        // One more join → Full.
        let pid = lobby.alloc_player_id();
        let (ack_tx, ack_rx) = oneshot::channel();
        h.room_tx
            .send(RoomCommand::AddPlayer {
                player_id: pid,
                name: "overflow".into(),
                outbound: out_tx.clone(),
                ack: ack_tx,
            })
            .await
            .expect("send AddPlayer");
        let ack = ack_rx.await.expect("ack");
        assert!(matches!(ack.result, JoinResult::Full));
    }

    #[tokio::test]
    async fn quick_join_creates_room_when_none_exist() {
        let dir = locate_maps_dir();
        let maps = MapRegistry::load_from_dir(&dir).expect("load maps");
        let lobby = Lobby::new(maps, "tournament".into());

        assert!(lobby.list_rooms().await.is_empty());
        let h = lobby.quick_join().await.expect("quick join creates");
        assert_eq!(lobby.list_rooms().await.len(), 1);
        assert_eq!(h.map_name, "tournament");

        // Second quick_join with capacity remaining returns the same room.
        let h2 = lobby.quick_join().await.expect("quick join existing");
        assert_eq!(h.room_id, h2.room_id);
    }
}
