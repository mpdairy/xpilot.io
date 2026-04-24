// Lobby — for M2 there is exactly one Room. Real lobby logic lands in M10.
//
// `Lobby` is held in axum's app state behind an `Arc`. It owns the player-id
// allocator and a handle (mpsc sender) to the single room's command channel.

use std::sync::atomic::{AtomicU32, Ordering};

use shared::entities::PlayerId;
use shared::map::Map;
use tokio::sync::mpsc;

use crate::room::{self, RoomCommand};

pub struct Lobby {
    next_player_id: AtomicU32,
    pub room_tx: mpsc::Sender<RoomCommand>,
}

impl Lobby {
    pub fn new(map: Map) -> Self {
        let (room_tx, room_rx) = mpsc::channel::<RoomCommand>(256);
        tokio::spawn(room::run(map, room_rx));

        Self {
            next_player_id: AtomicU32::new(1),
            room_tx,
        }
    }

    pub fn alloc_player_id(&self) -> PlayerId {
        self.next_player_id.fetch_add(1, Ordering::Relaxed)
    }
}
