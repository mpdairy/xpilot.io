// Per-connection task. Owns:
//   - the WebSocket (carries control messages, signaling, and any traffic
//     the RTC channels haven't taken over yet)
//   - an optional WebRtcSession created lazily on the first RtcOffer from
//     the client
//   - a router that drains the room's outbound mpsc and dispatches each
//     ServerMessage to either the RTC channel of the right reliability or
//     the WS, falling back to WS whenever the channel isn't open
//   - a dispatch loop that reads ClientMessages from BOTH the WS and the
//     RTC channels and forwards them to the lobby/room
//
// Net effect: the room (`outbound: mpsc::Sender<ServerMessage>`) doesn't
// need to know transports exist — it just sends ServerMessages.

use std::sync::Arc;

use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::extract::State;
use axum::response::Response;
use futures_util::stream::{SplitSink, SplitStream};
use futures_util::{SinkExt, StreamExt};
use shared::entities::PlayerId;
use shared::protocol::{decode, encode, ClientMessage, Reliability, ServerMessage};
use tokio::sync::{mpsc, oneshot, RwLock};

use crate::lobby::Lobby;
use crate::room::RoomCommand;
use crate::webrtc_session::{server_reliability, WebRtcSession};

const OUTBOUND_BUFFER: usize = 64;
const SIGNAL_BUFFER: usize = 16;

pub async fn ws_handler(
    ws: WebSocketUpgrade,
    State(lobby): State<Arc<Lobby>>,
) -> Response {
    ws.on_upgrade(move |socket| handle_socket(socket, lobby))
}

async fn handle_socket(socket: WebSocket, lobby: Arc<Lobby>) {
    let (ws_write, ws_read) = socket.split();

    // Encoded postcard frames going out via WS.
    let (ws_out_tx, ws_out_rx) = mpsc::channel::<Vec<u8>>(OUTBOUND_BUFFER);
    // ServerMessages from room → router → wire.
    let (out_tx, out_rx) = mpsc::channel::<ServerMessage>(OUTBOUND_BUFFER);
    // Local-side signaling messages our WebRtcSession produces (answer, ICE).
    let (signal_tx, signal_rx) = mpsc::channel::<ServerMessage>(SIGNAL_BUFFER);
    // Raw bytes arriving on the RTC channels — decoded in dispatch_loop.
    let (rtc_in_tx, rtc_in_rx) = mpsc::channel::<Vec<u8>>(OUTBOUND_BUFFER);

    let rtc: Arc<RwLock<Option<Arc<WebRtcSession>>>> = Arc::new(RwLock::new(None));

    let writer = tokio::spawn(write_loop(ws_write, ws_out_rx));
    let router = tokio::spawn(router_loop(
        out_rx,
        signal_rx,
        ws_out_tx.clone(),
        rtc.clone(),
    ));

    let player_id = dispatch_loop(
        ws_read,
        rtc_in_rx,
        rtc_in_tx.clone(),
        out_tx.clone(),
        signal_tx.clone(),
        rtc.clone(),
        lobby.clone(),
    )
    .await;

    if let Some(pid) = player_id {
        let _ = lobby
            .room_tx
            .send(RoomCommand::RemovePlayer { player_id: pid })
            .await;
    }

    // Drop session — releases its callback-held mpsc clones, which lets the
    // router exit cleanly once the room outbound also drops.
    if let Some(s) = rtc.write().await.take() {
        s.close().await;
    }
    drop(out_tx);
    drop(signal_tx);
    drop(rtc_in_tx);

    let _ = router.await;
    let _ = writer.await;
}

async fn write_loop(
    mut write: SplitSink<WebSocket, Message>,
    mut rx: mpsc::Receiver<Vec<u8>>,
) {
    while let Some(bytes) = rx.recv().await {
        if write.send(Message::Binary(bytes)).await.is_err() {
            break;
        }
    }
    let _ = write.close().await;
}

/// Pulls outbound traffic from two sources — game messages from the room
/// (`out_rx`) and signaling messages from the WebRtcSession (`signal_rx`) —
/// and routes each to either the RTC datachannel of the right reliability
/// or the WebSocket. Falling back to WS while the corresponding channel
/// isn't open keeps gameplay alive during the SDP/ICE handshake and after
/// any RTC failure.
async fn router_loop(
    mut out_rx: mpsc::Receiver<ServerMessage>,
    signal_rx: mpsc::Receiver<ServerMessage>,
    ws_out_tx: mpsc::Sender<Vec<u8>>,
    rtc: Arc<RwLock<Option<Arc<WebRtcSession>>>>,
) {
    let mut signal_rx_opt = Some(signal_rx);
    loop {
        let signal_recv = async {
            match &mut signal_rx_opt {
                Some(rx) => rx.recv().await,
                // Park forever once the signaling side has closed —
                // otherwise the select would spin on a dead channel.
                None => std::future::pending().await,
            }
        };
        tokio::select! {
            msg = out_rx.recv() => {
                match msg {
                    Some(m) => route_outbound(m, &ws_out_tx, &rtc).await,
                    None => break, // room hung up
                }
            }
            sig = signal_recv => {
                match sig {
                    Some(m) => {
                        // Signaling always rides the WS — it negotiates the very
                        // RTC channels we'd otherwise route on.
                        if let Ok(bytes) = encode(&m) {
                            let _ = ws_out_tx.send(bytes).await;
                        }
                    }
                    None => signal_rx_opt = None,
                }
            }
        }
    }
}

async fn route_outbound(
    msg: ServerMessage,
    ws_out_tx: &mpsc::Sender<Vec<u8>>,
    rtc: &Arc<RwLock<Option<Arc<WebRtcSession>>>>,
) {
    let reliability = server_reliability(&msg);
    let bytes = match encode(&msg) {
        Ok(b) => b,
        Err(e) => {
            tracing::error!("encode error: {}", e);
            return;
        }
    };
    let session = rtc.read().await.as_ref().cloned();
    let sent = if let Some(s) = session {
        match reliability {
            Reliability::Unreliable => s.send_unreliable(&bytes).await,
            Reliability::Reliable => s.send_reliable(&bytes).await,
        }
    } else {
        false
    };
    if !sent {
        let _ = ws_out_tx.send(bytes).await;
    }
}

async fn dispatch_loop(
    mut ws_read: SplitStream<WebSocket>,
    mut rtc_in_rx: mpsc::Receiver<Vec<u8>>,
    rtc_in_tx: mpsc::Sender<Vec<u8>>,
    out_tx: mpsc::Sender<ServerMessage>,
    signal_tx: mpsc::Sender<ServerMessage>,
    rtc: Arc<RwLock<Option<Arc<WebRtcSession>>>>,
    lobby: Arc<Lobby>,
) -> Option<PlayerId> {
    let mut player_id: Option<PlayerId> = None;
    let mut joined = false;

    loop {
        let cm: ClientMessage = tokio::select! {
            ws_msg = ws_read.next() => {
                let Some(maybe) = ws_msg else { break };
                let msg = match maybe {
                    Ok(m) => m,
                    Err(e) => {
                        tracing::debug!("ws recv error: {}", e);
                        break;
                    }
                };
                let bytes = match msg {
                    Message::Binary(b) => b,
                    Message::Close(_) => break,
                    Message::Ping(_) | Message::Pong(_) | Message::Text(_) => continue,
                };
                match decode(&bytes) {
                    Ok(m) => m,
                    Err(e) => {
                        tracing::debug!("decode error: {} ({} bytes)", e, bytes.len());
                        let _ = out_tx
                            .send(ServerMessage::Error {
                                message: format!("bad postcard: {}", e),
                            })
                            .await;
                        continue;
                    }
                }
            }
            rtc_msg = rtc_in_rx.recv() => {
                let Some(bytes) = rtc_msg else {
                    // RTC channel closed — that's fine, just stop polling it
                    // (other arm still reads from WS). A re-spawn isn't needed
                    // for v1; client can reconnect to negotiate a fresh session.
                    std::future::pending::<()>().await;
                    unreachable!()
                };
                match decode(&bytes) {
                    Ok(m) => m,
                    Err(e) => {
                        tracing::debug!("rtc decode error: {}", e);
                        continue;
                    }
                }
            }
        };

        if !handle_message(
            cm,
            &mut player_id,
            &mut joined,
            &out_tx,
            &signal_tx,
            &rtc,
            &rtc_in_tx,
            &lobby,
        )
        .await
        {
            break;
        }
    }

    if joined {
        player_id
    } else {
        None
    }
}

/// Returns false when the connection should terminate (Leave or fatal).
async fn handle_message(
    cm: ClientMessage,
    player_id: &mut Option<PlayerId>,
    joined: &mut bool,
    out_tx: &mpsc::Sender<ServerMessage>,
    signal_tx: &mpsc::Sender<ServerMessage>,
    rtc: &Arc<RwLock<Option<Arc<WebRtcSession>>>>,
    rtc_in_tx: &mpsc::Sender<Vec<u8>>,
    lobby: &Arc<Lobby>,
) -> bool {
    match cm {
        ClientMessage::Hello { name, .. } => {
            if player_id.is_some() {
                return true;
            }
            let pid = lobby.alloc_player_id();
            *player_id = Some(pid);
            tracing::info!(player_id = pid, %name, "hello");
            let _ = out_tx
                .send(ServerMessage::Welcome {
                    player_id: pid,
                    server_tick: 0,
                })
                .await;
        }
        ClientMessage::JoinRoom { .. } => {
            let pid = match *player_id {
                Some(p) => p,
                None => return true,
            };
            if *joined {
                return true;
            }
            let (ack_tx, ack_rx) = oneshot::channel();
            let _ = lobby
                .room_tx
                .send(RoomCommand::AddPlayer {
                    player_id: pid,
                    name: format!("Player{}", pid),
                    outbound: out_tx.clone(),
                    ack: ack_tx,
                })
                .await;
            if let Ok(ack) = ack_rx.await {
                let _ = out_tx.send(ack.joined_message).await;
                *joined = true;
            }
        }
        ClientMessage::Input(input) => {
            let pid = match *player_id {
                Some(p) => p,
                None => return true,
            };
            if !*joined {
                return true;
            }
            let _ = lobby
                .room_tx
                .send(RoomCommand::Input {
                    player_id: pid,
                    input,
                })
                .await;
        }
        ClientMessage::Leave => return false,
        ClientMessage::RtcOffer { sdp } => {
            // Read the existing session BEFORE the match — otherwise the read
            // guard's lifetime extends through the None arm and deadlocks
            // against the write below.
            let existing = rtc.read().await.clone();
            let session = match existing {
                Some(s) => s,
                None => match WebRtcSession::new(rtc_in_tx.clone(), signal_tx.clone()).await {
                    Ok(s) => {
                        let s = Arc::new(s);
                        *rtc.write().await = Some(s.clone());
                        s
                    }
                    Err(e) => {
                        tracing::error!("rtc init failed: {}", e);
                        return true;
                    }
                },
            };
            match session.handle_offer(sdp).await {
                Ok(answer_sdp) => {
                    let _ = signal_tx
                        .send(ServerMessage::RtcAnswer { sdp: answer_sdp })
                        .await;
                }
                Err(e) => tracing::error!("handle_offer failed: {}", e),
            }
        }
        ClientMessage::RtcAnswer { .. } => {
            // Server is the answerer; clients shouldn't send us answers.
        }
        ClientMessage::RtcIceCandidate {
            candidate,
            sdp_mid,
            sdp_m_line_index,
        } => {
            let session = rtc.read().await.clone();
            if let Some(session) = session {
                if let Err(e) = session
                    .add_ice_candidate(candidate, sdp_mid, sdp_m_line_index)
                    .await
                {
                    tracing::warn!("add_ice_candidate failed: {}", e);
                }
            }
        }
    }
    true
}
