// Per-connection WebRTC session. The browser is the offerer (it creates
// `unreliable` and `reliable` data channels and bakes them into the SDP
// offer); we're the answerer. Once both channels open, gameplay traffic
// flows through them and the WebSocket carries only signaling + control
// messages.
//
// Reliability mapping at the channel level:
//   `unreliable` — ordered=false, maxRetransmits=0  (snapshots, inputs)
//   `reliable`   — ordered=true,  fully reliable     (events, hello, etc.)

use std::sync::Arc;

use bytes::Bytes;
use shared::protocol::ServerMessage;
use tokio::sync::{mpsc, Mutex};
use webrtc::api::APIBuilder;
use webrtc::api::interceptor_registry::register_default_interceptors;
use webrtc::api::media_engine::MediaEngine;
use webrtc::data_channel::data_channel_message::DataChannelMessage;
use webrtc::data_channel::data_channel_state::RTCDataChannelState;
use webrtc::data_channel::RTCDataChannel;
use webrtc::ice_transport::ice_candidate::{RTCIceCandidate, RTCIceCandidateInit};
use webrtc::ice_transport::ice_server::RTCIceServer;
use webrtc::interceptor::registry::Registry;
use webrtc::peer_connection::configuration::RTCConfiguration;
use webrtc::peer_connection::sdp::session_description::RTCSessionDescription;
use webrtc::peer_connection::RTCPeerConnection;

pub const UNRELIABLE_LABEL: &str = "unreliable";
pub const RELIABLE_LABEL: &str = "reliable";

pub struct WebRtcSession {
    peer: Arc<RTCPeerConnection>,
    unreliable: Arc<Mutex<Option<Arc<RTCDataChannel>>>>,
    reliable: Arc<Mutex<Option<Arc<RTCDataChannel>>>>,
}

impl WebRtcSession {
    /// `inbound_tx` receives raw JSON strings from either data channel
    /// (deserialization happens at the dispatch layer).
    /// `signal_tx` is how we push our local ICE candidates back to the client
    /// over the existing WS — `connection::router_loop` forwards it.
    pub async fn new(
        inbound_tx: mpsc::Sender<Vec<u8>>,
        signal_tx: mpsc::Sender<ServerMessage>,
    ) -> Result<Self, webrtc::Error> {
        let mut me = MediaEngine::default();
        me.register_default_codecs()?;
        let mut registry = Registry::new();
        registry = register_default_interceptors(registry, &mut me)?;
        let api = APIBuilder::new()
            .with_media_engine(me)
            .with_interceptor_registry(registry)
            .build();

        let config = RTCConfiguration {
            ice_servers: vec![RTCIceServer {
                urls: vec!["stun:stun.l.google.com:19302".to_string()],
                ..Default::default()
            }],
            ..Default::default()
        };
        let peer = Arc::new(api.new_peer_connection(config).await?);

        // Stream our locally-discovered ICE candidates back to the offerer
        // over the WS. `None` marks end-of-candidates — we currently ignore it
        // since trickle ICE handles termination implicitly.
        let signal_tx_cb = signal_tx.clone();
        peer.on_ice_candidate(Box::new(move |c: Option<RTCIceCandidate>| {
            let signal_tx = signal_tx_cb.clone();
            Box::pin(async move {
                if let Some(c) = c {
                    if let Ok(json) = c.to_json() {
                        let _ = signal_tx
                            .send(ServerMessage::RtcIceCandidate {
                                candidate: json.candidate,
                                sdp_mid: json.sdp_mid,
                                sdp_m_line_index: json.sdp_mline_index.map(|x| x as u16),
                            })
                            .await;
                    }
                }
            })
        }));

        let unreliable: Arc<Mutex<Option<Arc<RTCDataChannel>>>> = Arc::new(Mutex::new(None));
        let reliable: Arc<Mutex<Option<Arc<RTCDataChannel>>>> = Arc::new(Mutex::new(None));

        // Slot incoming channels by label as the browser opens them. Wire
        // each one to forward text frames into `inbound_tx`.
        let unreliable_cb = unreliable.clone();
        let reliable_cb = reliable.clone();
        let inbound_tx_outer = inbound_tx.clone();
        peer.on_data_channel(Box::new(move |dc: Arc<RTCDataChannel>| {
            let label = dc.label().to_string();
            let unreliable = unreliable_cb.clone();
            let reliable = reliable_cb.clone();
            let inbound_tx = inbound_tx_outer.clone();
            Box::pin(async move {
                let inbound_tx_msg = inbound_tx.clone();
                let label_for_log = label.clone();
                dc.on_open(Box::new(move || {
                    let label = label_for_log.clone();
                    Box::pin(async move {
                        tracing::info!(label = %label, "rtc datachannel open");
                    })
                }));
                dc.on_message(Box::new(move |msg: DataChannelMessage| {
                    let tx = inbound_tx_msg.clone();
                    Box::pin(async move {
                        let _ = tx.send(msg.data.to_vec()).await;
                    })
                }));
                match label.as_str() {
                    UNRELIABLE_LABEL => *unreliable.lock().await = Some(dc),
                    RELIABLE_LABEL => *reliable.lock().await = Some(dc),
                    other => tracing::warn!(label = %other, "unknown rtc channel label"),
                }
            })
        }));

        Ok(Self { peer, unreliable, reliable })
    }

    /// Apply browser's SDP offer, generate + set our answer, return the
    /// answer SDP so the caller can ship it back over the WS.
    pub async fn handle_offer(&self, sdp: String) -> Result<String, webrtc::Error> {
        let offer = RTCSessionDescription::offer(sdp)?;
        self.peer.set_remote_description(offer).await?;
        let answer = self.peer.create_answer(None).await?;
        self.peer.set_local_description(answer).await?;
        let local = self
            .peer
            .local_description()
            .await
            .ok_or_else(|| webrtc::Error::new("no local description after set".into()))?;
        Ok(local.sdp)
    }

    pub async fn add_ice_candidate(
        &self,
        candidate: String,
        sdp_mid: Option<String>,
        sdp_m_line_index: Option<u16>,
    ) -> Result<(), webrtc::Error> {
        let init = RTCIceCandidateInit {
            candidate,
            sdp_mid,
            sdp_mline_index: sdp_m_line_index.map(|x| x as u16),
            username_fragment: None,
        };
        self.peer.add_ice_candidate(init).await?;
        Ok(())
    }

    /// True if the named channel is open. Cheap (one mutex hop, one atomic read).
    pub async fn unreliable_open(&self) -> bool {
        match &*self.unreliable.lock().await {
            Some(dc) => dc.ready_state() == RTCDataChannelState::Open,
            None => false,
        }
    }

    pub async fn reliable_open(&self) -> bool {
        match &*self.reliable.lock().await {
            Some(dc) => dc.ready_state() == RTCDataChannelState::Open,
            None => false,
        }
    }

    /// Send a binary frame on the unreliable channel. Returns false if the
    /// channel isn't open yet (caller falls back to WS).
    pub async fn send_unreliable(&self, payload: &[u8]) -> bool {
        let guard = self.unreliable.lock().await;
        let Some(dc) = guard.as_ref() else { return false };
        if dc.ready_state() != RTCDataChannelState::Open {
            return false;
        }
        dc.send(&Bytes::copy_from_slice(payload)).await.is_ok()
    }

    pub async fn send_reliable(&self, payload: &[u8]) -> bool {
        let guard = self.reliable.lock().await;
        let Some(dc) = guard.as_ref() else { return false };
        if dc.ready_state() != RTCDataChannelState::Open {
            return false;
        }
        dc.send(&Bytes::copy_from_slice(payload)).await.is_ok()
    }

    pub async fn close(&self) {
        let _ = self.peer.close().await;
    }
}

/// Server-side classification: which reliability tier each ServerMessage
/// belongs on. Drives both the routing decision and the choice of
/// data-channel config the client creates.
pub fn server_reliability(msg: &ServerMessage) -> shared::protocol::Reliability {
    use shared::protocol::Reliability;
    match msg {
        ServerMessage::Snapshot(_) => Reliability::Unreliable,
        _ => Reliability::Reliable,
    }
}
