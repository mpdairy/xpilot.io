// Browser transport. Always opens a WebSocket (carries control messages,
// signaling, and any traffic the RTC channels can't take). After the WS
// opens we kick off WebRTC negotiation: create RTCPeerConnection + two
// DataChannels (`unreliable`, `reliable`), generate an SDP offer, ship it
// over the WS. Server answers + trickles ICE the same way. Once both
// channels are open, gameplay traffic flows through them.
//
// Reliability mapping on the client side:
//   `Input` → unreliable channel (per-tick, drop-tolerant)
//   everything else → reliable channel
//   if the relevant channel isn't open, fall back to the WS so things keep
//   working during/after a failed handshake.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use shared::protocol::{decode, encode, ClientMessage, ServerMessage};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{
    js_sys, BinaryType, CloseEvent, ErrorEvent, MessageEvent, RtcConfiguration,
    RtcDataChannel, RtcDataChannelInit, RtcDataChannelState, RtcIceCandidate,
    RtcIceCandidateInit, RtcPeerConnection, RtcPeerConnectionIceEvent, RtcSdpType,
    RtcSessionDescriptionInit, WebSocket,
};

/// How long to keep trying to bring up RTC before logging a warning.
/// Sends silently fall back to the WS the whole time, so this is purely
/// informational.
const RTC_FALLBACK_LOG_AFTER_MS: i32 = 5_000;

pub struct Transport {
    inner: Rc<TransportInner>,
}

struct TransportInner {
    ws: WebSocket,
    peer: RefCell<Option<RtcPeerConnection>>,
    unreliable: RefCell<Option<RtcDataChannel>>,
    reliable: RefCell<Option<RtcDataChannel>>,
    on_message: RefCell<Option<Box<dyn FnMut(ServerMessage)>>>,
    rtc_logged_fallback: Cell<bool>,
}

impl Transport {
    pub fn connect<MsgFn, OpenFn>(
        url: &str,
        on_message: MsgFn,
        on_open: OpenFn,
    ) -> Result<Self, JsValue>
    where
        MsgFn: FnMut(ServerMessage) + 'static,
        OpenFn: FnOnce() + 'static,
    {
        let ws = WebSocket::new(url)?;
        // Receive binary frames as ArrayBuffer so we can copy them into a
        // Vec<u8> directly. Without this, Blob → async read.
        ws.set_binary_type(BinaryType::Arraybuffer);

        let inner = Rc::new(TransportInner {
            ws: ws.clone(),
            peer: RefCell::new(None),
            unreliable: RefCell::new(None),
            reliable: RefCell::new(None),
            on_message: RefCell::new(Some(Box::new(on_message))),
            rtc_logged_fallback: Cell::new(false),
        });

        // WS message: decode + dispatch (signaling intercepts inside).
        let inner_msg = inner.clone();
        let onmessage = Closure::<dyn FnMut(MessageEvent)>::new(move |ev: MessageEvent| {
            if let Some(bytes) = bytes_from_event(&ev) {
                inner_msg.handle_inbound(&bytes);
            }
        });
        ws.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
        onmessage.forget();

        // WS open: hand to caller (lib.rs sends Hello/JoinRoom in there).
        // After that callback returns we kick off WebRTC negotiation.
        let on_open = Rc::new(RefCell::new(Some(on_open)));
        let inner_open = inner.clone();
        let onopen = Closure::<dyn FnMut(web_sys::Event)>::new(move |_ev| {
            if let Some(cb) = on_open.borrow_mut().take() {
                cb();
            }
            if let Err(e) = inner_open.start_webrtc() {
                log::warn!("rtc start failed (will stay on WS): {:?}", e);
            }
        });
        ws.set_onopen(Some(onopen.as_ref().unchecked_ref()));
        onopen.forget();

        let onerror = Closure::<dyn FnMut(ErrorEvent)>::new(|ev: ErrorEvent| {
            log::error!("ws error: {} {}", ev.message(), ev.filename());
        });
        ws.set_onerror(Some(onerror.as_ref().unchecked_ref()));
        onerror.forget();

        let onclose = Closure::<dyn FnMut(CloseEvent)>::new(|ev: CloseEvent| {
            log::warn!("ws closed: code={} reason={:?}", ev.code(), ev.reason());
        });
        ws.set_onclose(Some(onclose.as_ref().unchecked_ref()));
        onclose.forget();

        Ok(Self { inner })
    }

    pub fn send(&self, msg: &ClientMessage) {
        let Ok(bytes) = encode(msg) else { return };
        let reliability = client_reliability(msg);
        // Signaling always rides the WS — it negotiates the very channels
        // we'd otherwise route on.
        let force_ws = matches!(
            msg,
            ClientMessage::RtcOffer { .. }
                | ClientMessage::RtcAnswer { .. }
                | ClientMessage::RtcIceCandidate { .. }
        );
        if !force_ws {
            let dc = match reliability {
                Reliability::Unreliable => self.inner.unreliable.borrow(),
                Reliability::Reliable => self.inner.reliable.borrow(),
            };
            if let Some(dc) = dc.as_ref() {
                if dc.ready_state() == RtcDataChannelState::Open {
                    if dc.send_with_u8_array(&bytes).is_ok() {
                        return;
                    }
                }
            }
        }
        let _ = self.inner.ws.send_with_u8_array(&bytes);
    }

    pub fn is_open(&self) -> bool {
        self.inner.ws.ready_state() == WebSocket::OPEN
    }
}

#[derive(Clone, Copy)]
enum Reliability {
    Unreliable,
    Reliable,
}

fn client_reliability(msg: &ClientMessage) -> Reliability {
    match msg {
        ClientMessage::Input(_) => Reliability::Unreliable,
        _ => Reliability::Reliable,
    }
}

impl TransportInner {
    /// Top-level inbound dispatch (used by both WS onmessage and RTC channel
    /// onmessage). Signaling messages get handled here directly so the rest
    /// of the client never sees them; everything else is forwarded to the
    /// caller's `on_message`.
    fn handle_inbound(self: &Rc<Self>, bytes: &[u8]) {
        let msg = match decode::<ServerMessage>(bytes) {
            Ok(m) => m,
            Err(e) => {
                log::warn!("decode error: {} ({} bytes)", e, bytes.len());
                return;
            }
        };
        match msg {
            ServerMessage::RtcAnswer { sdp } => {
                let inner = self.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    inner.apply_remote_answer(sdp).await;
                });
            }
            ServerMessage::RtcIceCandidate {
                candidate,
                sdp_mid,
                sdp_m_line_index,
            } => {
                let inner = self.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    inner
                        .apply_remote_ice(candidate, sdp_mid, sdp_m_line_index)
                        .await;
                });
            }
            ServerMessage::RtcOffer { .. } => {
                // Server is the answerer — it doesn't send offers.
                log::debug!("ignoring unexpected server offer");
            }
            other => {
                if let Some(cb) = self.on_message.borrow_mut().as_mut() {
                    cb(other);
                }
            }
        }
    }

    fn start_webrtc(self: &Rc<Self>) -> Result<(), JsValue> {
        // STUN-only config: enough for LAN + most home NATs. TURN can be added
        // later when real users hit symmetric NATs we can't punch through.
        let config = RtcConfiguration::new();
        let ice_servers = js_sys::Array::new();
        let stun = js_sys::Object::new();
        js_sys::Reflect::set(&stun, &"urls".into(), &"stun:stun.l.google.com:19302".into())?;
        ice_servers.push(&stun);
        config.set_ice_servers(&ice_servers);

        let peer = RtcPeerConnection::new_with_configuration(&config)?;

        // Trickle ICE: each local candidate gets shipped to server over WS as
        // soon as it's gathered.
        let inner_ice = self.clone();
        let on_ice =
            Closure::<dyn FnMut(RtcPeerConnectionIceEvent)>::new(move |ev: RtcPeerConnectionIceEvent| {
                let Some(c) = ev.candidate() else { return };
                let cand = c.candidate();
                let sdp_mid = c.sdp_mid();
                let sdp_m_line_index = c.sdp_m_line_index();
                let msg = ClientMessage::RtcIceCandidate {
                    candidate: cand,
                    sdp_mid,
                    sdp_m_line_index,
                };
                if let Ok(bytes) = encode(&msg) {
                    let _ = inner_ice.ws.send_with_u8_array(&bytes);
                }
            });
        peer.set_onicecandidate(Some(on_ice.as_ref().unchecked_ref()));
        on_ice.forget();

        // Two channels, configured with the right reliability semantics.
        // `negotiated: false` (the default) means SDP carries the channel
        // info to the answerer; the server sees them via on_data_channel.
        let unrel_init = RtcDataChannelInit::new();
        unrel_init.set_ordered(false);
        unrel_init.set_max_retransmits(0);
        let unrel_dc = peer.create_data_channel_with_data_channel_dict("unreliable", &unrel_init);

        let rel_init = RtcDataChannelInit::new();
        rel_init.set_ordered(true);
        let rel_dc = peer.create_data_channel_with_data_channel_dict("reliable", &rel_init);

        wire_channel(&self, &unrel_dc, "unreliable");
        wire_channel(&self, &rel_dc, "reliable");

        *self.unreliable.borrow_mut() = Some(unrel_dc);
        *self.reliable.borrow_mut() = Some(rel_dc);

        // Generate offer + send it. Done async because all of these return
        // Promises — wasm_bindgen_futures bridges them for us.
        let inner_offer = self.clone();
        let peer_offer = peer.clone();
        wasm_bindgen_futures::spawn_local(async move {
            let offer = match wasm_bindgen_futures::JsFuture::from(peer_offer.create_offer()).await {
                Ok(o) => o,
                Err(e) => {
                    log::error!("create_offer failed: {:?}", e);
                    return;
                }
            };
            let sdp_str = match js_sys::Reflect::get(&offer, &"sdp".into())
                .ok()
                .and_then(|v| v.as_string())
            {
                Some(s) => s,
                None => {
                    log::error!("offer had no sdp string");
                    return;
                }
            };
            let local_desc = RtcSessionDescriptionInit::new(RtcSdpType::Offer);
            local_desc.set_sdp(&sdp_str);
            if let Err(e) = wasm_bindgen_futures::JsFuture::from(
                peer_offer.set_local_description(&local_desc),
            )
            .await
            {
                log::error!("set_local_description failed: {:?}", e);
                return;
            }
            let send_msg = ClientMessage::RtcOffer { sdp: sdp_str };
            if let Ok(bytes) = encode(&send_msg) {
                let _ = inner_offer.ws.send_with_u8_array(&bytes);
            }
        });

        *self.peer.borrow_mut() = Some(peer);

        // Informational: if neither channel has opened by `RTC_FALLBACK_LOG_AFTER_MS`,
        // log it. Sends are already auto-falling-back to WS in the meantime.
        let inner_timer = self.clone();
        let cb = Closure::<dyn FnMut()>::new(move || {
            if inner_timer.rtc_logged_fallback.get() {
                return;
            }
            let any_open = inner_timer
                .unreliable
                .borrow()
                .as_ref()
                .map(|dc| dc.ready_state() == RtcDataChannelState::Open)
                .unwrap_or(false)
                || inner_timer
                    .reliable
                    .borrow()
                    .as_ref()
                    .map(|dc| dc.ready_state() == RtcDataChannelState::Open)
                    .unwrap_or(false);
            if !any_open {
                log::warn!("rtc channels still closed after 5s — gameplay running over WS fallback");
                inner_timer.rtc_logged_fallback.set(true);
            }
        });
        if let Some(window) = web_sys::window() {
            let _ = window.set_timeout_with_callback_and_timeout_and_arguments_0(
                cb.as_ref().unchecked_ref(),
                RTC_FALLBACK_LOG_AFTER_MS,
            );
        }
        cb.forget();

        Ok(())
    }

    async fn apply_remote_answer(self: Rc<Self>, sdp: String) {
        let peer = match self.peer.borrow().as_ref().cloned() {
            Some(p) => p,
            None => {
                log::warn!("answer arrived before peer existed");
                return;
            }
        };
        let desc = RtcSessionDescriptionInit::new(RtcSdpType::Answer);
        desc.set_sdp(&sdp);
        if let Err(e) =
            wasm_bindgen_futures::JsFuture::from(peer.set_remote_description(&desc)).await
        {
            log::error!("set_remote_description(answer) failed: {:?}", e);
        }
    }

    async fn apply_remote_ice(
        self: Rc<Self>,
        candidate: String,
        sdp_mid: Option<String>,
        sdp_m_line_index: Option<u16>,
    ) {
        let peer = match self.peer.borrow().as_ref().cloned() {
            Some(p) => p,
            None => return,
        };
        let init = RtcIceCandidateInit::new(&candidate);
        if let Some(mid) = sdp_mid.as_deref() {
            init.set_sdp_mid(Some(mid));
        }
        if let Some(idx) = sdp_m_line_index {
            init.set_sdp_m_line_index(Some(idx));
        }
        let cand = match RtcIceCandidate::new(&init) {
            Ok(c) => c,
            Err(e) => {
                log::warn!("RtcIceCandidate::new failed: {:?}", e);
                return;
            }
        };
        if let Err(e) = wasm_bindgen_futures::JsFuture::from(
            peer.add_ice_candidate_with_opt_rtc_ice_candidate(Some(&cand)),
        )
        .await
        {
            log::warn!("add_ice_candidate failed: {:?}", e);
        }
    }
}

/// Wire a freshly-created DataChannel: log open/close, route binary messages
/// back through `handle_inbound`. Same shape for both channels.
fn wire_channel(inner: &Rc<TransportInner>, dc: &RtcDataChannel, label: &'static str) {
    // RtcDataChannel default binaryType is "blob", which requires async to
    // read. Switching to "arraybuffer" lets us read synchronously in
    // `bytes_from_event`. web-sys doesn't expose a typed setter for this on
    // RtcDataChannel, so we set the property via Reflect.
    let _ = js_sys::Reflect::set(dc, &"binaryType".into(), &"arraybuffer".into());
    let inner_msg = inner.clone();
    let on_msg = Closure::<dyn FnMut(MessageEvent)>::new(move |ev: MessageEvent| {
        if let Some(bytes) = bytes_from_event(&ev) {
            inner_msg.handle_inbound(&bytes);
        }
    });
    dc.set_onmessage(Some(on_msg.as_ref().unchecked_ref()));
    on_msg.forget();

    let on_open = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
        log::info!("rtc channel '{}' open", label);
    });
    dc.set_onopen(Some(on_open.as_ref().unchecked_ref()));
    on_open.forget();

    let on_close = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
        log::warn!("rtc channel '{}' closed", label);
    });
    dc.set_onclose(Some(on_close.as_ref().unchecked_ref()));
    on_close.forget();

    let on_err = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
        log::warn!("rtc channel '{}' error", label);
    });
    dc.set_onerror(Some(on_err.as_ref().unchecked_ref()));
    on_err.forget();
}

/// Pull a `Vec<u8>` out of a binary `MessageEvent`. Returns None if the
/// payload isn't an ArrayBuffer (which shouldn't happen since we set
/// `binaryType` on every channel/socket we create).
fn bytes_from_event(ev: &MessageEvent) -> Option<Vec<u8>> {
    let data = ev.data();
    let buf: js_sys::ArrayBuffer = data.dyn_into().ok()?;
    let arr = js_sys::Uint8Array::new(&buf);
    Some(arr.to_vec())
}

