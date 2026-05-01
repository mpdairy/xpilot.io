use std::cell::RefCell;
use std::collections::{BTreeMap, VecDeque};
use std::rc::Rc;

use shared::constants::{
    BULLET_MASS, SHIP_RESPAWN_SECONDS, TICK_DT_SECONDS, WASH_CONE_HALF_ANGLE_RAD,
    WASH_CONE_LENGTH, WASH_FORCE_AT_MOUTH,
};
use shared::entities::{forward, Bullet, EntityId, Ship};
use shared::math::Vec2;
use shared::protocol::{
    ClientKind, ClientMessage, GameEvent, RoomSummary, ServerMessage, Snapshot, TickInput,
    PROTOCOL_VERSION,
};
use shared::physics;
use shared::world::{apply_ship_dynamics, step_bullet, try_fire, wrap_pos, World};

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{
    CanvasRenderingContext2d, Document, HtmlButtonElement, HtmlCanvasElement, HtmlInputElement,
    HtmlSelectElement,
};

mod input;
mod particles;
mod render;
mod transport;

use input::InputState;
use particles::ParticleField;
use transport::Transport;

const MAX_ACCUMULATOR_SECONDS: f64 = 0.25;
const MAX_PREDICTED_BULLETS: usize = 64;
/// How long after a snapshot's arrival we'll keep projecting remote entities
/// forward by their last known velocity. ~6 ticks at 60Hz; a connection
/// stalled longer than this just freezes those entities until the next
/// snapshot lands. Way better than them flying off into space.
const MAX_EXTRAP_SECONDS: f32 = 0.10;
/// Radius around a `ShipDied` event in which we'll cull our own predicted
/// bullets — server already despawned its copy, and a predicted bullet that
/// flew past the explosion looks broken.
const PRED_BULLET_DEATH_CULL_RADIUS: f32 = 30.0;

struct App {
    canvas: HtmlCanvasElement,
    ctx: CanvasRenderingContext2d,
    input: Rc<InputState>,
    transport: Transport,
    local_tick: u32,
    /// Inputs we've sent but the server hasn't yet acknowledged in a snapshot.
    /// Replayed onto the local ship after each snapshot to keep prediction
    /// in lockstep with the server.
    unacked_inputs: VecDeque<TickInput>,
    accumulator: f64,
    last_time_ms: Option<f64>,
    game: Option<GameView>,
    status: String,
    /// Cached map names from the server's `AvailableMaps` push. Re-populated
    /// into the create-room dropdown whenever they arrive.
    available_maps: Vec<String>,
    /// Latch for the auto-join-on-first-RoomList behavior. Once we've sent
    /// the auto join/create, we don't repeat it on subsequent RoomList
    /// arrivals (e.g. user hits Refresh in the lobby) — those are
    /// user-initiated browses, not the cold start.
    auto_joined: bool,
    /// True until the first JoinedRoom arrives. Drives the "show lobby
    /// over the freshly-joined game" behavior so a new player sees the
    /// rooms list with the auto-joined match running behind it; subsequent
    /// joins (e.g. user picked a room and hit Join) skip straight into the
    /// game with no overlay.
    first_join_pending: bool,
    /// Room id we're currently in, for the "click Join on the room you're
    /// already in → close lobby instead of sending a no-op JoinRoom" UX.
    current_room_id: Option<u32>,
    /// Recent chat messages, oldest-first. Each entry holds a wall-clock
    /// arrival time so the renderer can fade old lines out and drop them
    /// once they're invisible.
    chat_log: VecDeque<render::ChatLine>,
    /// PlayerId of the player highlighted in the scoreboard. PgUp/PgDn
    /// move the cursor; default = local player. When pointed at someone
    /// else, the HUD shows a green direction dot toward that ship.
    /// `None` until the first snapshot tells us our own player_id.
    selected_player_id: Option<shared::entities::PlayerId>,
    /// Stack of recent personal kill / death events involving the local
    /// player. Oldest at the front (drawn at the bottom of the stack);
    /// newest pushes onto the back and stacks upward. Lines older than
    /// `PERSONAL_KILL_LIFETIME_MS` get dropped at render time.
    /// Mirrors the Elm version's `Hud.messages` list.
    personal_kills: VecDeque<PersonalKill>,
    /// Local player's current consecutive-kill count since spawn. Resets
    /// to 0 on each death. The Elm version had `lifeKills` for this but
    /// commented out the on-screen display — re-adding here per request.
    kill_streak: u32,
}

#[derive(Clone)]
struct PersonalKill {
    /// Just the other player's name (or "Yourself" for suicide). Color
    /// carries the "I killed / I died" meaning, no verb in the text.
    name: String,
    color: &'static str,
    t_ms: f64,
}

const CHAT_MAX_LINES: usize = 20;

struct TimedSnapshot {
    snap: Snapshot,
    /// Wall-clock ms when this landed at the client. Used to project remote
    /// entities forward by `vel * (now - arrival_ms)` until the next snapshot.
    arrival_ms: f64,
}

struct GameView {
    /// Holds the map plus a scratch ship/bullet view we rebuild each frame
    /// from `latest_snapshot`. Cross-frame state in here is irrelevant.
    world: World,
    local_ship: EntityId,
    /// Predicted state of OUR ship, advanced by apply_ship_dynamics on the
    /// live keyboard input and reconciled on each snapshot by replaying
    /// unacked inputs.
    predicted_local_ship: Option<Ship>,
    /// Our own bullets, predicted client-side so they appear at the muzzle
    /// instantly. The server's authoritative copies for our shooter id are
    /// filtered out in the render rebuild.
    predicted_local_bullets: Vec<Bullet>,
    particles: ParticleField,
    /// Newest snapshot we've received plus its arrival timestamp. We render
    /// remote entities at `snap.pos + snap.vel * (now - arrival_ms)` — no
    /// interp delay, no input-based extrapolation. RTT is the only added
    /// latency, which matches what the user sees in their browser ping.
    latest_snapshot: Option<TimedSnapshot>,
    /// Where to keep the camera frozen while waiting for respawn. Set on a
    /// ShipDied event for the local ship; cleared the moment the local ship
    /// reappears in a snapshot.
    death_camera_pos: Option<Vec2>,
    /// Wall-clock ms at the moment of the local death; drives the on-screen
    /// respawn countdown.
    death_time_ms: Option<f64>,
    /// Pre-rendered radar walls. Built once when JoinedRoom arrives so the
    /// per-frame mini-radar is one drawImage call instead of an N×M cell
    /// scan (matters for huge maps like newdarkhell, 200×200).
    radar_walls: Option<HtmlCanvasElement>,
}

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let _ = console_log::init_with_level(log::Level::Info);
    log::info!("xpilot.io client starting");

    let window = web_sys::window().ok_or("no window")?;
    let document = window.document().ok_or("no document")?;
    let canvas: HtmlCanvasElement = document
        .get_element_by_id("game")
        .ok_or("no #game canvas")?
        .dyn_into()?;
    let ctx: CanvasRenderingContext2d = canvas
        .get_context("2d")?
        .ok_or("no 2d context")?
        .dyn_into()?;

    let input = Rc::new(InputState::default());
    input::install_listeners(&window, input.clone())?;

    let url = ws_url(&window)?;
    log::info!("connecting to {}", url);

    let app: Rc<RefCell<Option<App>>> = Rc::new(RefCell::new(None));

    let app_msg = app.clone();
    let app_open = app.clone();
    let document_for_open = document.clone();
    let transport = Transport::connect(
        &url,
        move |msg: ServerMessage| {
            let mut guard = app_msg.borrow_mut();
            if let Some(a) = guard.as_mut() {
                handle_server_message(a, msg);
            }
        },
        move || {
            let guard = app_open.borrow();
            if let Some(a) = guard.as_ref() {
                let name = read_player_name(&document_for_open);
                a.transport.send(&ClientMessage::Hello {
                    name,
                    client_kind: ClientKind::Human,
                    supports_webrtc: true,
                    protocol_version: PROTOCOL_VERSION,
                });
                a.transport.send(&ClientMessage::ListRooms);
            }
        },
    )?;

    *app.borrow_mut() = Some(App {
        canvas,
        ctx,
        input,
        transport,
        local_tick: 0,
        unacked_inputs: VecDeque::with_capacity(128),
        accumulator: 0.0,
        last_time_ms: None,
        game: None,
        status: "connecting…".into(),
        available_maps: Vec::new(),
        auto_joined: false,
        first_join_pending: true,
        current_room_id: None,
        chat_log: VecDeque::with_capacity(CHAT_MAX_LINES),
        selected_player_id: None,
        personal_kills: VecDeque::with_capacity(8),
        kill_streak: 0,
    });

    load_saved_name(&window, &document);
    install_name_persistence(&window, &document, app.clone())?;
    set_ui_mode(&document, UiMode::LobbyOnly);
    install_lobby_handlers(&document, app.clone())?;
    install_lobby_overlay_handlers(&window, &document, app.clone())?;
    install_chat_handlers(&window, &document, app.clone())?;
    install_focus_clear(&window, app.clone())?;
    install_resize_handler(&window, app.clone())?;
    spawn_animation_loop(app);
    Ok(())
}

fn read_player_name(doc: &Document) -> String {
    doc.get_element_by_id("player-name")
        .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
        .map(|el| {
            let v = el.value().trim().to_string();
            if v.is_empty() { "Player".into() } else { v }
        })
        .unwrap_or_else(|| "Player".into())
}

const NAME_STORAGE_KEY: &str = "xpilot_name";

/// On startup, replace the empty/default value in #player-name with whatever
/// the player typed last session. Silent-no-op if localStorage is unavailable
/// (private mode, file://) — the input keeps its HTML default.
fn load_saved_name(window: &web_sys::Window, doc: &Document) {
    let Some(storage) = window.local_storage().ok().flatten() else { return };
    let Ok(Some(saved)) = storage.get_item(NAME_STORAGE_KEY) else { return };
    if saved.trim().is_empty() {
        return;
    }
    if let Some(input) = doc
        .get_element_by_id("player-name")
        .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
    {
        input.set_value(&saved);
    }
}

/// On every keystroke in #player-name: save to localStorage AND push the
/// current value to the server as `SetName`. Without the live server push
/// the rename only took effect after a reload (or any lobby action that
/// happened to re-send the name) — confusing for anyone trying to set
/// their name mid-game. Trim before saving so a stray space doesn't
/// survive across reloads. No debounce: keystrokes are small messages
/// and there are at most ~10/sec even when typing fast.
fn install_name_persistence(
    window: &web_sys::Window,
    doc: &Document,
    app: Rc<RefCell<Option<App>>>,
) -> Result<(), JsValue> {
    let Some(input) = doc
        .get_element_by_id("player-name")
        .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
    else {
        return Ok(());
    };
    let storage = match window.local_storage() {
        Ok(Some(s)) => s,
        _ => return Ok(()),
    };
    let input_for_cb = input.clone();
    let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_ev| {
        let v = input_for_cb.value();
        let trimmed = v.trim().to_string();
        let _ = storage.set_item(NAME_STORAGE_KEY, &trimmed);
        // Also push to the server so other players see the new name in
        // their next snapshot. Skip if the transport hasn't connected yet
        // — the Hello message picks up the current value.
        if !trimmed.is_empty() {
            if let Some(a) = app.borrow().as_ref() {
                if a.transport.is_open() {
                    a.transport.send(&ClientMessage::SetName { name: trimmed });
                }
            }
        }
    });
    input.add_event_listener_with_callback("input", cb.as_ref().unchecked_ref())?;
    cb.forget();
    Ok(())
}

/// UI is in one of three modes. The lobby was originally a page-replace
/// (lobby OR game), but adding "open lobby mid-game without leaving" pushed
/// it to a true overlay — so we encode the three valid states explicitly
/// rather than juggling individual hidden attributes per call site.
#[derive(Clone, Copy)]
enum UiMode {
    /// Initial state. Lobby fills the page, no game running behind.
    LobbyOnly,
    /// In a game, no lobby. Rooms button visible to bring the lobby back.
    GameOnly,
    /// In a game with the lobby overlaid on top — backdrop dims the game,
    /// X / Esc / click-outside dismiss back to GameOnly.
    GameWithLobby,
}

fn set_ui_mode(doc: &Document, mode: UiMode) {
    let (lobby, backdrop, close_x, game, rooms_btn) = match mode {
        UiMode::LobbyOnly => (true, false, false, false, false),
        UiMode::GameOnly => (false, false, false, true, true),
        UiMode::GameWithLobby => (true, true, true, true, false),
    };
    set_hidden(doc, "lobby", !lobby);
    set_hidden(doc, "lobby-backdrop", !backdrop);
    set_hidden(doc, "lobby-close", !close_x);
    set_hidden(doc, "game", !game);
    set_hidden(doc, "rooms-btn", !rooms_btn);
}

fn set_hidden(doc: &Document, id: &str, hidden: bool) {
    if let Some(el) = doc.get_element_by_id(id) {
        if hidden {
            el.set_attribute("hidden", "").ok();
        } else {
            el.remove_attribute("hidden").ok();
        }
    }
}

fn set_status(doc: &Document, msg: &str) {
    if let Some(el) = doc.get_element_by_id("status") {
        el.set_text_content(Some(msg));
    }
}

fn populate_map_options(doc: &Document, names: &[String]) {
    let Some(sel) = doc
        .get_element_by_id("new-room-map")
        .and_then(|el| el.dyn_into::<HtmlSelectElement>().ok())
    else {
        return;
    };
    sel.set_inner_html("");
    for name in names {
        if let Ok(opt) = doc.create_element("option") {
            opt.set_attribute("value", name).ok();
            opt.set_text_content(Some(name));
            let _ = sel.append_child(&opt);
        }
    }
}

fn render_room_list(doc: &Document, rooms: &[RoomSummary]) {
    let Some(body) = doc.get_element_by_id("room-list-body") else {
        return;
    };
    if rooms.is_empty() {
        body.set_inner_html(
            "<tr><td colspan=\"4\" class=\"empty\">no rooms — quick join will create one</td></tr>",
        );
        return;
    }
    let mut html = String::new();
    for r in rooms {
        let full = r.player_count >= r.cap;
        let count_class = if full { " class=\"full\"" } else { "" };
        let btn = if full {
            "<button disabled>full</button>".to_string()
        } else {
            format!(
                "<button class=\"join-btn\" data-room-id=\"{}\">Join</button>",
                r.room_id
            )
        };
        html.push_str(&format!(
            "<tr><td>{}</td><td>{}</td><td{}>{} / {}</td><td>{}</td></tr>",
            html_escape(&r.name),
            html_escape(&r.map_name),
            count_class,
            r.player_count,
            r.cap,
            btn
        ));
    }
    body.set_inner_html(&html);
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn install_lobby_handlers(
    doc: &Document,
    app: Rc<RefCell<Option<App>>>,
) -> Result<(), JsValue> {
    // Each lobby action re-syncs the current value of #player-name to the
    // server before sending the action — without this, the name typed after
    // the WS opened never reaches the server (Hello captured the default
    // "Player" at connect time).
    let send_msg_with_name = {
        let app = app.clone();
        let doc = doc.clone();
        move |msg: ClientMessage| {
            let guard = app.borrow();
            let Some(a) = guard.as_ref() else { return };
            let name = read_player_name(&doc);
            a.transport.send(&ClientMessage::SetName { name });
            a.transport.send(&msg);
        }
    };

    // Quick Join → JoinRoom { room_id: None }, unless the player is already
    // in *some* room — in that case the button just dismisses the lobby
    // (server would reject a second join anyway, and "Quick Join" while
    // mid-game intuitively means "back to my game").
    if let Some(btn) = doc
        .get_element_by_id("quick-join")
        .and_then(|el| el.dyn_into::<HtmlButtonElement>().ok())
    {
        let send = send_msg_with_name.clone();
        let app2 = app.clone();
        let doc2 = doc.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            let already_in_game = app2
                .borrow()
                .as_ref()
                .map(|a| a.current_room_id.is_some())
                .unwrap_or(false);
            if already_in_game {
                set_ui_mode(&doc2, UiMode::GameOnly);
            } else {
                send(ClientMessage::JoinRoom { room_id: None });
            }
        });
        btn.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }

    // Refresh → ListRooms (no name sync needed — server doesn't display
    // names in the lobby list)
    if let Some(btn) = doc
        .get_element_by_id("refresh")
        .and_then(|el| el.dyn_into::<HtmlButtonElement>().ok())
    {
        let app2 = app.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            let guard = app2.borrow();
            if let Some(a) = guard.as_ref() {
                a.transport.send(&ClientMessage::ListRooms);
            }
        });
        btn.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }

    // Create Room → CreateRoom { name, map_name, bot_count }
    if let Some(btn) = doc
        .get_element_by_id("create-room")
        .and_then(|el| el.dyn_into::<HtmlButtonElement>().ok())
    {
        let send = send_msg_with_name.clone();
        let doc_for_cb = doc.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            let name = doc_for_cb
                .get_element_by_id("new-room-name")
                .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
                .map(|el| el.value().trim().to_string())
                .unwrap_or_default();
            let map_name = doc_for_cb
                .get_element_by_id("new-room-map")
                .and_then(|el| el.dyn_into::<HtmlSelectElement>().ok())
                .map(|el| el.value())
                .unwrap_or_default();
            let bot_count = doc_for_cb
                .get_element_by_id("new-room-bots")
                .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
                .map(|el| el.value())
                .and_then(|v| v.parse::<u32>().ok())
                .unwrap_or(4)
                .min(8);
            if name.is_empty() {
                set_status(&doc_for_cb, "room name required");
                return;
            }
            if map_name.is_empty() {
                set_status(&doc_for_cb, "pick a map");
                return;
            }
            set_status(&doc_for_cb, "");
            send(ClientMessage::CreateRoom { name, map_name, bot_count });
        });
        btn.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }

    // Per-row Join button is delegated through the table body — buttons are
    // recreated each render so binding individual handlers would leak.
    // Special case: clicking Join on the room you're already in just
    // dismisses the lobby. Cleaner than sending a no-op JoinRoom.
    if let Some(tbody) = doc.get_element_by_id("room-list-body") {
        let send = send_msg_with_name;
        let app2 = app.clone();
        let doc2 = doc.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |ev: web_sys::Event| {
            let Some(target) = ev.target() else { return };
            let Some(el) = target.dyn_ref::<web_sys::Element>() else { return };
            if !el.class_list().contains("join-btn") {
                return;
            }
            let Some(rid_str) = el.get_attribute("data-room-id") else { return };
            let Ok(room_id) = rid_str.parse::<u32>() else { return };
            let current = app2
                .borrow()
                .as_ref()
                .and_then(|a| a.current_room_id);
            if current == Some(room_id) {
                set_ui_mode(&doc2, UiMode::GameOnly);
            } else {
                send(ClientMessage::JoinRoom { room_id: Some(room_id) });
            }
        });
        tbody.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }
    Ok(())
}

fn ws_url(window: &web_sys::Window) -> Result<String, JsValue> {
    let loc = window.location();
    let proto = loc.protocol().unwrap_or_else(|_| "http:".into());
    // Production (https): connect to the same host on /ws — Caddy reverse-
    // proxies the WebSocket upgrade to the game server on localhost:8080.
    // Local dev (http): connect directly to the game server on :8080.
    if proto == "https:" {
        let host = loc.host().unwrap_or_else(|_| "localhost".into());
        Ok(format!("wss://{}/ws", host))
    } else {
        let host = loc.hostname().unwrap_or_else(|_| "localhost".into());
        Ok(format!("ws://{}:8080/ws", host))
    }
}

fn perf_now() -> f64 {
    web_sys::window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0)
}

/// True if the given player_id is a server-spawned bot. Defaults to false
/// (human-ish) if the player isn't in the latest snapshot — better to
/// over-show a kill than miss one.
fn lookup_is_bot(app: &App, player_id: shared::entities::PlayerId) -> bool {
    app.game
        .as_ref()
        .and_then(|g| g.latest_snapshot.as_ref())
        .and_then(|ts| ts.snap.players.iter().find(|p| p.player_id == player_id))
        .map(|p| p.is_bot)
        .unwrap_or(false)
}

/// Look up a player's display name from the latest snapshot's PlayerInfo
/// list. Bots get a "(R)" suffix to distinguish from human players.
/// Falls back to "p<id>" if the player isn't in the current snapshot
/// (race between event delivery and snapshot — rare but possible).
fn lookup_player_name(app: &App, player_id: shared::entities::PlayerId) -> String {
    app.game
        .as_ref()
        .and_then(|g| g.latest_snapshot.as_ref())
        .and_then(|ts| ts.snap.players.iter().find(|p| p.player_id == player_id))
        .map(|p| {
            if p.is_bot {
                format!("{} (R)", p.name)
            } else {
                p.name.clone()
            }
        })
        .unwrap_or_else(|| format!("p{}", player_id))
}

/// Push a line into the chat log, evicting the oldest if at capacity.
fn push_chat_line(app: &mut App, author: String, text: String, kind: render::ChatKind) {
    if app.chat_log.len() == CHAT_MAX_LINES {
        app.chat_log.pop_front();
    }
    app.chat_log.push_back(render::ChatLine {
        author,
        text,
        kind,
        t_ms: perf_now(),
    });
}

/// Build a kill / death notification line in the classic XPilot style.
/// Picks a verb based on `t_ms` so consecutive kills don't all read the
/// same — same flavour the original used for its "smoked", "fragged",
/// "killed" messages. Suicide form when no killer is credited.
fn kill_message(killer: Option<&str>, victim: &str, t_ms: f64) -> String {
    let kill_verbs = ["killed", "fragged", "nailed", "blasted", "smoked", "wasted"];
    let suicide_verbs = ["crashed and burned", "ate dust", "self-destructed", "splattered"];
    let bucket = (t_ms as u64 / 17) as usize;
    match killer {
        Some(k) => {
            let v = kill_verbs[bucket % kill_verbs.len()];
            format!("{} {} {}", k, v, victim)
        }
        None => {
            let v = suicide_verbs[bucket % suicide_verbs.len()];
            format!("{} {}", victim, v)
        }
    }
}

fn install_focus_clear(
    window: &web_sys::Window,
    app: Rc<RefCell<Option<App>>>,
) -> Result<(), JsValue> {
    let app2 = app.clone();
    let blur = Closure::<dyn FnMut(web_sys::Event)>::new(move |_ev| {
        clear_and_notify_server(&app2);
    });
    window.add_event_listener_with_callback("blur", blur.as_ref().unchecked_ref())?;
    blur.forget();

    if let Some(document) = window.document() {
        let vis = Closure::<dyn FnMut(web_sys::Event)>::new(move |_ev| {
            clear_and_notify_server(&app);
        });
        document
            .add_event_listener_with_callback("visibilitychange", vis.as_ref().unchecked_ref())?;
        vis.forget();
    }
    Ok(())
}

/// Send the current canvas-sized viewport to the server. The server uses it
/// (plus AOI_MARGIN) to filter out bullets/particles outside our visible area
/// from snapshots — keeping bandwidth proportional to what we can actually
/// see on screen rather than the whole world. Cheap to send: only called on
/// JoinedRoom and on window resize (no zoom yet).
fn send_viewport(app: &App) {
    if !app.transport.is_open() {
        return;
    }
    let half_w = (app.canvas.width() as f32) * 0.5;
    let half_h = (app.canvas.height() as f32) * 0.5;
    app.transport.send(&ClientMessage::Viewport {
        half_width: half_w,
        half_height: half_h,
    });
}

/// Wires the in-game ways of bringing the lobby back up:
///   - "Rooms" button (top-left) → open
///   - Escape key → toggle
///   - X in the lobby corner → close
///   - clicking the backdrop → close
/// All four are no-ops before the player has joined a game (overlay only
/// makes sense once there's a game running behind it).
fn install_lobby_overlay_handlers(
    window: &web_sys::Window,
    document: &Document,
    app: Rc<RefCell<Option<App>>>,
) -> Result<(), JsValue> {
    let doc = document.clone();

    // Escape: toggle lobby — close if open, open if closed. Only acts when
    // a game is running; otherwise leaves the initial-lobby alone.
    let app_esc = app.clone();
    let doc_esc = doc.clone();
    let on_key = Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(move |ev: web_sys::KeyboardEvent| {
        // Skip everything when chat is open — chat owns its own Esc handling
        // and shouldn't lose keystrokes to the lobby toggle.
        if !is_hidden(&doc_esc, "chat-input") {
            return;
        }
        // Don't react if focus is in a form field (player-name input,
        // create-room name, etc.) — typing letters there shouldn't fly the ship.
        if active_is_form_field(&doc_esc) {
            return;
        }
        let guard = app_esc.borrow();
        let Some(a) = guard.as_ref() else { return };
        if a.game.is_none() {
            return;
        }
        let lobby_open = !is_hidden(&doc_esc, "lobby");
        let key = ev.key();
        let code = ev.code();
        // Ship-control keys close the lobby and let input.rs's listener
        // (which also reads window keydowns) start applying them to the
        // ship the same tick. So someone who joins and just wants to fly
        // can mash A/S/Shift/Space and the lobby gets out of the way.
        let is_ship_key = matches!(
            code.as_str(),
            "KeyA" | "KeyS" | "KeyX"
                | "ArrowLeft" | "ArrowRight" | "ArrowUp" | "ArrowDown"
                | "ShiftLeft" | "ShiftRight"
                | "Space" | "Enter"
                | "ControlLeft" | "ControlRight"
        );
        if key == "Escape" {
            if lobby_open {
                set_ui_mode(&doc_esc, UiMode::GameOnly);
            } else {
                set_ui_mode(&doc_esc, UiMode::GameWithLobby);
                a.transport.send(&ClientMessage::ListRooms);
            }
        } else if is_ship_key && lobby_open {
            set_ui_mode(&doc_esc, UiMode::GameOnly);
            // Don't prevent_default — input.rs's listener still gets this
            // event and the keypress drives the ship the same frame.
        }
    });
    window.add_event_listener_with_callback("keydown", on_key.as_ref().unchecked_ref())?;
    on_key.forget();

    // PageUp / PageDown — move the scoreboard target cursor. Same sort the
    // scoreboard uses (net kills desc), so the cursor visually steps row
    // by row. Skip while chat or a form field has focus so typing in the
    // chat / name fields isn't intercepted.
    let app_pg = app.clone();
    let doc_pg = doc.clone();
    let on_pg = Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(
        move |ev: web_sys::KeyboardEvent| {
            let key = ev.key();
            if key != "PageUp" && key != "PageDown" {
                return;
            }
            if !is_hidden(&doc_pg, "chat-input") {
                return;
            }
            if active_is_form_field(&doc_pg) {
                return;
            }
            let mut guard = app_pg.borrow_mut();
            let Some(a) = guard.as_mut() else { return };
            let Some(g) = a.game.as_ref() else { return };
            let Some(snap) = g.latest_snapshot.as_ref().map(|t| &t.snap) else {
                return;
            };
            // Sort same way the scoreboard does so cursor movement matches
            // what the user sees on the right.
            let mut sorted: Vec<&shared::protocol::PlayerInfo> = snap.players.iter().collect();
            sorted.sort_by_key(|p| -(p.kills as i64 - p.deaths as i64));
            if sorted.is_empty() {
                return;
            }
            let local_pid = snap
                .ships
                .iter()
                .find(|sh| sh.entity_id == g.local_ship)
                .map(|sh| sh.player_id);
            // Default cursor = local player; otherwise use whatever was
            // remembered (if it's still in the room).
            let current = a.selected_player_id.or(local_pid);
            let cur_idx = current
                .and_then(|pid| sorted.iter().position(|p| p.player_id == pid))
                .unwrap_or(0);
            let new_idx = match key.as_str() {
                "PageUp" => cur_idx.saturating_sub(1),
                "PageDown" => (cur_idx + 1).min(sorted.len() - 1),
                _ => cur_idx,
            };
            a.selected_player_id = Some(sorted[new_idx].player_id);
            ev.prevent_default(); // stop the browser from page-scrolling
        },
    );
    window.add_event_listener_with_callback("keydown", on_pg.as_ref().unchecked_ref())?;
    on_pg.forget();

    // Rooms button — only visible while in GameOnly mode (set_ui_mode hides
    // it whenever the lobby is up), so a click always means "open lobby".
    if let Some(btn) = doc
        .get_element_by_id("rooms-btn")
        .and_then(|el| el.dyn_into::<HtmlButtonElement>().ok())
    {
        let app2 = app.clone();
        let doc2 = doc.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            let guard = app2.borrow();
            let Some(a) = guard.as_ref() else { return };
            if a.game.is_none() {
                return;
            }
            set_ui_mode(&doc2, UiMode::GameWithLobby);
            a.transport.send(&ClientMessage::ListRooms);
        });
        btn.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }

    // Close X.
    if let Some(btn) = doc
        .get_element_by_id("lobby-close")
        .and_then(|el| el.dyn_into::<HtmlButtonElement>().ok())
    {
        let app2 = app.clone();
        let doc2 = doc.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            let guard = app2.borrow();
            let Some(a) = guard.as_ref() else { return };
            if a.game.is_some() {
                set_ui_mode(&doc2, UiMode::GameOnly);
            }
        });
        btn.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }

    // Backdrop click. Pure dismiss — clicks on the lobby itself stop here
    // because the backdrop is only the area outside the lobby.
    if let Some(bd) = doc.get_element_by_id("lobby-backdrop") {
        let app2 = app.clone();
        let doc2 = doc.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            let guard = app2.borrow();
            let Some(a) = guard.as_ref() else { return };
            if a.game.is_some() {
                set_ui_mode(&doc2, UiMode::GameOnly);
            }
        });
        bd.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }

    Ok(())
}

fn is_hidden(doc: &Document, id: &str) -> bool {
    doc.get_element_by_id(id)
        .map(|el| el.has_attribute("hidden"))
        .unwrap_or(true)
}

fn open_chat(doc: &Document) {
    if let Some(input) = doc
        .get_element_by_id("chat-input")
        .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
    {
        input.set_value("");
        input.remove_attribute("hidden").ok();
        let _ = input.focus();
    }
}

fn close_chat(doc: &Document) {
    if let Some(input) = doc
        .get_element_by_id("chat-input")
        .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
    {
        input.set_value("");
        input.set_attribute("hidden", "").ok();
        let _ = input.blur();
    }
}

/// Wires:
///   - "m" key → open chat compose (only in-game, not when typing in another field)
///   - chat-input Enter → send `ClientMessage::Chat` and close
///   - chat-input Escape → cancel and close
///   - chat-input blur → cancel and close (clicking elsewhere dismisses)
fn install_chat_handlers(
    window: &web_sys::Window,
    document: &Document,
    app: Rc<RefCell<Option<App>>>,
) -> Result<(), JsValue> {
    let doc = document.clone();

    // Window-level "m" — open chat. Skipped if any input/select/etc is
    // currently focused (so typing 'm' in the player-name field works) or if
    // the chat input itself is already up.
    let app_m = app.clone();
    let doc_m = doc.clone();
    let on_m = Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(move |ev: web_sys::KeyboardEvent| {
        if ev.key() != "m" && ev.key() != "M" {
            return;
        }
        if !is_hidden(&doc_m, "chat-input") {
            return; // already open
        }
        if active_is_form_field(&doc_m) {
            return;
        }
        let guard = app_m.borrow();
        let Some(a) = guard.as_ref() else { return };
        if a.game.is_none() {
            return;
        }
        // Don't let the literal "m" leak into the freshly-focused input.
        ev.prevent_default();
        open_chat(&doc_m);
    });
    window.add_event_listener_with_callback("keydown", on_m.as_ref().unchecked_ref())?;
    on_m.forget();

    // Chat input keydown — Enter sends, Escape cancels. stop_propagation so
    // these don't trigger the global Escape→lobby toggle or the shipboard
    // input handlers.
    if let Some(input) = doc
        .get_element_by_id("chat-input")
        .and_then(|el| el.dyn_into::<HtmlInputElement>().ok())
    {
        let app_in = app.clone();
        let doc_in = doc.clone();
        let input_for_cb = input.clone();
        let cb = Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(
            move |ev: web_sys::KeyboardEvent| match ev.key().as_str() {
                "Enter" => {
                    ev.prevent_default();
                    ev.stop_propagation();
                    let text = input_for_cb.value();
                    let trimmed = text.trim();
                    if !trimmed.is_empty() {
                        let guard = app_in.borrow();
                        if let Some(a) = guard.as_ref() {
                            a.transport.send(&ClientMessage::Chat {
                                text: trimmed.to_string(),
                            });
                        }
                    }
                    close_chat(&doc_in);
                }
                "Escape" => {
                    ev.prevent_default();
                    ev.stop_propagation();
                    close_chat(&doc_in);
                }
                _ => {}
            },
        );
        input.add_event_listener_with_callback("keydown", cb.as_ref().unchecked_ref())?;
        cb.forget();

        // Blur: if the user clicks somewhere else, close. Avoids a stuck
        // input when they decide not to send.
        let doc_blur = doc.clone();
        let on_blur = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            close_chat(&doc_blur);
        });
        input.add_event_listener_with_callback("blur", on_blur.as_ref().unchecked_ref())?;
        on_blur.forget();
    }

    Ok(())
}

/// True if focus is currently in any form-field-like element. Used to gate
/// the global "m" handler so typing 'm' in the player-name field doesn't
/// also pop chat.
fn active_is_form_field(doc: &Document) -> bool {
    let Some(active) = doc.active_element() else { return false };
    matches!(
        active.tag_name().as_str(),
        "INPUT" | "TEXTAREA" | "SELECT" | "BUTTON"
    )
}

fn install_resize_handler(
    window: &web_sys::Window,
    app: Rc<RefCell<Option<App>>>,
) -> Result<(), JsValue> {
    let resize = Closure::<dyn FnMut(web_sys::Event)>::new(move |_ev| {
        let guard = app.borrow();
        if let Some(a) = guard.as_ref() {
            // Only meaningful once in-game; harmless before that (server
            // ignores Viewport from a player not in a room).
            if a.game.is_some() {
                send_viewport(a);
            }
        }
    });
    window.add_event_listener_with_callback("resize", resize.as_ref().unchecked_ref())?;
    resize.forget();
    Ok(())
}

fn clear_and_notify_server(app_cell: &Rc<RefCell<Option<App>>>) {
    let mut guard = app_cell.borrow_mut();
    let Some(app) = guard.as_mut() else { return };
    app.input.clear();
    if !app.transport.is_open() {
        return;
    }
    let zero = TickInput {
        client_tick: app.local_tick,
        ..Default::default()
    };
    app.transport.send(&ClientMessage::Input(zero));
    app.unacked_inputs.push_back(zero);
    while app.unacked_inputs.len() > 256 {
        app.unacked_inputs.pop_front();
    }
    if let Some(g) = app.game.as_mut() {
        if let Some(ship) = g.predicted_local_ship.as_mut() {
            apply_ship_dynamics(ship, &zero, &g.world.map);
        }
    }
    app.local_tick = app.local_tick.wrapping_add(1);
}

fn handle_server_message(app: &mut App, msg: ServerMessage) {
    let doc = web_sys::window().and_then(|w| w.document());
    match msg {
        ServerMessage::Welcome {
            player_id,
            server_tick,
            protocol_version,
        } => {
            log::info!(
                "welcome: pid={} server_tick={} server_proto={}",
                player_id,
                server_tick,
                protocol_version
            );
            app.status = "in lobby".into();
            if let Some(d) = &doc {
                set_status(d, "");
            }
        }
        ServerMessage::AvailableMaps { names } => {
            log::info!("available maps: {:?}", names);
            app.available_maps = names.clone();
            if let Some(d) = &doc {
                populate_map_options(d, &names);
            }
        }
        ServerMessage::RoomList { rooms } => {
            log::debug!("room list: {} rooms", rooms.len());
            if let Some(d) = &doc {
                render_room_list(d, &rooms);
            }
            // Auto-join on first RoomList: hop straight into a non-full room
            // if any exists, else create one with the tournament map and 4
            // bots. Latched so a later Refresh doesn't try to join again.
            if !app.auto_joined && app.game.is_none() {
                app.auto_joined = true;
                let name = doc
                    .as_ref()
                    .map(|d| read_player_name(d))
                    .unwrap_or_else(|| "Player".into());
                app.transport.send(&ClientMessage::SetName { name });
                let target = rooms.iter().find(|r| r.player_count < r.cap);
                match target {
                    Some(r) => {
                        log::info!("auto-joining room {} ({})", r.room_id, r.name);
                        app.transport.send(&ClientMessage::JoinRoom {
                            room_id: Some(r.room_id),
                        });
                    }
                    None => {
                        log::info!("no rooms — auto-creating tournament w/ 4 bots");
                        app.transport.send(&ClientMessage::CreateRoom {
                            name: "auto".into(),
                            map_name: "tournament".into(),
                            bot_count: 4,
                        });
                    }
                }
            }
        }
        ServerMessage::JoinedRoom { room_id, map, players, your_ship_id } => {
            log::info!(
                "joined room {} as ship {} ({} players)",
                room_id,
                your_ship_id,
                players.len()
            );
            app.current_room_id = Some(room_id);
            let world = World::new(map);
            let radar_walls = doc
                .as_ref()
                .and_then(|d| render::build_radar_walls(d, &world.map));
            app.game = Some(GameView {
                world,
                local_ship: your_ship_id,
                predicted_local_ship: None,
                predicted_local_bullets: Vec::with_capacity(MAX_PREDICTED_BULLETS),
                particles: ParticleField::new(),
                latest_snapshot: None,
                death_camera_pos: None,
                death_time_ms: None,
                radar_walls,
            });
            app.unacked_inputs.clear();
            app.local_tick = 0;
            app.status = String::new();
            if let Some(d) = &doc {
                // First join of the session: leave the lobby up so the player
                // can see the room list with the auto-joined match running
                // behind it. Subsequent joins (manual room pick) snap right
                // into gameplay.
                let mode = if app.first_join_pending {
                    app.first_join_pending = false;
                    app.transport.send(&ClientMessage::ListRooms);
                    UiMode::GameWithLobby
                } else {
                    UiMode::GameOnly
                };
                set_ui_mode(d, mode);
            }
            // First Viewport — server uses this immediately for AOI culling.
            // Until it arrives, the server sends unfiltered snapshots.
            send_viewport(app);
        }
        ServerMessage::PlayerJoined(p) => {
            log::info!("player joined: {} ({})", p.name, p.player_id);
        }
        ServerMessage::PlayerLeft(pid) => {
            log::info!("player left: {}", pid);
        }
        ServerMessage::Snapshot(snap) => {
            let Some(g) = app.game.as_mut() else { return };

            // Drop unacked inputs the server has now processed.
            while let Some(front) = app.unacked_inputs.front() {
                if front.client_tick <= snap.your_last_processed_input {
                    app.unacked_inputs.pop_front();
                } else {
                    break;
                }
            }

            // Re-anchor predicted local ship from the snapshot, then replay
            // unacked inputs to advance it back to "now".
            let local_in_snap = snap
                .ships
                .iter()
                .find(|s| s.entity_id == g.local_ship)
                .cloned();
            if let Some(mut local) = local_in_snap {
                for input in &app.unacked_inputs {
                    apply_ship_dynamics(&mut local, input, &g.world.map);
                }
                g.predicted_local_ship = Some(local);
                // Respawned (or first spawn): clear death-state UI.
                g.death_camera_pos = None;
                g.death_time_ms = None;
            } else {
                g.predicted_local_ship = None;
            }

            // Out-of-order arrivals (possible once we move to UDP-ish
            // datachannels): drop anything older than what we already have.
            if let Some(prev) = &g.latest_snapshot {
                if snap.server_tick <= prev.snap.server_tick {
                    return;
                }
            }
            g.latest_snapshot = Some(TimedSnapshot {
                snap,
                arrival_ms: perf_now(),
            });
        }
        ServerMessage::Event(ev) => {
            // Chat is the only event that doesn't require an active GameView
            // — incoming chats are kept even if the snapshot pump hasn't
            // populated the world yet (e.g. immediately after JoinedRoom).
            if let GameEvent::Chat { player_id, ref text } = ev {
                let author = lookup_player_name(app, player_id);
                push_chat_line(app, author, text.clone(), render::ChatKind::Player);
            }
            // Kill notifications: only push to the chat log if a HUMAN was
            // involved — bot-on-bot fights would otherwise drown the chat.
            // Always update personal stats (last_personal_kill + streak)
            // when the local player is the killer or victim.
            if let GameEvent::ShipDied {
                victim_player_id,
                killer,
                ..
            } = ev
            {
                let victim_is_bot = lookup_is_bot(app, victim_player_id);
                let killer_is_bot = killer.map(|k| lookup_is_bot(app, k));
                let victim_name = lookup_player_name(app, victim_player_id);
                let killer_name = killer.map(|k| lookup_player_name(app, k));
                let local_pid = app.game.as_ref().and_then(|g| {
                    g.latest_snapshot
                        .as_ref()
                        .and_then(|ts| {
                            ts.snap
                                .ships
                                .iter()
                                .find(|sh| sh.entity_id == g.local_ship)
                        })
                        .map(|sh| sh.player_id)
                });

                // Human-involved filter: human victim OR (human killer AND
                // killer != victim, so suicides count as one party).
                let any_human = !victim_is_bot
                    || killer_is_bot.map(|b| !b).unwrap_or(false);
                if any_human {
                    let text = match killer {
                        Some(k) if k != victim_player_id => kill_message(
                            killer_name.as_deref(),
                            &victim_name,
                            perf_now(),
                        ),
                        _ => kill_message(None, &victim_name, perf_now()),
                    };
                    push_chat_line(app, String::new(), text, render::ChatKind::Kill);
                }

                // Personal stats — ports the Elm version's `hudMessage`
                // colour map exactly: green for kills, yellow for deaths
                // by enemy fire, dark red for suicide ("Yourself").
                if let Some(me) = local_pid {
                    let new_pk = if victim_player_id == me {
                        app.kill_streak = 0;
                        match killer {
                            Some(k) if k != me => Some(PersonalKill {
                                name: killer_name
                                    .clone()
                                    .unwrap_or_else(|| "?".into()),
                                color: "#aaaa00",
                                t_ms: perf_now(),
                            }),
                            // killer == me, or no killer (wall crash) →
                            // both read as "you did this to yourself".
                            _ => Some(PersonalKill {
                                name: "Yourself".into(),
                                color: "#aa0000",
                                t_ms: perf_now(),
                            }),
                        }
                    } else if killer == Some(me) {
                        app.kill_streak = app.kill_streak.saturating_add(1);
                        Some(PersonalKill {
                            name: victim_name.clone(),
                            color: "#008000",
                            t_ms: perf_now(),
                        })
                    } else {
                        None
                    };
                    if let Some(pk) = new_pk {
                        // Match the Elm `hudMessage`: append to the END of
                        // the list. Renderer iterates with index for the
                        // y = bottom - 15*i layout.
                        if app.personal_kills.len() >= 8 {
                            app.personal_kills.pop_front();
                        }
                        app.personal_kills.push_back(pk);
                    }
                }
            }
            if let Some(g) = app.game.as_mut() {
                if let GameEvent::ShipDied { entity_id, pos, .. } = ev {
                    // Explosion particles now come from the server via
                    // snapshot.particles — no local spawn needed.
                    if entity_id == g.local_ship {
                        g.death_camera_pos = Some(pos);
                        g.death_time_ms = Some(perf_now());
                    }
                    // Cull our predicted bullets close to the death — server
                    // already despawned its copy when the kill landed; without
                    // this, our predicted bullet keeps flying past the
                    // explosion looking broken.
                    let cull_r2 = PRED_BULLET_DEATH_CULL_RADIUS
                        * PRED_BULLET_DEATH_CULL_RADIUS;
                    g.predicted_local_bullets.retain(|b| {
                        let dx = b.pos.x - pos.x;
                        let dy = b.pos.y - pos.y;
                        dx * dx + dy * dy > cull_r2
                    });
                }
            }
        }
        ServerMessage::Error { message } => {
            log::error!("server error: {}", message);
            app.status = format!("server error: {}", message);
            if let Some(d) = &doc {
                set_status(d, &message);
            }
        }
        ServerMessage::RtcOffer { .. }
        | ServerMessage::RtcAnswer { .. }
        | ServerMessage::RtcIceCandidate { .. } => {}
    }
}

fn spawn_animation_loop(app: Rc<RefCell<Option<App>>>) {
    let f: Rc<RefCell<Option<Closure<dyn FnMut(f64)>>>> = Rc::new(RefCell::new(None));
    let g = f.clone();
    let app2 = app.clone();
    *g.borrow_mut() = Some(Closure::wrap(Box::new(move |timestamp: f64| {
        frame(&app2, timestamp);
        let win = web_sys::window().expect("window");
        win.request_animation_frame(f.borrow().as_ref().unwrap().as_ref().unchecked_ref())
            .expect("rAF");
    }) as Box<dyn FnMut(f64)>));
    let win = web_sys::window().expect("window");
    win.request_animation_frame(g.borrow().as_ref().unwrap().as_ref().unchecked_ref())
        .expect("rAF initial");
}

fn frame(app_cell: &Rc<RefCell<Option<App>>>, timestamp_ms: f64) {
    let mut guard = app_cell.borrow_mut();
    let Some(app) = guard.as_mut() else { return };

    let dt_seconds = match app.last_time_ms {
        Some(prev) => ((timestamp_ms - prev) / 1000.0).max(0.0),
        None => 0.0,
    };
    app.last_time_ms = Some(timestamp_ms);

    app.accumulator += dt_seconds;
    if app.accumulator > MAX_ACCUMULATOR_SECONDS {
        app.accumulator = MAX_ACCUMULATOR_SECONDS;
    }

    let step_dt = TICK_DT_SECONDS as f64;
    while app.accumulator >= step_dt {
        do_local_tick(app);
        app.accumulator -= step_dt;
    }

    // Build the renderable world from the newest snapshot, projecting
    // ships/bullets forward by `vel * (now - arrival_ms)`. No interp delay,
    // no input-based extrapolation — the latter is what was wobbling before.
    if let Some(g) = app.game.as_mut() {
        rebuild_render_world(g, timestamp_ms);
    }

    if let Some(g) = app.game.as_mut() {
        let ships: Vec<_> = g.world.ships.values().cloned().collect();
        let walls = g.world.map.walls.clone();
        g.particles.step(ships.iter(), &walls, dt_seconds as f32);
    }

    if let Some(g) = app.game.as_ref() {
        let alive_pos = g.predicted_local_ship.as_ref().map(|s| s.pos);
        // Follow predicted ship while alive; freeze on the death spot during
        // the respawn timer; fall back to map center before either happens.
        let follow = alive_pos
            .or(g.death_camera_pos)
            .unwrap_or_else(|| Vec2::new(g.world.map.width * 0.5, g.world.map.height * 0.5));
        let newest = g.latest_snapshot.as_ref().map(|t| &t.snap);
        let players: Vec<_> = newest.map(|s| s.players.clone()).unwrap_or_default();
        let local_alive = alive_pos.is_some();
        let local_pid = newest
            .and_then(|s| s.ships.iter().find(|sh| sh.entity_id == g.local_ship))
            .map(|sh| sh.player_id);
        let local_charge = g.predicted_local_ship.as_ref().map(|s| s.shot_charge);
        // Respawn countdown: how much real time is left. None when alive.
        let respawn_in = g.death_time_ms.map(|t| {
            let elapsed = ((timestamp_ms - t) / 1000.0) as f32;
            (SHIP_RESPAWN_SECONDS - elapsed).max(0.0)
        });
        // Cursor target = whatever PgUp/PgDn picked, defaulting to local
        // player. World position resolved via the snapshot's ships list —
        // None if the target's ship isn't currently alive (don't draw the
        // direction dot in that case).
        let cursor_pid = app.selected_player_id.or(local_pid);
        let cursor_target_pos = cursor_pid.and_then(|pid| {
            newest
                .and_then(|s| s.ships.iter().find(|sh| sh.player_id == pid))
                .map(|sh| sh.pos)
        });
        render::render(
            &app.ctx,
            &app.canvas,
            &g.world,
            &g.particles,
            follow,
            &players,
            local_pid,
            local_alive,
            local_charge,
            respawn_in,
            g.radar_walls.as_ref(),
            &app.chat_log,
            timestamp_ms,
            cursor_pid,
            cursor_target_pos,
            // Personal-kill stack: borrowed slice of (name, color, age).
            // Renderer drops the ones older than its lifetime and stacks
            // the rest upward from the HUD bottom dash.
            &app
                .personal_kills
                .iter()
                .map(|pk| render::PersonalKillView {
                    name: &pk.name,
                    color: pk.color,
                    age_ms: timestamp_ms - pk.t_ms,
                })
                .collect::<Vec<_>>(),
            app.kill_streak,
        );
    } else {
        render::render_status(&app.ctx, &app.canvas, &app.status);
    }
}

/// Pure rebuild from the newest snapshot. Each remote ship/bullet is placed
/// at `snap.pos + snap.vel * elapsed_since_arrival`, capped at
/// `MAX_EXTRAP_SECONDS` so a stalled connection freezes them in place rather
/// than launching them into the void. No interp delay, no input-based
/// extrapolation (which was the source of the wobble), no event buffering.
/// Predicted local ship + bullets are overlaid on top.
fn rebuild_render_world(g: &mut GameView, now_ms: f64) {
    let GameView {
        world,
        local_ship,
        latest_snapshot,
        predicted_local_ship,
        predicted_local_bullets,
        ..
    } = g;

    let local_id = *local_ship;

    let Some(ts) = latest_snapshot else {
        world.ships.clear();
        world.bullets.clear();
        return;
    };

    let elapsed = (((now_ms - ts.arrival_ms) / 1000.0) as f32).clamp(0.0, MAX_EXTRAP_SECONDS);

    let mut ships = BTreeMap::new();
    for s in &ts.snap.ships {
        let mut ship = s.clone();
        ship.pos = Vec2::new(ship.pos.x + ship.vel.x * elapsed, ship.pos.y + ship.vel.y * elapsed);
        ships.insert(ship.entity_id, ship);
    }

    let mut bullets = BTreeMap::new();
    for b in &ts.snap.bullets {
        if b.shooter == local_id {
            continue; // we render the predicted version
        }
        let mut bullet = b.clone();
        bullet.pos = Vec2::new(
            bullet.pos.x + bullet.vel.x * elapsed,
            bullet.pos.y + bullet.vel.y * elapsed,
        );
        bullets.insert(bullet.entity_id, bullet);
    }

    // Server-authored explosion particles. Just project forward by vel like
    // ships/bullets — they don't need their own re-sim on the client, and any
    // bonus motion past the snapshot tick is corrected on the next snapshot.
    let mut particles = Vec::with_capacity(ts.snap.particles.len());
    for p in &ts.snap.particles {
        let mut p = *p;
        p.pos = Vec2::new(p.pos.x + p.vel.x * elapsed, p.pos.y + p.vel.y * elapsed);
        particles.push(p);
    }

    world.ships = ships;
    world.bullets = bullets;
    world.particles = particles;
    world.tick = ts.snap.server_tick;

    // Sync cannon alive flags from the server. The client doesn't simulate
    // cannons; without this, every cannon would render as alive forever even
    // after the server killed it. `dead_cannons` is empty when all alive.
    let dead: std::collections::BTreeSet<(u32, u32)> =
        ts.snap.dead_cannons.iter().copied().collect();
    for (cell, cannon) in world.cannons.iter_mut() {
        cannon.alive = !dead.contains(cell);
    }

    if let Some(local) = predicted_local_ship.clone() {
        world.ships.insert(local_id, local);
    }

    // Cull predicted bullets that have entered any remote ship — server has
    // its own copy and despawns on hit, so without this our predicted bullets
    // visibly fly through kills.
    predicted_local_bullets.retain(|b| {
        for (id, ship) in world.ships.iter() {
            if *id == local_id {
                continue;
            }
            if physics::point_in_triangle(b.pos, &ship.world_vertices()) {
                return false;
            }
        }
        true
    });

    for (i, b) in predicted_local_bullets.iter().enumerate() {
        let id = u32::MAX - i as u32;
        world.bullets.insert(id, b.clone());
    }
}

/// Local fixed tick: send our input, advance our predicted ship, step our
/// predicted local bullets. Does NOT touch remote ship state — that's
/// rebuilt fresh each frame from the snapshot.
fn do_local_tick(app: &mut App) {
    let Some(g) = app.game.as_mut() else { return };
    let input = app.input.snapshot(app.local_tick);

    if app.transport.is_open() {
        app.transport.send(&ClientMessage::Input(input));
    }
    app.unacked_inputs.push_back(input);
    while app.unacked_inputs.len() > 256 {
        app.unacked_inputs.pop_front();
    }

    if let Some(ship) = g.predicted_local_ship.as_mut() {
        apply_ship_dynamics(ship, &input, &g.world.map);
        if let Some(nb) = try_fire(ship, &input, &g.world.map) {
            g.predicted_local_bullets.push(Bullet {
                entity_id: 0,
                shooter: nb.shooter,
                pos: nb.pos,
                vel: nb.vel,
                mass: BULLET_MASS,
                age_seconds: 0.0,
            });
            while g.predicted_local_bullets.len() > MAX_PREDICTED_BULLETS {
                g.predicted_local_bullets.remove(0);
            }
        }
    }

    // Apply our own thruster wash to our predicted bullets — same as the
    // server does for the authoritative copies. Without this, you can't see
    // your own bullets get pushed when you overtake them.
    if let Some(ship) = g.predicted_local_ship.as_ref() {
        if ship.thrusting {
            let apex = ship.pos;
            let axis = forward(ship.angle) * -1.0;
            for bullet in g.predicted_local_bullets.iter_mut() {
                if let Some(force) = physics::cone_force(
                    bullet.pos,
                    apex,
                    axis,
                    WASH_CONE_LENGTH,
                    WASH_CONE_HALF_ANGLE_RAD,
                    WASH_FORCE_AT_MOUTH,
                ) {
                    bullet.vel += force * TICK_DT_SECONDS;
                }
            }
        }
    }

    g.predicted_local_bullets.retain_mut(|b| {
        if step_bullet(b, &g.world.map) {
            return false;
        }
        if g.world.map.edge_wrap {
            b.pos = wrap_pos(b.pos, g.world.map.width, g.world.map.height);
        }
        true
    });

    app.local_tick = app.local_tick.wrapping_add(1);
}
