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
}

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
    });

    show_lobby(&document, true);
    install_lobby_handlers(&document, app.clone())?;
    install_focus_clear(&window, app.clone())?;
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

fn show_lobby(doc: &Document, lobby_visible: bool) {
    if let Some(el) = doc.get_element_by_id("lobby") {
        if lobby_visible {
            el.remove_attribute("hidden").ok();
        } else {
            el.set_attribute("hidden", "").ok();
        }
    }
    if let Some(el) = doc.get_element_by_id("game") {
        if lobby_visible {
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

    // Quick Join → JoinRoom { room_id: None }
    if let Some(btn) = doc
        .get_element_by_id("quick-join")
        .and_then(|el| el.dyn_into::<HtmlButtonElement>().ok())
    {
        let send = send_msg_with_name.clone();
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| {
            send(ClientMessage::JoinRoom { room_id: None });
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
    if let Some(tbody) = doc.get_element_by_id("room-list-body") {
        let send = send_msg_with_name;
        let cb = Closure::<dyn FnMut(web_sys::Event)>::new(move |ev: web_sys::Event| {
            let Some(target) = ev.target() else { return };
            let Some(el) = target.dyn_ref::<web_sys::Element>() else { return };
            if !el.class_list().contains("join-btn") {
                return;
            }
            let Some(rid_str) = el.get_attribute("data-room-id") else { return };
            let Ok(room_id) = rid_str.parse::<u32>() else { return };
            send(ClientMessage::JoinRoom { room_id: Some(room_id) });
        });
        tbody.add_event_listener_with_callback("click", cb.as_ref().unchecked_ref())?;
        cb.forget();
    }
    Ok(())
}

fn ws_url(window: &web_sys::Window) -> Result<String, JsValue> {
    let loc = window.location();
    let host = loc.hostname().unwrap_or_else(|_| "localhost".to_string());
    Ok(format!("ws://{}:8080/ws", host))
}

fn perf_now() -> f64 {
    web_sys::window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0)
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
        }
        ServerMessage::JoinedRoom { room_id, map, players, your_ship_id } => {
            log::info!(
                "joined room {} as ship {} ({} players)",
                room_id,
                your_ship_id,
                players.len()
            );
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
                show_lobby(d, false);
            }
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
