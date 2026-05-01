use shared::constants::SHOT_CHARGE_MAX;
use shared::entities::{forward, EntityId, Ship};
use shared::map::{Block, Map};
use shared::math::Vec2;
use shared::protocol::PlayerInfo;
use shared::world::World;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{CanvasRenderingContext2d, Document, HtmlCanvasElement};

use crate::particles::ParticleField;

/// Cap on the radar's longer axis (px). Maps wider than they are tall hit
/// the width cap and vice versa.
const RADAR_MAX: f64 = 200.0;
const RADAR_PAD: f64 = 10.0;

/// One chat message in the on-screen log. Fades after a long visible
/// window. Owner (lib.rs) caps queue length.
pub struct ChatLine {
    pub author: String,
    pub text: String,
    pub kind: ChatKind,
    /// `performance.now()` ms when this landed.
    pub t_ms: f64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ChatKind {
    /// A chat message typed by a player.
    Player,
    /// A server-generated kill / death notification. Drawn in a
    /// distinct colour so it doesn't blend with player chatter.
    Kill,
}

/// Borrowed view of one entry in `App.personal_kills`. Just the player
/// name + already-resolved colour + age — the renderer doesn't need a
/// clock or any further lookup.
pub struct PersonalKillView<'a> {
    pub name: &'a str,
    pub color: &'static str,
    pub age_ms: f64,
}

/// Display schedule for chat lines: full alpha for `CHAT_VISIBLE_MS`, then
/// linear fade over `CHAT_FADE_MS`, then dropped from the on-screen log.
/// 60 s + 5 s fade — sticks around about a minute so a player who looked
/// away can scan back through what happened.
const CHAT_VISIBLE_MS: f64 = 60_000.0;
const CHAT_FADE_MS: f64 = 5_000.0;
/// Cap on rendered lines. The queue itself can be longer; this just limits
/// how many ever appear on-screen at once.
const CHAT_DISPLAY_MAX: usize = 8;

pub struct Camera {
    pub center: Vec2,
    pub viewport: Vec2,
    /// `Some((world_w, world_h))` for toroidal maps. World→screen for
    /// entities (`project_entity`) shifts the point by ±world dim along each
    /// axis so it lands within half-a-world of the camera, making the seam
    /// invisible. Block iteration handles the seam by extending its loop
    /// bounds outside the grid and modding for the lookup.
    pub wrap_dims: Option<(f32, f32)>,
}

impl Camera {
    pub fn world_to_screen(&self, p: Vec2) -> (f64, f64) {
        let dx = (p.x - self.center.x) as f64 + self.viewport.x as f64 * 0.5;
        let dy = (p.y - self.center.y) as f64 + self.viewport.y as f64 * 0.5;
        (dx, dy)
    }

    /// Returns `p` snapped to the wrapped instance closest to the camera.
    /// No-op for non-wrap maps.
    pub fn nearest(&self, p: Vec2) -> Vec2 {
        let Some((w, h)) = self.wrap_dims else { return p };
        let mut x = p.x;
        let dx = x - self.center.x;
        if dx > w * 0.5 {
            x -= w;
        } else if dx < -w * 0.5 {
            x += w;
        }
        let mut y = p.y;
        let dy = y - self.center.y;
        if dy > h * 0.5 {
            y -= h;
        } else if dy < -h * 0.5 {
            y += h;
        }
        Vec2::new(x, y)
    }

    /// Project an entity (ship / bullet / particle) to screen, accounting
    /// for wrap so the entity appears on the visually-correct side of the
    /// seam relative to the camera.
    pub fn project_entity(&self, p: Vec2) -> (f64, f64) {
        self.world_to_screen(self.nearest(p))
    }
}

pub fn render(
    ctx: &CanvasRenderingContext2d,
    canvas: &HtmlCanvasElement,
    world: &World,
    particles: &ParticleField,
    follow: Vec2,
    players: &[PlayerInfo],
    local_player_id: Option<EntityId>,
    local_alive: bool,
    local_shot_charge: Option<f32>,
    respawn_remaining_seconds: Option<f32>,
    radar_walls: Option<&HtmlCanvasElement>,
    chat_log: &std::collections::VecDeque<ChatLine>,
    now_ms: f64,
    // cursor_player_id: scoreboard cursor target (defaults to local player).
    // Drives the `>` marker on the scoreboard row and the HUD direction dot.
    // cursor_target_pos: world pos of the cursor's target ship, when alive.
    // None hides the direction dot.
    cursor_player_id: Option<EntityId>,
    cursor_target_pos: Option<Vec2>,
    // Stack of recent personal kill / death events. Entries are
    // oldest-first (matches the Elm `Hud.messages` order). Renderer drops
    // entries older than `PERSONAL_KILL_LIFETIME_MS` and stacks the rest
    // upward from the HUD bottom dash.
    personal_kills: &[PersonalKillView],
    // Local player's current consecutive-kill streak since spawn.
    kill_streak: u32,
) {
    let w = canvas.width() as f64;
    let h = canvas.height() as f64;
    let camera = Camera {
        center: follow,
        viewport: Vec2::new(w as f32, h as f32),
        wrap_dims: if world.map.edge_wrap {
            Some((world.map.width, world.map.height))
        } else {
            None
        },
    };

    // Background.
    ctx.set_fill_style_str("#000");
    ctx.fill_rect(0.0, 0.0, w, h);

    // Walls — block-style render (classic XPilot look). Each wall cell draws
    // its full outline so adjacent cells make a visible grid; falls back to
    // the line-segment list for hand-built test maps without a block grid.
    ctx.set_stroke_style_str("#5588ff");
    ctx.set_line_width(2.0);
    ctx.begin_path();
    if let Some(grid) = world.map.blocks.as_ref() {
        let bs = grid.block_size as f64;
        // Viewport-cull: only iterate cells whose box could intersect the
        // canvas. Without this, big maps (200×200) would draw 40k edges/frame.
        let min_wx = camera.center.x as f64 - w * 0.5;
        let max_wx = camera.center.x as f64 + w * 0.5;
        let min_wy = camera.center.y as f64 - h * 0.5;
        let max_wy = camera.center.y as f64 + h * 0.5;
        let mut min_bx = (min_wx / bs).floor() as i64;
        let mut max_bx = (max_wx / bs).ceil() as i64;
        let mut min_by = (min_wy / bs).floor() as i64;
        let mut max_by = (max_wy / bs).ceil() as i64;
        // Wrap maps: leave the range extended (negative or > grid.width is
        // fine — `emit_block_outline` mods the lookup index but draws at the
        // extended world coords, so cells across the seam render seamlessly).
        if !world.map.edge_wrap {
            min_bx = min_bx.max(0);
            max_bx = max_bx.min(grid.width as i64);
            min_by = min_by.max(0);
            max_by = max_by.min(grid.height as i64);
        }
        for by in min_by..max_by {
            for bx in min_bx..max_bx {
                emit_block_outline(ctx, &camera, grid, bx, by, world.map.edge_wrap);
            }
        }
    } else {
        for wall in &world.map.walls {
            let (ax, ay) = camera.world_to_screen(wall.a);
            let (bx, by) = camera.world_to_screen(wall.b);
            ctx.move_to(ax, ay);
            ctx.line_to(bx, by);
        }
    }
    ctx.stroke();

    // Second pass — cannon spawn-direction indicators in white. Same viewport
    // cull as the wall pass; cheap when no cannons are visible (no path
    // segments emitted).
    if let Some(grid) = world.map.blocks.as_ref() {
        let bs = grid.block_size as f64;
        let min_wx = camera.center.x as f64 - w * 0.5;
        let max_wx = camera.center.x as f64 + w * 0.5;
        let min_wy = camera.center.y as f64 - h * 0.5;
        let max_wy = camera.center.y as f64 + h * 0.5;
        let mut min_bx = (min_wx / bs).floor() as i64;
        let mut max_bx = (max_wx / bs).ceil() as i64;
        let mut min_by = (min_wy / bs).floor() as i64;
        let mut max_by = (max_wy / bs).ceil() as i64;
        if !world.map.edge_wrap {
            min_bx = min_bx.max(0);
            max_bx = max_bx.min(grid.width as i64);
            min_by = min_by.max(0);
            max_by = max_by.min(grid.height as i64);
        }
        // Same line width as the wall pass so the indicator reads as a
        // map element of the same weight, not a glow on top.
        ctx.set_stroke_style_str("#fff");
        ctx.begin_path();
        for by in min_by..max_by {
            for bx in min_bx..max_bx {
                emit_spawn_indicator(ctx, &camera, grid, bx, by, world.map.edge_wrap);
            }
        }
        ctx.stroke();

        // Active cannon firing-triangles drawn as a thin white outline —
        // 1px so the cannon reads more like a ship's silhouette than the
        // chunkier blue walls around it. Dead cannons skipped; the wall
        // block stays visible either way.
        ctx.set_stroke_style_str("#fff");
        ctx.set_line_width(1.0);
        ctx.begin_path();
        for by in min_by..max_by {
            for bx in min_bx..max_bx {
                emit_cannon_triangle(ctx, &camera, world, grid, bx, by);
            }
        }
        ctx.stroke();
        ctx.set_line_width(2.0);
    }

    // Particles (behind ships). Two sources rendered the same way:
    // client-only thruster embers (cosmetic) and server-authored explosion
    // debris (real sim entities, networked). Additive blend so overlapping
    // particles brighten toward white — gives plumes/explosions the hot-core
    // look.
    let _ = ctx.set_global_composite_operation("lighter");
    ctx.set_fill_style_str("#ff8844");
    for p in &particles.items {
        let (x, y) = camera.project_entity(p.pos);
        let alpha = (1.0 - (p.age / p.life).clamp(0.0, 1.0)) as f64;
        ctx.set_global_alpha(alpha);
        ctx.fill_rect(x - 1.1, y - 1.1, 2.2, 2.2);
    }
    for p in &world.particles {
        let (x, y) = camera.project_entity(p.pos);
        let alpha = (1.0 - (p.age / p.life).clamp(0.0, 1.0)) as f64;
        ctx.set_global_alpha(alpha);
        ctx.fill_rect(x - 1.1, y - 1.1, 2.2, 2.2);
    }
    ctx.set_global_alpha(1.0);
    let _ = ctx.set_global_composite_operation("source-over");

    // Bullets.
    ctx.set_fill_style_str("#fff");
    for bullet in world.bullets.values() {
        let (x, y) = camera.project_entity(bullet.pos);
        ctx.fill_rect(x - 1.0, y - 1.0, 2.0, 2.0);
    }

    // Ships. Other-player names rendered underneath in white — like the
    // Elm version. Local player skipped (you know who you are). Bots get a
    // " (R)" suffix in the label and a small red center dot on the ship.
    for ship in world.ships.values() {
        let is_me = local_player_id.map_or(false, |me| me == ship.player_id);
        let player = players.iter().find(|p| p.player_id == ship.player_id);
        let is_bot = player.map(|p| p.is_bot).unwrap_or(false);
        let name_string: Option<String> = if is_me {
            None
        } else {
            player.map(display_name)
        };
        draw_ship(ctx, &camera, ship, name_string.as_deref(), is_bot);
    }

    // Debug HUD.
    ctx.set_fill_style_str("#6cf");
    ctx.set_font("14px monospace");
    let _ = ctx.fill_text(
        &format!(
            "tick {}  ships {}  bullets {}",
            world.tick,
            world.ships.len(),
            world.bullets.len()
        ),
        12.0,
        22.0,
    );
    let _ = ctx.fill_text("a/s turn   shift thrust   enter/space fire", 12.0, h - 12.0);

    // Local-player HUD around screen center (= around the local ship).
    if local_alive {
        if let Some(charge) = local_shot_charge {
            draw_local_hud(ctx, w, h, charge);
        }
        // Direction dot to whichever player the scoreboard cursor is on,
        // skipping ourselves. Drawn as part of the HUD because the indicator
        // only makes sense when we have a ship to be relative TO.
        if let (Some(cursor_pid), Some(local_pid), Some(target_pos)) =
            (cursor_player_id, local_player_id, cursor_target_pos)
        {
            if cursor_pid != local_pid {
                let mw = world.map.width as f64;
                let mh = world.map.height as f64;
                let map_diag = (mw * mw + mh * mh).sqrt();
                draw_target_indicator(
                    ctx, &camera, follow, target_pos, w, h, map_diag, now_ms,
                );
            }
        }
    }

    // Mini radar top-left, scoreboard top-right.
    draw_minimap(ctx, world, local_player_id, cursor_player_id, radar_walls, now_ms);
    draw_scoreboard(ctx, w, players, local_player_id, cursor_player_id);

    // Respawn countdown along the bottom of the HUD when waiting.
    if let Some(t) = respawn_remaining_seconds {
        draw_respawn_countdown(ctx, w, h, t);
    }

    // Player chat at the top (right of radar, growing downward), kill
    // notifications at the bottom (right of radar, growing upward, newest
    // closest to the bottom). Splitting them by anchor matches how the
    // original xpilot let game events and player talk live in different
    // regions instead of mixing them in one column.
    draw_chat_pane(ctx, chat_log, now_ms, ChatKind::Player, ChatAnchor::Top);
    draw_chat_pane(ctx, chat_log, now_ms, ChatKind::Kill, ChatAnchor::Bottom(h));

    // Personal-kill stack inside the HUD area, plus streak below.
    if local_alive {
        draw_personal_kills(ctx, w, h, personal_kills, kill_streak);
    }
}

/// Stack chat lines top-center of the canvas, oldest fading first. Lines
/// past their visible+fade window are skipped (the queue itself isn't
/// pruned here — that happens in lib.rs when it overflows the cap).
/// Anchor side for a chat pane.
///   - `Top`: pane sticks to the top of the screen, oldest at top, newest
///     drawn just below — i.e. text grows DOWN.
///   - `Bottom(canvas_h)`: pane sticks to the bottom; newest sits at the
///     bottom, older lines drawn ABOVE — text grows UP.
#[derive(Clone, Copy)]
enum ChatAnchor {
    Top,
    Bottom(f64),
}

/// Renders one chat pane. Filters the shared log by `kind` so player chat
/// and kill notifications can live in different regions and read as
/// distinct streams (mirrors classic xpilot, which separated game events
/// from player talk). Both panes share the exact same blue and font.
fn draw_chat_pane(
    ctx: &CanvasRenderingContext2d,
    log: &std::collections::VecDeque<ChatLine>,
    now_ms: f64,
    kind: ChatKind,
    anchor: ChatAnchor,
) {
    if log.is_empty() {
        return;
    }
    // line_h derived from `kind` further down so the bigger player font
    // gets enough vertical room.
    let pad = 4.0;
    let max_width: f64 = 700.0;

    // First pass: pick out lines of this kind that are still visible.
    let filtered: Vec<(&ChatLine, f64)> = log
        .iter()
        .filter(|l| l.kind == kind)
        .filter_map(|l| {
            let age = now_ms - l.t_ms;
            if age < CHAT_VISIBLE_MS {
                Some((l, 1.0))
            } else if age < CHAT_VISIBLE_MS + CHAT_FADE_MS {
                let fade_phase = (age - CHAT_VISIBLE_MS) / CHAT_FADE_MS;
                Some((l, 1.0 - fade_phase))
            } else {
                None
            }
        })
        .collect();
    if filtered.is_empty() {
        return;
    }
    // Cap at the newest CHAT_DISPLAY_MAX so old chatter doesn't fill the
    // screen if many messages come in fast.
    let start = filtered.len().saturating_sub(CHAT_DISPLAY_MAX);
    let visible: Vec<(&ChatLine, f64)> = filtered.into_iter().skip(start).collect();

    // Player chat: brighter, no backdrop. Kill messages: blue, slightly
    // smaller, also no backdrop now (used to have a translucent black box).
    // (font, color, backdrop, line_h, char_w)
    let (font, color, backdrop, line_h, char_w) = match kind {
        ChatKind::Player => ("14px monospace", "#fff", false, 17.0_f64, 8.4_f64),
        ChatKind::Kill => ("11px monospace", "#9cf", false, 14.0_f64, 6.6_f64),
    };
    ctx.set_font(font);
    ctx.set_text_align("left");
    let left_x = RADAR_PAD + RADAR_MAX + 12.0;

    // Build a per-line iteration where (y, line, alpha) is set up for the
    // chosen anchor. For Top: oldest first (top), newest last (bottom).
    // For Bottom: newest first (bottom), older above.
    let lines: Vec<(f64, &ChatLine, f64)> = match anchor {
        ChatAnchor::Top => {
            let mut y = 16.0 + line_h;
            let mut out = Vec::with_capacity(visible.len());
            for (l, a) in &visible {
                out.push((y, *l, *a));
                y += line_h;
            }
            out
        }
        ChatAnchor::Bottom(canvas_h) => {
            let mut y = canvas_h - 12.0;
            let mut out = Vec::with_capacity(visible.len());
            for (l, a) in visible.iter().rev() {
                out.push((y, *l, *a));
                y -= line_h;
            }
            out
        }
    };

    for (y, line, alpha) in &lines {
        // Kill messages have an empty `author` — the formatted text already
        // names killer + victim. Player chats keep "Name: text".
        let text = if line.author.is_empty() {
            line.text.clone()
        } else {
            format!("{}: {}", line.author, line.text)
        };
        let max_chars = ((max_width - 2.0 * pad) / char_w) as usize;
        let display: String = text.chars().take(max_chars).collect();
        let metrics_w = match ctx.measure_text(&display) {
            Ok(m) => m.width(),
            Err(_) => display.chars().count() as f64 * char_w,
        };
        let bx = left_x;
        if backdrop {
            let by = y - line_h + 4.0;
            ctx.set_global_alpha(0.45 * *alpha);
            ctx.set_fill_style_str("#000");
            ctx.fill_rect(bx, by, metrics_w + pad * 2.0, line_h);
        }
        ctx.set_global_alpha(*alpha);
        ctx.set_fill_style_str(color);
        let _ = ctx.fill_text(&display, bx + pad, *y);
    }
    ctx.set_global_alpha(1.0);
    ctx.set_text_align("start");
}

/// Lock-style direction dot for the scoreboard cursor's target — port of
/// OG xpilot 4.5.x `Paint_lock` (see reference/xpilot-4.5.5/src/client/painthud.c).
/// Lives INSIDE the HUD box at 60 % of the half-extent in the target's
/// direction (an ellipse anchored on screen centre). Size is the classic
/// `min(mapdiag / dist, 10)` inverse-distance formula, floored at 1 px.
/// Blinks every other ~250 ms when the target is within `WARN_DIST` —
/// matches OG's `lock_dist > WARNING_DISTANCE || warningCount++ % 2 == 0`.
fn draw_target_indicator(
    ctx: &CanvasRenderingContext2d,
    camera: &Camera,
    local_pos: Vec2,
    target_pos: Vec2,
    canvas_w: f64,
    canvas_h: f64,
    map_diag: f64,
    now_ms: f64,
) {
    // 70% of the HUD's half-extents — sits well inside the box but a bit
    // further from centre than OG's 0.6 so it doesn't visually crowd the
    // ship icon at screen middle. HUD is 180×150 → half = 90×75.
    const POS_FRAC: f64 = 0.7;
    const HALF_W: f64 = 90.0;
    const HALF_H: f64 = 75.0;
    // Tighter cap than OG's 10 px — at our world scale the dot ballooned
    // up too quickly when targets came in close. 5 px max keeps it as a
    // marker, not a blob.
    const SIZE_CAP: f64 = 5.0;
    const SIZE_FLOOR: f64 = 1.0;
    // Distance below which the dot starts blinking. Scaled to our world
    // (≈12 % of map diagonal) so it kicks in at the same "they're getting
    // close" range OG had relative to its visibility radius.
    const WARN_DIST: f64 = 600.0;

    let nearest = camera.nearest(target_pos);
    let dx = (nearest.x - local_pos.x) as f64;
    let dy = (nearest.y - local_pos.y) as f64;
    let dist = ((dx * dx + dy * dy) as f64).sqrt();
    if dist < 1e-3 {
        return;
    }

    // Blink when close: visible every other 250 ms half-cycle.
    let blink_on = dist > WARN_DIST || ((now_ms / 250.0) as i64) & 1 == 0;
    if !blink_on {
        return;
    }

    let angle = dy.atan2(dx);
    let mid_x = canvas_w / 2.0;
    let mid_y = canvas_h / 2.0;
    let dot_x = mid_x + POS_FRAC * HALF_W * angle.cos();
    let dot_y = mid_y + POS_FRAC * HALF_H * angle.sin();

    // OG: size = min(mapdiag / lock_dist, 10), floored at 1.
    let radius = (map_diag / dist).min(SIZE_CAP).max(SIZE_FLOOR);

    ctx.set_fill_style_str("#070");
    ctx.begin_path();
    let _ = ctx.arc(dot_x, dot_y, radius, 0.0, core::f64::consts::TAU);
    ctx.fill();
}

/// Personal-kill stack + kill-streak counter — port of `xpilot.elm:1240-1305`.
///
/// Stack: anchored at the HUD bottom-LEFT, stacking UPWARD with index 0
/// (oldest) at the bottom and newer entries pushed above. Just the
/// player's name; colour carries the meaning (green = killed them,
/// yellow = died to them, dark red = "Yourself"). Italic bold monospace.
/// Drops at 5 s like Elm's `tickHudMessages`, with a brief alpha fade
/// in the final 1 s for polish.
///
/// Streak: green italic bold monospace number in the HUD bottom-RIGHT
/// corner, matching `xpilot.elm:1294-1305`. Always visible (Elm always
/// rendered `hud.kills` even when 0). Truncated at 999 to fit the
/// 3-character box the original used.
fn draw_personal_kills(
    ctx: &CanvasRenderingContext2d,
    canvas_w: f64,
    canvas_h: f64,
    kills: &[PersonalKillView],
    kill_streak: u32,
) {
    const HUD_HALF_W: f64 = 90.0;
    const HUD_HALF_H: f64 = 75.0;
    const LIFETIME_MS: f64 = 5_000.0;
    const FADE_MS: f64 = 1_000.0;
    const STEP_PY: f64 = 16.0;

    let mid_x = canvas_w / 2.0;
    let mid_y = canvas_h / 2.0;
    let left_x = mid_x - HUD_HALF_W;
    let base_y = mid_y + HUD_HALF_H - 4.0;

    ctx.set_font("italic bold 13px monospace");
    ctx.set_text_align("left");

    let visible: Vec<&PersonalKillView> =
        kills.iter().filter(|k| k.age_ms < LIFETIME_MS).collect();
    for (i, k) in visible.iter().enumerate() {
        let y = base_y - STEP_PY * (i as f64);
        let alpha = if k.age_ms < LIFETIME_MS - FADE_MS {
            1.0
        } else {
            ((LIFETIME_MS - k.age_ms) / FADE_MS).clamp(0.0, 1.0)
        };
        ctx.set_global_alpha(alpha);
        ctx.set_fill_style_str(k.color);
        let _ = ctx.fill_text(k.name, left_x, y);
    }
    ctx.set_global_alpha(1.0);

    // Kill-streak counter in HUD bottom-right corner. Elm:
    //   x = middle.x + hudWidth/2 - 20, y = middle.y + hudHeight/2
    //   String.left 3 (toString hud.kills)
    // Right-align text and put its anchor near the inside-right of the
    // box so the digits read inward from the energy bar.
    ctx.set_font("italic bold 16px monospace");
    ctx.set_fill_style_str("#008000");
    ctx.set_text_align("right");
    let display = kill_streak.min(999).to_string();
    let _ = ctx.fill_text(&display, mid_x + HUD_HALF_W - 8.0, mid_y + HUD_HALF_H - 4.0);

    ctx.set_text_align("start");
}

fn draw_local_hud(ctx: &CanvasRenderingContext2d, canvas_w: f64, canvas_h: f64, charge: f32) {
    let energy = (charge / SHOT_CHARGE_MAX).clamp(0.0, 1.0) as f64;
    let mid_x = canvas_w / 2.0;
    let mid_y = canvas_h / 2.0;
    let hud_w: f64 = 180.0;
    let hud_h: f64 = 150.0;
    let bar_w: f64 = 6.0;

    let hx = mid_x - hud_w / 2.0;
    let hy = mid_y - hud_h / 2.0;
    let bx = mid_x + hud_w / 2.0;

    // Dimmer green so the HUD doesn't fight the gameplay for attention.
    let hud_color = "#070";
    ctx.set_stroke_style_str(hud_color);
    ctx.set_line_width(1.0);

    // Dashed top + bottom boundary lines only — no side walls.
    let dash = js_sys::Array::of2(&JsValue::from_f64(10.0), &JsValue::from_f64(5.0));
    let _ = ctx.set_line_dash(&dash);
    ctx.begin_path();
    ctx.move_to(hx, hy);
    ctx.line_to(hx + hud_w, hy);
    ctx.stroke();
    ctx.begin_path();
    ctx.move_to(hx, hy + hud_h);
    ctx.line_to(hx + hud_w, hy + hud_h);
    ctx.stroke();
    let solid = js_sys::Array::new();
    let _ = ctx.set_line_dash(&solid);

    // Energy bar — full HUD height, fills bottom-up with current charge.
    ctx.stroke_rect(bx, hy, bar_w, hud_h);
    let fill_h = hud_h * energy;
    let fill_y = hy + (hud_h - fill_h);
    ctx.set_fill_style_str(hud_color);
    ctx.fill_rect(bx, fill_y, bar_w, fill_h);
}

fn draw_minimap(
    ctx: &CanvasRenderingContext2d,
    world: &World,
    local_player_id: Option<EntityId>,
    cursor_player_id: Option<EntityId>,
    radar_walls: Option<&HtmlCanvasElement>,
    now_ms: f64,
) {
    let (radar_w, radar_h, scale) = match radar_dims(world.map.width, world.map.height) {
        Some(d) => d,
        None => return,
    };
    let x = RADAR_PAD;
    let y = RADAR_PAD;

    ctx.set_fill_style_str("rgba(0, 0, 0, 0.55)");
    ctx.fill_rect(x, y, radar_w, radar_h);

    if let Some(walls) = radar_walls {
        let _ = ctx.draw_image_with_html_canvas_element(walls, x, y);
    }

    // 2 Hz blink for the cursor's target dot — visible on for ~250 ms,
    // off for ~250 ms. Skipped when cursor is on yourself.
    let blink_on = ((now_ms / 250.0) as i64) & 1 == 0;

    for ship in world.ships.values() {
        let is_me = local_player_id.map_or(false, |me| me == ship.player_id);
        let is_cursor =
            !is_me && cursor_player_id.map_or(false, |c| c == ship.player_id);
        // Cursor target: dot is straight-up hidden during the off-half of
        // the blink (no overlay/size change). Same color/size as any other
        // ship when it IS visible — pure on/off rhythm.
        if is_cursor && !blink_on {
            continue;
        }
        let color = if is_me { "#ff0" } else { "#fff" };
        ctx.set_fill_style_str(color);
        let sx = x + ship.pos.x as f64 * scale;
        let sy = y + ship.pos.y as f64 * scale;
        let r = if is_me { 2.5 } else { 1.5 };
        ctx.fill_rect(sx - r, sy - r, r * 2.0, r * 2.0);
        if is_me {
            let dir = forward(ship.angle);
            let len: f64 = 9.0;
            let nx = sx + dir.x as f64 * len;
            let ny = sy + dir.y as f64 * len;
            ctx.set_stroke_style_str("#fff");
            ctx.set_line_width(1.5);
            ctx.begin_path();
            ctx.move_to(sx, sy);
            ctx.line_to(nx, ny);
            ctx.stroke();
        }
    }

    ctx.set_stroke_style_str("rgba(120, 140, 220, 0.6)");
    ctx.set_line_width(1.0);
    ctx.stroke_rect(x, y, radar_w, radar_h);
}

fn radar_dims(map_w: f32, map_h: f32) -> Option<(f64, f64, f64)> {
    let mw = map_w as f64;
    let mh = map_h as f64;
    if mw <= 0.0 || mh <= 0.0 {
        return None;
    }
    let scale = (RADAR_MAX / mw).min(RADAR_MAX / mh);
    Some((mw * scale, mh * scale, scale))
}

/// Build the radar wall layer once per map load. The returned canvas has
/// the same dimensions as the radar's wall area; `draw_minimap` blits it
/// each frame at the radar's screen position.
pub fn build_radar_walls(document: &Document, map: &Map) -> Option<HtmlCanvasElement> {
    let (rw, rh, scale) = radar_dims(map.width, map.height)?;
    let canvas: HtmlCanvasElement = document
        .create_element("canvas")
        .ok()?
        .dyn_into()
        .ok()?;
    canvas.set_width(rw.ceil() as u32);
    canvas.set_height(rh.ceil() as u32);
    let ctx: CanvasRenderingContext2d = canvas
        .get_context("2d")
        .ok()??
        .dyn_into()
        .ok()?;
    let grid = map.blocks.as_ref()?;
    let bs = grid.block_size as f64;
    let cell = bs * scale;
    // Half-pixel oversize prevents 1-pixel gaps between adjacent cells when
    // `scale * bs` lands just under an integer.
    let pad = 0.5;
    // Same hue as the main walls (#5588ff) shaded down a touch so it reads
    // as "background" against the action.
    ctx.set_fill_style_str("rgba(60, 100, 200, 0.9)");
    for by in 0..grid.height as i64 {
        for bx in 0..grid.width as i64 {
            if grid.get(bx, by).is_wall() {
                ctx.fill_rect(bx as f64 * cell, by as f64 * cell, cell + pad, cell + pad);
            }
        }
    }
    Some(canvas)
}

fn draw_scoreboard(
    ctx: &CanvasRenderingContext2d,
    canvas_w: f64,
    players: &[PlayerInfo],
    local_player_id: Option<EntityId>,
    cursor_player_id: Option<EntityId>,
) {
    let line_h = 16.0;
    let pad = 8.0;
    let rows = players.len() + 1; // header + entries
    let height = pad * 2.0 + line_h * rows as f64;
    let width = 260.0;
    let x = canvas_w - width - 10.0;
    let y = 10.0;

    ctx.set_fill_style_str("rgba(0, 0, 0, 0.55)");
    ctx.fill_rect(x, y, width, height);
    ctx.set_stroke_style_str("#44a");
    ctx.set_line_width(1.0);
    ctx.stroke_rect(x, y, width, height);

    ctx.set_font("13px monospace");
    ctx.set_fill_style_str("#6cf");
    // Two extra spaces in the header for the "> " cursor column on the left.
    let _ = ctx.fill_text("  name               K   D", x + pad, y + pad + line_h - 3.0);

    let mut sorted: Vec<&PlayerInfo> = players.iter().collect();
    sorted.sort_by_key(|p| -(p.kills as i64 - p.deaths as i64));

    for (i, p) in sorted.iter().enumerate() {
        let row_y = y + pad + line_h * (i as f64 + 2.0) - 3.0;
        let is_me = local_player_id.map_or(false, |me| me == p.player_id);
        let is_cursor = cursor_player_id.map_or(false, |c| c == p.player_id);
        let color = if !p.dead {
            if is_me { "#ff0" } else { "#fff" }
        } else if is_me {
            "#a80"
        } else {
            "#888"
        };
        let mut name = display_name(p);
        name.truncate(16);
        // Draw the row text first, then overlay a yellow `>` for the cursor
        // row. Splitting these lets the cursor stay yellow regardless of
        // whether the highlighted player is alive / dead / you.
        ctx.set_fill_style_str(color);
        let _ = ctx.fill_text(
            &format!("  {:<16}  {:>3} {:>3}", name, p.kills, p.deaths),
            x + pad,
            row_y,
        );
        if is_cursor {
            ctx.set_fill_style_str("#ff0");
            let _ = ctx.fill_text(">", x + pad, row_y);
        }
    }
}

/// Display name for a player, with a " (R)" suffix for server-spawned
/// bots so humans can tell players apart at a glance. Short tag so it
/// doesn't crowd the scoreboard column.
fn display_name(p: &PlayerInfo) -> String {
    if p.is_bot {
        format!("{} (R)", p.name)
    } else {
        p.name.clone()
    }
}

fn draw_respawn_countdown(ctx: &CanvasRenderingContext2d, w: f64, h: f64, remaining: f32) {
    // Match the local HUD box dimensions in `draw_local_hud` and place the
    // countdown text just below its bottom edge.
    let hud_w: f64 = 180.0;
    let hud_h: f64 = 150.0;
    let mid_x = w / 2.0;
    let mid_y = h / 2.0;
    let bottom = mid_y + hud_h / 2.0;
    // Match the local HUD's green so the countdown reads as part of the
    // same widget rather than a separate alert.
    ctx.set_fill_style_str("#070");
    ctx.set_font("11px monospace");
    let text = format!("{:.2}", remaining);
    let tw = ctx.measure_text(&text).map(|m| m.width()).unwrap_or(36.0);
    let _ = ctx.fill_text(&text, mid_x - tw / 2.0, bottom + 14.0);
    let _ = (hud_w,); // currently unused — kept so the layout is obviously paired with the HUD.
}

/// Emit the segments that make up one wall cell's outline. Drawn into the
/// caller's `beginPath()` accumulator. No edge culling — emitting every cell's
/// full outline is what gives the classic XPilot grid look; adjacent wall
/// cells double-stroke the shared edge but the visual is the same and the
/// browser canvas swallows the extra move/line ops cheaply.
fn emit_block_outline(
    ctx: &CanvasRenderingContext2d,
    camera: &Camera,
    grid: &shared::map::BlockGrid,
    bx: i64,
    by: i64,
    wrap: bool,
) {
    let bs = grid.block_size;
    // For wrap maps, `bx`/`by` may sit outside the grid — mod for the
    // lookup but draw the cell at its extended world position so wrap
    // cells appear contiguous with the rest of the world.
    let (lookup_x, lookup_y) = if wrap {
        (
            bx.rem_euclid(grid.width as i64),
            by.rem_euclid(grid.height as i64),
        )
    } else {
        (bx, by)
    };
    let lx = bx as f32 * bs;
    let ly = by as f32 * bs;
    let hx = lx + bs;
    let hy = ly + bs;
    // Engine is y-down: smaller world-y is visually upper.
    let tl = camera.world_to_screen(Vec2::new(lx, ly));
    let tr = camera.world_to_screen(Vec2::new(hx, ly));
    let bl = camera.world_to_screen(Vec2::new(lx, hy));
    let br = camera.world_to_screen(Vec2::new(hx, hy));
    let line = |a: (f64, f64), b: (f64, f64)| {
        ctx.move_to(a.0, a.1);
        ctx.line_to(b.0, b.1);
    };
    match grid.get(lookup_x, lookup_y) {
        Block::Wall
        | Block::CannonFireUp
        | Block::CannonFireDown
        | Block::CannonFireLeft
        | Block::CannonFireRight => {
            // Active cannon block renders the same 4-sided wall as a plain
            // Wall — the firing triangle is added by `emit_cannon_triangle`.
            line(tl, tr); // top
            line(tr, br); // right
            line(br, bl); // bottom
            line(bl, tl); // left
        }
        // Spawn-marker cells (lowercase r/c/d/f) are OPEN — they only
        // contribute their indicator line in the second pass.
        Block::CannonUp
        | Block::CannonDown
        | Block::CannonLeft
        | Block::CannonRight => {}
        Block::TriUL => {
            line(tl, tr); // top
            line(tl, bl); // left
            line(tr, bl); // hypotenuse top-right ↔ bottom-left
        }
        Block::TriUR => {
            line(tl, tr); // top
            line(tr, br); // right
            line(tl, br); // hypotenuse top-left ↔ bottom-right
        }
        Block::TriLL => {
            line(bl, br); // bottom
            line(tl, bl); // left
            line(tl, br); // hypotenuse top-left ↔ bottom-right
        }
        Block::TriLR => {
            line(bl, br); // bottom
            line(tr, br); // right
            line(tr, bl); // hypotenuse top-right ↔ bottom-left
        }
        Block::Space | Block::Base => {}
    }
}

/// Draws an active cannon's firing-triangle as a closed sub-path so the
/// outer `ctx.fill()` paints it. Skips dead cannons (server marks them in
/// `dead_cannons`). The triangle's base is along the cell edge facing the
/// firing direction; the tip sticks `CANNON_TRIANGLE_FRAC` of a block past
/// that edge into the open cell beyond.
fn emit_cannon_triangle(
    ctx: &CanvasRenderingContext2d,
    camera: &Camera,
    world: &World,
    grid: &shared::map::BlockGrid,
    bx: i64,
    by: i64,
) {
    let bs = grid.block_size;
    let lookup_x = if world.map.edge_wrap {
        let w = grid.width as i64;
        ((bx % w) + w) % w
    } else {
        bx
    };
    let lookup_y = if world.map.edge_wrap {
        let h = grid.height as i64;
        ((by % h) + h) % h
    } else {
        by
    };
    let block = grid.get(lookup_x, lookup_y);
    if block.cannon_fire().is_none() {
        return;
    }
    // Alive lookup uses the wrapped (real) cell coords — that's what the
    // server keys cannons by.
    let alive = world
        .cannons
        .get(&(lookup_x as u32, lookup_y as u32))
        .map(|c| c.alive)
        .unwrap_or(true);
    if !alive {
        return;
    }
    let f = shared::constants::CANNON_TRIANGLE_FRAC;
    let inset = shared::constants::CANNON_BASE_INSET;
    let sit = shared::constants::CANNON_SIT_OFFSET;
    let lx = bx as f32 * bs;
    let ly = by as f32 * bs;
    let hx = lx + bs;
    let hy = ly + bs;
    // Triangle sits OUTSIDE the wall by `sit` so the wall outline shows
    // through underneath. Base is inset by `inset` on each end so the wall
    // outline also extends past the cannon on both sides of the base.
    let (base_a, base_b, tip) = match block {
        shared::map::Block::CannonFireUp => (
            Vec2::new(lx + inset, ly - sit),
            Vec2::new(hx - inset, ly - sit),
            Vec2::new(lx + bs * 0.5, ly - sit - f * bs),
        ),
        shared::map::Block::CannonFireDown => (
            Vec2::new(lx + inset, hy + sit),
            Vec2::new(hx - inset, hy + sit),
            Vec2::new(lx + bs * 0.5, hy + sit + f * bs),
        ),
        shared::map::Block::CannonFireLeft => (
            Vec2::new(lx - sit, ly + inset),
            Vec2::new(lx - sit, hy - inset),
            Vec2::new(lx - sit - f * bs, ly + bs * 0.5),
        ),
        shared::map::Block::CannonFireRight => (
            Vec2::new(hx + sit, ly + inset),
            Vec2::new(hx + sit, hy - inset),
            Vec2::new(hx + sit + f * bs, ly + bs * 0.5),
        ),
        _ => return,
    };
    let a = camera.world_to_screen(base_a);
    let b = camera.world_to_screen(base_b);
    let t = camera.world_to_screen(tip);
    ctx.move_to(a.0, a.1);
    ctx.line_to(b.0, b.1);
    ctx.line_to(t.0, t.1);
    ctx.close_path();
}

/// Draws the spawn-direction indicator line on a cell. The line acts as the
/// "back wall" the ship rests against, with its perpendicular giving the
/// facing direction — so for an up-facing spawn the line sits along the
/// BOTTOM edge of the cell. Skips cells that aren't bases or cannons.
fn emit_spawn_indicator(
    ctx: &CanvasRenderingContext2d,
    camera: &Camera,
    grid: &shared::map::BlockGrid,
    bx: i64,
    by: i64,
    edge_wrap: bool,
) {
    let bs = grid.block_size;
    let (lookup_x, lookup_y) = if edge_wrap {
        let w = grid.width as i64;
        let h = grid.height as i64;
        (((bx % w) + w) % w, ((by % h) + h) % h)
    } else {
        (bx, by)
    };
    let block = grid.get(lookup_x, lookup_y);
    if !matches!(
        block,
        Block::Base
            | Block::CannonUp
            | Block::CannonDown
            | Block::CannonLeft
            | Block::CannonRight
    ) {
        return;
    }
    let lx = bx as f32 * bs;
    let ly = by as f32 * bs;
    let hx = lx + bs;
    let hy = ly + bs;
    let tl = camera.world_to_screen(Vec2::new(lx, ly));
    let tr = camera.world_to_screen(Vec2::new(hx, ly));
    let bl = camera.world_to_screen(Vec2::new(lx, hy));
    let br = camera.world_to_screen(Vec2::new(hx, hy));
    // Indicator sits on the side OPPOSITE the spawn-facing direction —
    // perpendicular to it gives the ship's heading. Bases face up by default.
    let (a, b) = match block {
        Block::CannonUp | Block::Base => (bl, br), // back wall = bottom, ship faces up
        Block::CannonDown => (tl, tr),             // back wall = top, ship faces down
        Block::CannonLeft => (tr, br),             // back wall = right, ship faces left
        Block::CannonRight => (tl, bl),            // back wall = left, ship faces right
        _ => return,
    };
    ctx.move_to(a.0, a.1);
    ctx.line_to(b.0, b.1);
}

pub fn render_status(ctx: &CanvasRenderingContext2d, canvas: &HtmlCanvasElement, status: &str) {
    let w = canvas.width() as f64;
    let h = canvas.height() as f64;
    ctx.set_fill_style_str("#000");
    ctx.fill_rect(0.0, 0.0, w, h);
    ctx.set_fill_style_str("#6cf");
    ctx.set_font("18px monospace");
    let _ = ctx.fill_text(status, 20.0, 40.0);
}

fn draw_ship(
    ctx: &CanvasRenderingContext2d,
    camera: &Camera,
    ship: &Ship,
    name: Option<&str>,
    is_bot: bool,
) {
    // For wrap maps, the ship may be on the "other side" of the seam from
    // the camera; shift all 3 vertices by the same offset so the triangle
    // stays rigid while crossing the boundary.
    let visual_pos = camera.nearest(ship.pos);
    let offset = visual_pos - ship.pos;
    let verts = ship.world_vertices();
    let pts: [(f64, f64); 3] = [
        camera.world_to_screen(verts[0] + offset),
        camera.world_to_screen(verts[1] + offset),
        camera.world_to_screen(verts[2] + offset),
    ];
    ctx.set_stroke_style_str("#fff");
    ctx.set_fill_style_str("#000");
    ctx.set_line_width(1.5);
    ctx.begin_path();
    ctx.move_to(pts[0].0, pts[0].1);
    ctx.line_to(pts[1].0, pts[1].1);
    ctx.line_to(pts[2].0, pts[2].1);
    ctx.close_path();
    ctx.fill();
    ctx.stroke();

    // Bot tell: a small red dot at the ship centroid — reads as a "brain
    // light" against the otherwise-blank triangle. Drawn after stroke so
    // it sits on top.
    if is_bot {
        let (cx, cy) = camera.world_to_screen(visual_pos);
        ctx.set_fill_style_str("#f44");
        ctx.begin_path();
        let _ = ctx.arc(cx, cy, 2.0, 0.0, core::f64::consts::TAU);
        ctx.fill();
    }

    if let Some(name) = name {
        // Yellow italic monospace, 35px below ship.pos — matches the Elm
        // version's `fill yellow / fontStyle italic / y = pos.y + 35`.
        let (cx, cy) = camera.world_to_screen(visual_pos);
        ctx.set_fill_style_str("yellow");
        ctx.set_font("italic 13px monospace");
        let tw = ctx.measure_text(name).map(|m| m.width()).unwrap_or(0.0);
        let _ = ctx.fill_text(name, cx - tw / 2.0, cy + 35.0);
    }
}
