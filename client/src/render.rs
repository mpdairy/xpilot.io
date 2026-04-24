use shared::constants::SHOT_CHARGE_MAX;
use shared::entities::{EntityId, Ship};
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
    // Elm version. Local player skipped (you know who you are).
    for ship in world.ships.values() {
        let is_me = local_player_id.map_or(false, |me| me == ship.player_id);
        let name = if is_me {
            None
        } else {
            players
                .iter()
                .find(|p| p.player_id == ship.player_id)
                .map(|p| p.name.as_str())
        };
        draw_ship(ctx, &camera, ship, name);
    }

    // Debug HUD.
    ctx.set_fill_style_str("#0a0");
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
    }

    // Mini radar top-left, scoreboard top-right.
    draw_minimap(ctx, world, local_player_id, radar_walls);
    draw_scoreboard(ctx, w, players, local_player_id);

    // Respawn countdown along the bottom of the HUD when waiting.
    if let Some(t) = respawn_remaining_seconds {
        draw_respawn_countdown(ctx, w, h, t);
    }
}

fn draw_local_hud(ctx: &CanvasRenderingContext2d, canvas_w: f64, canvas_h: f64, charge: f32) {
    let energy = (charge / SHOT_CHARGE_MAX).clamp(0.0, 1.0) as f64;
    let mid_x = canvas_w / 2.0;
    let mid_y = canvas_h / 2.0;
    let hud_w: f64 = 180.0;
    let hud_h: f64 = 150.0;
    let bar_w: f64 = 4.0;

    let hx = mid_x - hud_w / 2.0;
    let hy = mid_y - hud_h / 2.0;
    let bx = mid_x + hud_w / 2.0;

    // CSS "green" = #008000 — what the Elm version uses.
    ctx.set_stroke_style_str("green");
    ctx.set_line_width(2.0);

    ctx.set_line_cap("round");
    let dash = js_sys::Array::of2(&JsValue::from_f64(0.0), &JsValue::from_f64(7.0));
    let _ = ctx.set_line_dash(&dash);
    ctx.stroke_rect(hx, hy, hud_w, hud_h);
    let solid = js_sys::Array::new();
    let _ = ctx.set_line_dash(&solid);
    ctx.set_line_cap("butt");

    ctx.set_line_width(1.0);
    ctx.stroke_rect(bx, hy, bar_w, hud_h);
    let fill_h = hud_h * energy;
    let fill_y = hy + (hud_h - fill_h);
    ctx.set_fill_style_str("green");
    ctx.fill_rect(bx, fill_y, bar_w, fill_h);
}

fn draw_minimap(
    ctx: &CanvasRenderingContext2d,
    world: &World,
    local_player_id: Option<EntityId>,
    radar_walls: Option<&HtmlCanvasElement>,
) {
    let (radar_w, radar_h, scale) = match radar_dims(world.map.width, world.map.height) {
        Some(d) => d,
        None => return,
    };
    let x = RADAR_PAD;
    let y = RADAR_PAD;

    ctx.set_fill_style_str("rgba(0, 0, 0, 0.55)");
    ctx.fill_rect(x, y, radar_w, radar_h);

    // Walls drawn from a pre-rendered offscreen canvas — for big maps
    // (newdarkhell is 200×200 = 40k cell tests) iterating each frame burns
    // CPU for no reason since the wall layer never changes.
    if let Some(walls) = radar_walls {
        let _ = ctx.draw_image_with_html_canvas_element(walls, x, y);
    }

    // Ship dots. Local player is yellow so they can find themself instantly.
    for ship in world.ships.values() {
        let is_me = local_player_id.map_or(false, |me| me == ship.player_id);
        ctx.set_fill_style_str(if is_me { "#ff0" } else { "#fff" });
        let sx = x + ship.pos.x as f64 * scale;
        let sy = y + ship.pos.y as f64 * scale;
        let r = if is_me { 2.5 } else { 1.5 };
        ctx.fill_rect(sx - r, sy - r, r * 2.0, r * 2.0);
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
) {
    let line_h = 16.0;
    let pad = 8.0;
    let rows = players.len() + 1; // header + entries
    let height = pad * 2.0 + line_h * rows as f64;
    let width = 220.0;
    let x = canvas_w - width - 10.0;
    let y = 10.0;

    ctx.set_fill_style_str("rgba(0, 0, 0, 0.55)");
    ctx.fill_rect(x, y, width, height);
    ctx.set_stroke_style_str("#0a0");
    ctx.set_line_width(1.0);
    ctx.stroke_rect(x, y, width, height);

    ctx.set_font("13px monospace");
    ctx.set_fill_style_str("#0c0");
    let _ = ctx.fill_text("name           K   D", x + pad, y + pad + line_h - 3.0);

    let mut sorted: Vec<&PlayerInfo> = players.iter().collect();
    sorted.sort_by_key(|p| -(p.kills as i64 - p.deaths as i64));

    for (i, p) in sorted.iter().enumerate() {
        let row_y = y + pad + line_h * (i as f64 + 2.0) - 3.0;
        let is_me = local_player_id.map_or(false, |me| me == p.player_id);
        let color = if !p.dead {
            if is_me { "#ff0" } else { "#fff" }
        } else if is_me {
            "#a80"
        } else {
            "#888"
        };
        ctx.set_fill_style_str(color);
        let mut name = p.name.clone();
        name.truncate(12);
        let _ = ctx.fill_text(
            &format!("{:<12}  {:>3} {:>3}", name, p.kills, p.deaths),
            x + pad,
            row_y,
        );
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
    ctx.set_fill_style_str("#0c0");
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
        Block::Wall => {
            line(tl, tr); // top
            line(tr, br); // right
            line(br, bl); // bottom
            line(bl, tl); // left
        }
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

pub fn render_status(ctx: &CanvasRenderingContext2d, canvas: &HtmlCanvasElement, status: &str) {
    let w = canvas.width() as f64;
    let h = canvas.height() as f64;
    ctx.set_fill_style_str("#000");
    ctx.fill_rect(0.0, 0.0, w, h);
    ctx.set_fill_style_str("#0a0");
    ctx.set_font("18px monospace");
    let _ = ctx.fill_text(status, 20.0, 40.0);
}

fn draw_ship(ctx: &CanvasRenderingContext2d, camera: &Camera, ship: &Ship, name: Option<&str>) {
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
