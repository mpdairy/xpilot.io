// Authoritative-style sim state. `World::step` is deterministic given
// identical inputs — same on server and client prediction.
//
// The per-ship and per-bullet pieces are exposed as free fns so the client
// can drive prediction by calling `apply_ship_dynamics` on just its own ship,
// without spawning bullets locally (the server is authoritative for those).

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::constants::*;
use crate::entities::{forward, Bullet, EntityId, PlayerId, Ship, SHIP_NOSE_OFFSET};
use crate::map::{Block, BlockGrid, Map};
use crate::math::{self, Vec2};
use crate::physics;
use crate::protocol::{GameEvent, TickInput};

/// Wall bounce restitution. 0.4 matches the Elm version's `-0.4 * vel.y`
/// bounce — feels predictable, slow drifts decay into rest naturally instead
/// of pinning at the surface forever.
const WALL_RESTITUTION: f32 = 0.4;
/// Iterations per tick of the residual-overlap pass.
const COLLISION_ITERS: u32 = 6;
/// Tiny separation distance applied along the contact normal after a wall
/// resolution. Prevents float-precision tail from flickering SAT detection
/// on/off between adjacent ticks (which causes visible per-tick oscillation
/// even when the ship is "logically at rest" against the wall).
const CONTACT_SLOP: f32 = 0.05;

/// Wrap a position into [0, w) × [0, h) for toroidal maps. Uses `rem_euclid`
/// so negative inputs wrap correctly (a ship that just passed x=0 lands at
/// x≈w, not x≈-1).
pub fn wrap_pos(p: Vec2, w: f32, h: f32) -> Vec2 {
    Vec2::new(p.x.rem_euclid(w), p.y.rem_euclid(h))
}

/// True when `pos` falls inside the solid (filled) part of the cell at
/// (`bx`, `by`) of kind `cell`. Whole Wall cells are solid throughout;
/// triangle cells are tested against the actual triangle wedge.
fn pos_in_solid_part(pos: Vec2, cell: Block, bx: i64, by: i64, bs: f32) -> bool {
    if matches!(cell, Block::Space | Block::Base) {
        return false;
    }
    if matches!(cell, Block::Wall) {
        return true;
    }
    let lx = bx as f32 * bs;
    let ly = by as f32 * bs;
    let hx = lx + bs;
    let hy = ly + bs;
    // Engine is y-down: smaller y is visually upper.
    let tl = Vec2::new(lx, ly);
    let tr = Vec2::new(hx, ly);
    let bl = Vec2::new(lx, hy);
    let br = Vec2::new(hx, hy);
    let tri = match cell {
        Block::TriUL => [tl, tr, bl], // filled top-left wedge
        Block::TriUR => [tl, tr, br], // filled top-right wedge
        Block::TriLL => [tl, bl, br], // filled bottom-left wedge
        Block::TriLR => [tr, bl, br], // filled bottom-right wedge
        _ => return false,
    };
    physics::point_in_triangle(pos, &tri)
}

/// One wall feature for swept collision: either a line SEGMENT (cell edge or
/// slope hypotenuse) or an isolated VERTEX (cell corner where solid meets
/// empty in a way that exposes the corner to ship motion).
///
/// The xpilot-ng style: ship vertices sweep against segments (gives a "vertex
/// hits flat wall" hit), AND map vertices "sweep" backward against ship edges
/// (gives a "wall corner pokes into ship body" hit). Both are needed; either
/// one alone misses corner cases.
#[derive(Clone, Copy, Debug)]
struct WallSeg {
    a: Vec2,
    b: Vec2,
    /// Outward normal — points from wall toward open space. Precomputed so the
    /// bounce direction is unambiguous (no centroid disambiguation, no flip).
    out_normal: Vec2,
}

#[derive(Clone, Copy, Debug)]
struct WallPt {
    p: Vec2,
}

/// World-space polygon for a solid cell. `Block::Wall` is a square; the four
/// `Tri*` variants are the three corners that border the empty diagonal half
/// (so the polygon is the actual filled wedge, not the bounding square).
/// Returns `None` for non-solid cells.
fn cell_polygon(cell: Block, bx: i64, by: i64, bs: f32) -> Option<Vec<Vec2>> {
    let lx = bx as f32 * bs;
    let ly = by as f32 * bs;
    let hx = lx + bs;
    let hy = ly + bs;
    let tl = Vec2::new(lx, ly);
    let tr = Vec2::new(hx, ly);
    let bl = Vec2::new(lx, hy);
    let br = Vec2::new(hx, hy);
    Some(match cell {
        Block::Wall => vec![tl, tr, br, bl],
        Block::TriUL => vec![tl, tr, bl],
        Block::TriUR => vec![tl, tr, br],
        Block::TriLL => vec![tl, bl, br],
        Block::TriLR => vec![tr, bl, br],
        _ => return None,
    })
}

/// Solid cell polygons within a 1-cell radius of `pos`. Polygons are built at
/// the *unwrapped* world position of each cell so a ship near the wrap seam
/// still gets cells from the other side rendered as if they're adjacent —
/// cells just past `width` are drawn at `unwrapped_bx * bs` (could be > width)
/// and the ship's vertices are also unwrapped, so SAT works without wrap math.
fn nearby_solid_cells(pos: Vec2, grid: &BlockGrid, edge_wrap: bool) -> Vec<Vec<Vec2>> {
    let bs = grid.block_size;
    let center_bx = (pos.x / bs).floor() as i64;
    let center_by = (pos.y / bs).floor() as i64;
    // Ship body is at most 15 units from pivot (nose). 15 < bs (35), so the
    // ship triangle can only overlap cells within 1 of the pivot's cell.
    let r: i64 = 1;
    let mut out = Vec::with_capacity(((2 * r + 1) * (2 * r + 1)) as usize);
    for dby in -r..=r {
        for dbx in -r..=r {
            let bx = center_bx + dbx;
            let by = center_by + dby;
            let (lookup_x, lookup_y) = if edge_wrap {
                (
                    bx.rem_euclid(grid.width as i64),
                    by.rem_euclid(grid.height as i64),
                )
            } else {
                (bx, by)
            };
            let cell = grid.get(lookup_x, lookup_y);
            if !cell.is_wall() {
                continue;
            }
            if let Some(poly) = cell_polygon(cell, bx, by, bs) {
                out.push(poly);
            }
        }
    }
    out
}

/// Build the wall features (line segments + isolated corner points) within
/// a ~2-cell radius of `pos`. Each cell's outward-facing edges are emitted
/// only when the neighbour cell is non-solid (so internal shared edges
/// between two solid blocks don't generate spurious bounce surfaces). Slope
/// hypotenuses are always emitted. Each segment carries its outward normal
/// so the bounce direction is unambiguous at the contact line.
///
/// Corners (`WallPt`): one per solid-cell vertex that's actually exposed to
/// open space. These are what catch the "wall corner pokes into ship body"
/// case where a ship edge sweeps past a wall corner mid-tick.
fn nearby_wall_features(
    pos: Vec2,
    grid: &BlockGrid,
    edge_wrap: bool,
) -> (Vec<WallSeg>, Vec<WallPt>) {
    let bs = grid.block_size;
    let center_bx = (pos.x / bs).floor() as i64;
    let center_by = (pos.y / bs).floor() as i64;
    // Ship vertex extends ≤15 from pivot. Per-tick motion at 600 u/s = 10
    // units. So worst-case sweep reaches 25 units = within 1 cell. Use 2 to
    // be safe and to also pick up corners from cells whose edges overlap.
    let r: i64 = 2;
    let mut segs: Vec<WallSeg> = Vec::with_capacity(32);
    let mut pts: Vec<WallPt> = Vec::with_capacity(16);
    let lookup = |bx: i64, by: i64| -> Block {
        if edge_wrap {
            grid.get(
                bx.rem_euclid(grid.width as i64),
                by.rem_euclid(grid.height as i64),
            )
        } else {
            grid.get(bx, by)
        }
    };
    // Helper to add a unique point (cheap dedup — small set).
    let add_pt = |pts: &mut Vec<WallPt>, p: Vec2| {
        for existing in pts.iter() {
            if (existing.p.x - p.x).abs() < 1e-3 && (existing.p.y - p.y).abs() < 1e-3 {
                return;
            }
        }
        pts.push(WallPt { p });
    };
    for dby in -r..=r {
        for dbx in -r..=r {
            let bx = center_bx + dbx;
            let by = center_by + dby;
            let cell = lookup(bx, by);
            if !cell.is_wall() {
                continue;
            }
            let lx = bx as f32 * bs;
            let ly = by as f32 * bs;
            let hx = lx + bs;
            let hy = ly + bs;
            let tl = Vec2::new(lx, ly);
            let tr = Vec2::new(hx, ly);
            let bl = Vec2::new(lx, hy);
            let br = Vec2::new(hx, hy);

            // --- axis-aligned cell edges, only where neighbour is non-solid ---
            // For Wall cells (full square): emit all 4 edges that face an
            // empty neighbour. For slope tiles: emit only the side edges that
            // are part of the filled wedge (Block::has_top/bottom/left/right).
            let neighbour_top = lookup(bx, by - 1);
            let neighbour_bot = lookup(bx, by + 1);
            let neighbour_left = lookup(bx - 1, by);
            let neighbour_right = lookup(bx + 1, by);

            // top edge (TL → TR), outward normal -Y
            if cell.has_top() && !neighbour_top.has_bottom() {
                segs.push(WallSeg {
                    a: tl,
                    b: tr,
                    out_normal: Vec2::new(0.0, -1.0),
                });
                add_pt(&mut pts, tl);
                add_pt(&mut pts, tr);
            }
            // bottom edge (BL → BR), outward normal +Y
            if cell.has_bottom() && !neighbour_bot.has_top() {
                segs.push(WallSeg {
                    a: bl,
                    b: br,
                    out_normal: Vec2::new(0.0, 1.0),
                });
                add_pt(&mut pts, bl);
                add_pt(&mut pts, br);
            }
            // left edge (TL → BL), outward normal -X
            if cell.has_left() && !neighbour_left.has_right() {
                segs.push(WallSeg {
                    a: tl,
                    b: bl,
                    out_normal: Vec2::new(-1.0, 0.0),
                });
                add_pt(&mut pts, tl);
                add_pt(&mut pts, bl);
            }
            // right edge (TR → BR), outward normal +X
            if cell.has_right() && !neighbour_right.has_left() {
                segs.push(WallSeg {
                    a: tr,
                    b: br,
                    out_normal: Vec2::new(1.0, 0.0),
                });
                add_pt(&mut pts, tr);
                add_pt(&mut pts, br);
            }

            // --- slope hypotenuses ---
            // For each Tri* cell, the hypotenuse joins the two non-filled
            // corners; outward normal points away from the filled wedge.
            // (1/sqrt(2) for diagonal normalisation.)
            const D: f32 = 0.70710677; // 1/sqrt(2)
            match cell {
                Block::TriUL => {
                    // filled TL+TR+BL; hyp TR↔BL, outward toward BR (+X+Y)
                    segs.push(WallSeg {
                        a: tr,
                        b: bl,
                        out_normal: Vec2::new(D, D),
                    });
                    add_pt(&mut pts, tr);
                    add_pt(&mut pts, bl);
                }
                Block::TriUR => {
                    // filled TL+TR+BR; hyp TL↔BR, outward toward BL (-X+Y)
                    segs.push(WallSeg {
                        a: tl,
                        b: br,
                        out_normal: Vec2::new(-D, D),
                    });
                    add_pt(&mut pts, tl);
                    add_pt(&mut pts, br);
                }
                Block::TriLL => {
                    // filled TL+BL+BR; hyp TL↔BR, outward toward TR (+X-Y)
                    segs.push(WallSeg {
                        a: tl,
                        b: br,
                        out_normal: Vec2::new(D, -D),
                    });
                    add_pt(&mut pts, tl);
                    add_pt(&mut pts, br);
                }
                Block::TriLR => {
                    // filled TR+BL+BR; hyp TR↔BL, outward toward TL (-X-Y)
                    segs.push(WallSeg {
                        a: tr,
                        b: bl,
                        out_normal: Vec2::new(-D, -D),
                    });
                    add_pt(&mut pts, tr);
                    add_pt(&mut pts, bl);
                }
                _ => {}
            }
        }
    }
    (segs, pts)
}

/// The four world-edge segments for a non-wrap map. Hand-built test maps put
/// these in `map.walls` themselves; for grid maps we synthesise them so we
/// don't have to mark which entries of `map.walls` are bounding vs interior.
fn bounding_segments(w: f32, h: f32) -> [(Vec2, Vec2); 4] {
    [
        (Vec2::new(0.0, 0.0), Vec2::new(w, 0.0)),
        (Vec2::new(w, 0.0), Vec2::new(w, h)),
        (Vec2::new(w, h), Vec2::new(0.0, h)),
        (Vec2::new(0.0, h), Vec2::new(0.0, 0.0)),
    ]
}

/// Result of a swept collision — earliest contact along the ship's motion.
#[derive(Clone, Copy, Debug)]
struct SweepHit {
    /// Fraction of the motion in [0, 1] at which contact occurs.
    t: f32,
    /// Outward normal at the contact (points away from wall, toward open
    /// space). Caller reflects velocity along this normal.
    normal: Vec2,
}

/// Find the earliest fraction t ∈ [0, 1] at which a moving point `p0 + t*d`
/// crosses a static segment a..b. Returns t and the segment-perpendicular
/// outward normal supplied by the caller. None if no crossing in [0, 1].
///
/// This is the standard parametric segment-intersection: the motion ray
/// meets the segment plane at one t, the segment ray hits the motion line
/// at one s. We want both in [0, 1].
fn sweep_point_vs_segment(
    p0: Vec2,
    d: Vec2,
    a: Vec2,
    b: Vec2,
    out_normal: Vec2,
) -> Option<SweepHit> {
    let s = b - a;
    let denom = d.x * s.y - d.y * s.x;
    if denom.abs() < 1e-9 {
        return None; // Motion parallel to segment.
    }
    let qp = a - p0;
    let t = (qp.x * s.y - qp.y * s.x) / denom;
    let u = (qp.x * d.y - qp.y * d.x) / denom;
    // Strict interior on the segment — endpoint hits (at u=0 or u=1) are
    // CORNER contacts, which the wall-vertex-vs-ship-edge sweep handles
    // with a proper corner-bisector-like normal. Catching them here too
    // would double-count and let the (axis-aligned) edge normal win the
    // earliest-t race, producing wrong reflections (e.g. tangent slide
    // along a wall top getting bounced backward by the wall's right-edge
    // normal at the corner).
    if !(0.0..=1.0).contains(&t) {
        return None;
    }
    const U_EPS: f32 = 1e-3;
    if u < U_EPS || u > 1.0 - U_EPS {
        return None;
    }
    // Only count contacts where the point is moving INTO the wall (motion
    // has a component opposite to the outward normal). Otherwise the point
    // is leaving the wall and shouldn't trigger a bounce.
    if d.dot(out_normal) >= 0.0 {
        return None;
    }
    Some(SweepHit { t, normal: out_normal })
}

/// Find the earliest fraction t at which the swept ship triangle (translating
/// by `d` over the tick) sweeps an EDGE across the stationary wall point `p`.
/// Equivalent to: in the ship's frame, the wall point moves by `-d`; find when
/// it crosses any of the 3 ship edges from outside to inside.
///
/// Returns t plus the contact normal — for vertex contacts the normal is the
/// perpendicular to the ship edge that the point crosses, pointing OUTWARD
/// from the ship (i.e. the direction the ship needs to be pushed to clear
/// the corner).
fn sweep_ship_edges_vs_point(
    verts0: &[Vec2; 3],
    d: Vec2,
    p: Vec2,
) -> Option<SweepHit> {
    let mut best: Option<SweepHit> = None;
    for i in 0..3 {
        let a = verts0[i];
        let b = verts0[(i + 1) % 3];
        let edge = b - a;
        // Outward edge normal (CCW-winding-agnostic — we'll flip if needed).
        // For a CCW triangle with edges going (right, up, left), outward is
        // perp-cw. We don't know winding, so flip toward the third vertex.
        let third = verts0[(i + 2) % 3];
        let mut n = Vec2::new(-edge.y, edge.x);
        let nlen_sq = n.length_squared();
        if nlen_sq < 1e-12 {
            continue;
        }
        n = n * (1.0 / math::sqrt(nlen_sq));
        // Make n point AWAY from third (outward).
        if n.dot(third - a) > 0.0 {
            n = n * -1.0;
        }
        // In the ship's frame, point p moves by -d. So the relative motion
        // segment for p is from p (at t=0) to p - d (at t=1). Reuse the same
        // parametric intersection.
        // The point crosses the edge from outside to inside iff (p - a)·n > 0
        // initially (point on outward side), and (p - d - a)·n < 0 at end.
        let initial = (p - a).dot(n);
        let final_ = (p - d - a).dot(n);
        if initial <= 0.0 || final_ >= 0.0 {
            continue;
        }
        // Solve for t: ((p - t*d) - a)·n = 0 → t = (p - a)·n / d·n.
        let denom = d.dot(n);
        if denom.abs() < 1e-9 {
            continue;
        }
        let t = initial / denom;
        if !(0.0..=1.0).contains(&t) {
            continue;
        }
        // Check the contact lies WITHIN the edge segment (not past either
        // endpoint).
        let contact = p - d * t;
        let edge_len_sq = edge.length_squared();
        if edge_len_sq < 1e-12 {
            continue;
        }
        let s = (contact - a).dot(edge) / edge_len_sq;
        if !(0.0..=1.0).contains(&s) {
            continue;
        }
        // Outward normal for the bounce: opposite of the ship-edge inward
        // normal — i.e., the wall point should "push the ship" along +n
        // direction (since +n is the ship's outward, pushing the ship in +n
        // moves it AWAY from the corner that was poking it).
        if best.map_or(true, |h| t < h.t) {
            best = Some(SweepHit { t, normal: n });
        }
    }
    best
}

/// Run the full feature-level swept collision for one tick of translation.
/// Returns the earliest hit (or None if the path is clear). Combines:
///   - each ship vertex's path vs each nearby wall segment (vertex-vs-line)
///   - each nearby wall corner's relative path vs each ship edge (corner-vs-edge)
///
/// The earlier of the two is the actual contact. This matches xpilot-ng's
/// dual-sweep approach and correctly handles both flat-wall hits and
/// corner-pokes-ship-body cases without resorting to depth-of-overlap MTV.
fn swept_collide_translation(
    prev_pos: Vec2,
    delta: Vec2,
    angle: f32,
    map: &Map,
) -> Option<SweepHit> {
    let mut earliest: Option<SweepHit> = None;
    let mut consider = |hit: Option<SweepHit>| {
        if let Some(h) = hit {
            if earliest.map_or(true, |e: SweepHit| h.t < e.t) {
                earliest = Some(h);
            }
        }
    };

    let verts = ship_verts_at(prev_pos, angle);

    // Walls from the grid.
    if let Some(grid) = map.blocks.as_ref() {
        let (segs, pts) = nearby_wall_features(prev_pos, grid, map.edge_wrap);
        // Ship vertices sweep against wall segments.
        for v in &verts {
            for seg in &segs {
                consider(sweep_point_vs_segment(*v, delta, seg.a, seg.b, seg.out_normal));
            }
        }
        // Wall corners "swept" backward into ship edges.
        for pt in &pts {
            consider(sweep_ship_edges_vs_point(&verts, delta, pt.p));
        }
        // Bounding box for non-wrap maps.
        if !map.edge_wrap {
            for (a, b) in bounding_segments(map.width, map.height) {
                let mid = (a + b) * 0.5;
                let inward = Vec2::new(map.width * 0.5, map.height * 0.5) - mid;
                let inward_len = inward.length();
                let inward_unit = if inward_len > 1e-6 {
                    inward * (1.0 / inward_len)
                } else {
                    Vec2::new(0.0, 0.0)
                };
                // Outward normal of the bounding wall = away from world center.
                let out_normal = inward_unit * -1.0;
                for v in &verts {
                    consider(sweep_point_vs_segment(*v, delta, a, b, out_normal));
                }
            }
        }
    } else {
        // Hand-built test maps: each entry in map.walls is a segment. Outward
        // normal isn't stored, so derive it from any of the ship vertices that
        // is currently outside the wall plane.
        for w in &map.walls {
            let s = w.b - w.a;
            let mut n = Vec2::new(-s.y, s.x);
            let nlen_sq = n.length_squared();
            if nlen_sq < 1e-12 {
                continue;
            }
            n = n * (1.0 / math::sqrt(nlen_sq));
            // Choose the side the ship's pivot is on; that's "outward".
            if (prev_pos - w.a).dot(n) < 0.0 {
                n = n * -1.0;
            }
            for v in &verts {
                consider(sweep_point_vs_segment(*v, delta, w.a, w.b, n));
            }
        }
    }

    earliest
}

/// Ship triangle vertices at an arbitrary `(pos, angle)` pose — used by the
/// TOI sweep below without mutating the ship.
fn ship_verts_at(pos: Vec2, angle: f32) -> [Vec2; 3] {
    let cos_a = math::cos(angle);
    let sin_a = math::sin(angle);
    let mut out = [Vec2::ZERO; 3];
    for (i, p) in crate::entities::SHIP_BODY.iter().enumerate() {
        out[i] = Vec2::new(
            pos.x + p.x * cos_a - p.y * sin_a,
            pos.y + p.x * sin_a + p.y * cos_a,
        );
    }
    out
}

/// True iff a ship at `(pos, angle)` overlaps any wall geometry. Cheap-out:
/// returns on the first overlap found rather than computing depths.
fn has_wall_overlap_at(pos: Vec2, angle: f32, map: &Map) -> bool {
    let verts = ship_verts_at(pos, angle);
    if let Some(grid) = map.blocks.as_ref() {
        for poly in nearby_solid_cells(pos, grid, map.edge_wrap) {
            if physics::polygon_polygon_collide(&verts, &poly).is_some() {
                return true;
            }
        }
        if !map.edge_wrap {
            for (a, b) in bounding_segments(map.width, map.height) {
                if physics::polygon_segment_collide(&verts, a, b).is_some() {
                    return true;
                }
            }
        }
    } else {
        for w in &map.walls {
            if physics::polygon_segment_collide(&verts, w.a, w.b).is_some() {
                return true;
            }
        }
    }
    false
}

/// MTV from `verts` overlapping `cell_poly`, restricted to cell-edge axes
/// only — never ship-edge normals. This is the difference between "Elm-like
/// predictable bounce against a wall" and "weird sideways pop because the
/// ship's diagonal-edge normal happened to have the smallest overlap."
///
/// For a square Wall cell that's just x and y axes (the response is always
/// axis-aligned, like the original Elm version). For a slope tile the cell's
/// hypotenuse normal is also a candidate, so slopes still bounce diagonally.
///
/// Detection still uses *full* SAT (also tests ship-edge normals) — without
/// that we'd get false positives near corners. Only the response direction
/// is restricted.
fn cell_axis_mtv(verts: &[Vec2], cell_poly: &[Vec2]) -> Option<(Vec2, f32)> {
    // Detection: full SAT — bail if no overlap.
    physics::polygon_polygon_collide(verts, cell_poly)?;

    // Response: scan cell-edge axes for the smallest overlap.
    let mut min_depth = f32::MAX;
    let mut min_axis = Vec2::ZERO;
    let n = cell_poly.len();
    for i in 0..n {
        let p1 = cell_poly[i];
        let p2 = cell_poly[(i + 1) % n];
        let edge = p2 - p1;
        let len_sq = edge.length_squared();
        if len_sq < 1e-12 {
            continue;
        }
        let inv = 1.0 / math::sqrt(len_sq);
        let axis = Vec2::new(-edge.y * inv, edge.x * inv);
        let (vmin, vmax) = project_axis(verts, axis);
        let (cmin, cmax) = project_axis(cell_poly, axis);
        let depth = (vmax - cmin).min(cmax - vmin);
        if depth > 0.0 && depth < min_depth {
            min_depth = depth;
            min_axis = axis;
        }
    }
    if min_depth == f32::MAX {
        return None;
    }
    // Orient: push from cell toward ship.
    let mut cv = Vec2::ZERO;
    for v in verts {
        cv += *v;
    }
    let cv = cv * (1.0 / verts.len() as f32);
    let mut cc = Vec2::ZERO;
    for v in cell_poly {
        cc += *v;
    }
    let cc = cc * (1.0 / n as f32);
    if (cv - cc).dot(min_axis) < 0.0 {
        min_axis = min_axis * -1.0;
    }
    Some((min_axis, min_depth))
}

fn project_axis(verts: &[Vec2], axis: Vec2) -> (f32, f32) {
    let mut pmin = f32::MAX;
    let mut pmax = f32::MIN;
    for v in verts {
        let p = v.dot(axis);
        if p < pmin {
            pmin = p;
        }
        if p > pmax {
            pmax = p;
        }
    }
    (pmin, pmax)
}

/// Single deepest cell-axis-restricted contact at `(pos, angle)`. Used by the
/// TOI sweep to sample the contact normal at the just-into-overlap pose, and
/// by the residual-overlap fallback. Restricting to cell axes = predictable
/// axis-aligned response, no ship-diagonal pop-out.
fn deepest_contact_at(pos: Vec2, angle: f32, map: &Map) -> Option<(Vec2, f32)> {
    let verts = ship_verts_at(pos, angle);
    let mut deepest: Option<(Vec2, f32)> = None;
    let mut consider = |opt: Option<(Vec2, f32)>, acc: &mut Option<(Vec2, f32)>| {
        if let Some((n, d)) = opt {
            if acc.map_or(true, |(_, dd)| d > dd) {
                *acc = Some((n, d));
            }
        }
    };
    if let Some(grid) = map.blocks.as_ref() {
        for poly in nearby_solid_cells(pos, grid, map.edge_wrap) {
            consider(cell_axis_mtv(&verts, &poly), &mut deepest);
        }
        if !map.edge_wrap {
            for (a, b) in bounding_segments(map.width, map.height) {
                // Bounding segments are 1-D; their wall_perp IS the only axis,
                // so the existing helper is already cell-axis-only.
                consider(physics::polygon_segment_collide(&verts, a, b), &mut deepest);
            }
        }
    } else {
        for w in &map.walls {
            consider(physics::polygon_segment_collide(&verts, w.a, w.b), &mut deepest);
        }
    }
    deepest
}

/// Iteratively push the ship out of any wall geometry it overlaps. Returns
/// the FIRST contact's normal (used by the caller for impact-speed reporting
/// and the wall-crash kill check), or `None` if no overlap.
///
/// Per-iteration strategy:
/// - Find the SINGLE deepest contact (largest MTV depth) across all nearby
///   cells + bounding segments. We don't sum MTVs across multiple cells —
///   that creates an artificial push that's bigger than any single exit move,
///   and at a convex corner makes the bounce normal a blend of two faces
///   that doesn't match the surface the ship actually hit.
/// - Push by exactly that MTV (along its axis, by its depth).
/// - Velocity response gated by `MIN_BOUNCE_SPEED`:
///     - hard hit (`vn <= -MIN_BOUNCE_SPEED`) → reflect with restitution.
///     - shallow (`-MIN_BOUNCE_SPEED < vn < 0`) → flatten inward component
///       (slide); no restitution. This kills the corner-chatter where SAT
///       found a sub-pixel overlap, the old reflect produced a sub-pixel
///       outward velocity, the ship re-grazed next frame, and so on.
///     - moving away (`vn >= 0`) → leave velocity alone.
/// - At concave pockets the second iteration finds the second wall, pushes,
///   and applies the same gated response. Over 1–2 iterations the ship's
///   inward components against every contacted wall are zeroed (or bounced
///   if hard enough).
/// - After the loop, lift off by `CONTACT_SLOP` along the LAST contact normal
///   so SAT's float-precision tail can't refire next tick.
fn resolve_wall_overlaps(ship: &mut Ship, map: &Map) -> Option<Vec2> {
    let mut first_normal: Option<Vec2> = None;
    let mut last_normal: Option<Vec2> = None;
    for _ in 0..COLLISION_ITERS {
        let (normal, depth) = match deepest_contact_at(ship.pos, ship.angle, map) {
            Some(p) => p,
            None => break,
        };
        ship.pos += normal * depth;
        ship.vel = physics::reflect(ship.vel, normal, WALL_RESTITUTION);
        if first_normal.is_none() {
            first_normal = Some(normal);
        }
        last_normal = Some(normal);
    }
    if let Some(n) = last_normal {
        ship.pos += n * CONTACT_SLOP;
    }
    first_normal
}

pub struct World {
    pub tick: u32,
    pub ships: BTreeMap<EntityId, Ship>,
    pub bullets: BTreeMap<EntityId, Bullet>,
    /// Live explosion debris. Each particle is a real sim entity: it moves,
    /// dies on walls, and pushes any ship it touches by transferring its
    /// momentum. No more radial blast field — what you see is exactly what
    /// applies the force.
    pub particles: Vec<Particle>,
    /// Active turret cells (uppercase R/C/D/F in the .xp grid). Keyed by
    /// grid cell so client + server agree on identity without an entity-id
    /// map. The block at the cell tells us the firing direction.
    pub cannons: BTreeMap<(u32, u32), CannonRuntime>,
    pub map: Map,
    next_entity_id: EntityId,
    /// Deterministic xorshift32 RNG state. Drives explosion-particle spawn
    /// directions/speeds/lifetimes so server runs are reproducible.
    rng_state: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct CannonRuntime {
    /// Stable id used as the `shooter` on bullets this cannon fires.
    /// Allocated at world init from the same EntityId pool as ships/bullets.
    pub entity_id: EntityId,
    pub alive: bool,
    pub hp: u32,
    /// Tick at which a dead cannon revives. Unused while alive.
    pub respawn_at_tick: u32,
    /// Tick at which the next shot is fired. Updated on each fire.
    pub fire_at_tick: u32,
}

/// One piece of explosion debris. When a particle's swept path this tick
/// crosses a ship's triangle, the particle dies and the ship absorbs
/// `vel * mass / ship.mass` of velocity. Walls kill particles outright.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Particle {
    pub pos: Vec2,
    pub vel: Vec2,
    pub age: f32,
    pub life: f32,
    pub mass: f32,
}

impl World {
    pub fn new(mut map: Map) -> Self {
        // The wire form of Map ships an empty `walls` (skipped in serde) —
        // recompute from blocks now so the sim has something to collide
        // against. No-op for hand-built maps that don't have blocks.
        map.rebuild_walls();
        let mut s = Self {
            tick: 0,
            ships: BTreeMap::new(),
            bullets: BTreeMap::new(),
            particles: Vec::new(),
            cannons: BTreeMap::new(),
            map,
            next_entity_id: 1,
            rng_state: 0x9E37_79B1, // arbitrary nonzero
        };
        // Collect cannon cells in a separate pass so the init loop can use
        // &mut self for `alloc_id` and `random_fire_delay_ticks` without
        // borrowing the grid for the whole loop.
        let cannon_cells: Vec<(u32, u32)> = if let Some(grid) = s.map.blocks.as_ref() {
            let mut cells = Vec::new();
            for y in 0..grid.height {
                for x in 0..grid.width {
                    if grid.get(x as i64, y as i64).cannon_fire().is_some() {
                        cells.push((x, y));
                    }
                }
            }
            cells
        } else {
            Vec::new()
        };
        for cell in cannon_cells {
            let entity_id = s.alloc_id();
            let initial_delay = s.random_fire_delay_ticks();
            s.cannons.insert(
                cell,
                CannonRuntime {
                    entity_id,
                    alive: true,
                    hp: CANNON_HP,
                    respawn_at_tick: 0,
                    fire_at_tick: initial_delay,
                },
            );
        }
        s
    }

    /// Draw a uniform random fire delay in ticks from [MIN, MAX] seconds.
    /// Advances the deterministic RNG state.
    fn random_fire_delay_ticks(&mut self) -> u32 {
        let r = self.next_rand();
        let secs = CANNON_FIRE_MIN_SECONDS
            + r * (CANNON_FIRE_MAX_SECONDS - CANNON_FIRE_MIN_SECONDS);
        (secs / TICK_DT_SECONDS) as u32
    }

    /// xorshift32, kept inline so this whole sim stays free of `rand` deps.
    /// Returns f32 in [0, 1).
    fn next_rand(&mut self) -> f32 {
        let mut x = self.rng_state;
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        self.rng_state = x;
        (x as f32) / (u32::MAX as f32)
    }

    /// Spawn `count` explosion particles at `center`, each with `base_vel`
    /// (the dead ship's velocity) plus a random outward burst.
    fn spawn_explosion_particles(&mut self, center: Vec2, base_vel: Vec2, count: u32) {
        for _ in 0..count {
            let r1 = self.next_rand();
            let r2 = self.next_rand();
            let r3 = self.next_rand();
            let angle = r1 * core::f32::consts::TAU;
            let speed = PARTICLE_SPEED_MIN + r2 * (PARTICLE_SPEED_MAX - PARTICLE_SPEED_MIN);
            let life = PARTICLE_LIFE_MIN + r3 * (PARTICLE_LIFE_MAX - PARTICLE_LIFE_MIN);
            let outward = Vec2::new(math::cos(angle), math::sin(angle)) * speed;
            self.particles.push(Particle {
                pos: center,
                vel: base_vel + outward,
                age: 0.0,
                life,
                mass: PARTICLE_MASS,
            });
        }
    }

    fn alloc_id(&mut self) -> EntityId {
        let id = self.next_entity_id;
        self.next_entity_id += 1;
        id
    }

    pub fn spawn_ship(&mut self, player_id: PlayerId, pos: Vec2, angle: f32) -> EntityId {
        let entity_id = self.alloc_id();
        self.insert_ship(entity_id, player_id, pos, angle);
        entity_id
    }

    /// Re-insert a ship at a fixed entity_id. Used on respawn so the same
    /// player keeps the same id across deaths (clients reference it for
    /// prediction and HUD attribution).
    pub fn respawn_ship(&mut self, entity_id: EntityId, player_id: PlayerId, pos: Vec2, angle: f32) {
        self.insert_ship(entity_id, player_id, pos, angle);
    }

    fn insert_ship(&mut self, entity_id: EntityId, player_id: PlayerId, pos: Vec2, angle: f32) {
        let ship = Ship {
            entity_id,
            player_id,
            pos,
            vel: Vec2::ZERO,
            angle,
            mass: SHIP_MASS,
            hp: SHIP_MAX_HP,
            thrusting: false,
            fire_cooldown: 0.0,
            shot_charge: SHOT_CHARGE_MAX,
            last_input: TickInput::default(),
        };
        self.ships.insert(entity_id, ship);
    }

    /// Advance the sim by one fixed tick. `inputs` is keyed by ship entity id.
    /// Iteration order is BTreeMap-stable for determinism. Returns the events
    /// that fired this tick so the caller (server room) can broadcast them.
    pub fn step(&mut self, inputs: &BTreeMap<EntityId, TickInput>) -> Vec<GameEvent> {
        let mut events: Vec<GameEvent> = Vec::new();
        let dt = TICK_DT_SECONDS;

        // 0. Cannons — revive any whose respawn timer has expired and fire
        //    a bullet from each alive turret due this tick. Done first so
        //    cannon-fired bullets enter the same ship/wall pipeline below.
        self.step_cannons();

        // 1. Per-ship dynamics + collect new-bullet intents + record any
        //    wall-crash impact severity for the death pass below.
        let mut new_bullets: Vec<NewBullet> = Vec::new();
        let mut wall_crashes: Vec<(EntityId, WallImpact)> = Vec::new();
        for (id, ship) in self.ships.iter_mut() {
            let input = inputs.get(id).copied().unwrap_or_default();
            if let Some(impact) = apply_ship_dynamics(ship, &input, &self.map) {
                let threshold = if impact.vertex_idx == 2 {
                    SHIP_NOSE_KILL_SPEED
                } else {
                    SHIP_BACK_KILL_SPEED
                };
                if impact.impact_speed > threshold {
                    wall_crashes.push((*id, impact));
                }
            }
            if let Some(b) = try_fire(ship, &input, &self.map) {
                new_bullets.push(b);
            }
        }
        for b in new_bullets {
            let entity_id = self.alloc_id();
            self.bullets.insert(
                entity_id,
                Bullet {
                    entity_id,
                    shooter: b.shooter,
                    pos: b.pos,
                    vel: b.vel,
                    mass: BULLET_MASS,
                    age_seconds: 0.0,
                },
            );
        }

        // 1b. Crash deaths. No killer attribution — ship rammed itself into
        //     a wall. Spawns explosion debris like a bullet death.
        for (id, _impact) in wall_crashes {
            let Some(s) = self.ships.remove(&id) else { continue };
            let (p, v) = (s.pos, s.vel);
            events.push(GameEvent::ShipDied {
                entity_id: id,
                killer: None,
                pos: p,
                vel: v,
            });
            self.spawn_explosion_particles(p, v, EXPLOSION_PARTICLE_COUNT);
        }

        // 2. Thruster wash — every thrusting ship emits a force cone behind
        //    it that pushes other ships and bullets in that cone.
        apply_thruster_wash(self, dt);

        // 3. Ship-ship collision: any two triangles that overlap, both die.
        let ship_ids: Vec<EntityId> = self.ships.keys().copied().collect();
        let mut pairs: Vec<(EntityId, EntityId)> = Vec::new();
        for i in 0..ship_ids.len() {
            for j in (i + 1)..ship_ids.len() {
                let a_id = ship_ids[i];
                let b_id = ship_ids[j];
                let a_verts = self.ships[&a_id].world_vertices();
                let b_verts = self.ships[&b_id].world_vertices();
                if physics::triangles_overlap(&a_verts, &b_verts) {
                    pairs.push((a_id, b_id));
                }
            }
        }
        for (a_id, b_id) in pairs {
            let a_player = self.ships.get(&a_id).map(|s| s.player_id);
            let b_player = self.ships.get(&b_id).map(|s| s.player_id);
            if let Some(s) = self.ships.remove(&a_id) {
                let (p, v) = (s.pos, s.vel);
                events.push(GameEvent::ShipDied {
                    entity_id: a_id,
                    killer: b_player,
                    pos: p,
                    vel: v,
                });
                self.spawn_explosion_particles(p, v, EXPLOSION_PARTICLE_COUNT);
            }
            if let Some(s) = self.ships.remove(&b_id) {
                let (p, v) = (s.pos, s.vel);
                events.push(GameEvent::ShipDied {
                    entity_id: b_id,
                    killer: a_player,
                    pos: p,
                    vel: v,
                });
                self.spawn_explosion_particles(p, v, EXPLOSION_PARTICLE_COUNT);
            }
        }

        // 3b. Ship vs cannon. A ship's triangle entering the cannon's open-air
        //     firing-triangle AABB kills both. Wall-block contact is handled
        //     by normal ship-vs-wall physics — this only catches contact with
        //     the visible cannon barrel sticking out into open space.
        let bs_opt = self.map.blocks.as_ref().map(|g| g.block_size);
        if let Some(bs) = bs_opt {
            // Build hits in a scope that holds an immutable borrow on the
            // grid; the mutating apply pass below re-fetches the block per
            // cell so we don't hold that borrow across `spawn_explosion_*`.
            let sc_hits: Vec<(EntityId, (u32, u32))> = {
                let grid = self.map.blocks.as_ref().unwrap();
                let mut hits = Vec::new();
                for (s_id, ship) in &self.ships {
                    let verts = ship.world_vertices();
                    for (cell, cannon) in &self.cannons {
                        if !cannon.alive {
                            continue;
                        }
                        let block = grid.get(cell.0 as i64, cell.1 as i64);
                        let Some((min, max)) = cannon_hit_aabb(cell.0, cell.1, block, bs) else {
                            continue;
                        };
                        if verts.iter().any(|v| {
                            v.x >= min.x && v.x <= max.x && v.y >= min.y && v.y <= max.y
                        }) {
                            hits.push((*s_id, *cell));
                            break;
                        }
                    }
                }
                hits
            };
            let respawn_ticks = (CANNON_RESPAWN_SECONDS / TICK_DT_SECONDS) as u32;
            let now = self.tick;
            for (s_id, cell) in sc_hits {
                // Snapshot the cannon centroid before any mutating call —
                // releases the grid borrow so spawn_explosion_particles can
                // take &mut self.
                let cannon_center = {
                    let grid = self.map.blocks.as_ref().unwrap();
                    let block = grid.get(cell.0 as i64, cell.1 as i64);
                    cannon_triangle_center(cell.0, cell.1, block, bs).unwrap_or(Vec2::new(
                        (cell.0 as f32 + 0.5) * bs,
                        (cell.1 as f32 + 0.5) * bs,
                    ))
                };
                if let Some(s) = self.ships.remove(&s_id) {
                    let (p, v) = (s.pos, s.vel);
                    events.push(GameEvent::ShipDied {
                        entity_id: s_id,
                        killer: None, // cannon kills don't credit a player
                        pos: p,
                        vel: v,
                    });
                    self.spawn_explosion_particles(p, v, EXPLOSION_PARTICLE_COUNT);
                }
                // Cannon dies too — the wall block stays, only the runtime
                // turret goes offline until respawn.
                let kill_cannon = self
                    .cannons
                    .get(&cell)
                    .map(|c| c.alive)
                    .unwrap_or(false);
                if kill_cannon {
                    if let Some(c) = self.cannons.get_mut(&cell) {
                        c.alive = false;
                        c.respawn_at_tick = now + respawn_ticks;
                    }
                    self.spawn_explosion_particles(
                        cannon_center,
                        Vec2::ZERO,
                        EXPLOSION_PARTICLE_COUNT,
                    );
                }
            }
        }

        // 4. Step bullets. Capture each bullet's prev pos for the swept hit test.
        let mut bullet_prev: BTreeMap<EntityId, Vec2> = BTreeMap::new();
        let mut dead_bullets: Vec<EntityId> = Vec::new();
        for (id, bullet) in self.bullets.iter_mut() {
            bullet_prev.insert(*id, bullet.pos);
            if step_bullet(bullet, &self.map) {
                dead_bullets.push(*id);
            }
        }
        for id in &dead_bullets {
            self.bullets.remove(id);
        }

        // 4b. Bullet vs cannon hits. Run before bullet-vs-ship so a bullet
        //     that would also have hit a ship behind the cannon is consumed
        //     here (the wall block always blocks anyway). Cannon-fired
        //     bullets are excluded so cannons don't friendly-fire each other.
        self.step_bullet_cannon_hits();

        // 5. Bullet vs ship hits (swept). Iterate in id order for determinism.
        let mut hits: Vec<(EntityId, EntityId)> = Vec::new();
        'bullets: for (b_id, bullet) in &self.bullets {
            let prev = match bullet_prev.get(b_id) {
                Some(p) => *p,
                None => bullet.pos,
            };
            for (s_id, ship) in &self.ships {
                if ship.entity_id == bullet.shooter
                    && bullet.age_seconds < BULLET_SELF_HIT_GRACE_SECONDS
                {
                    continue;
                }
                let verts = ship.world_vertices();
                if physics::segment_hits_triangle(prev, bullet.pos, &verts) {
                    hits.push((*b_id, *s_id));
                    continue 'bullets;
                }
            }
        }

        // 5b. Step explosion debris. Walls absorb particles outright; ships
        // absorb them and gain `vel * mass / ship.mass` of velocity. No more
        // radial blast field — what you see is what pushes you.
        step_particles(self, dt);

        // 6. Apply bullet hits.
        for (b_id, s_id) in hits {
            let bullet = match self.bullets.remove(&b_id) {
                Some(b) => b,
                None => continue,
            };
            let shooter_player = self.ships.get(&bullet.shooter).map(|s| s.player_id);
            let victim_player = match self.ships.get(&s_id) {
                Some(s) => s.player_id,
                None => continue,
            };

            let new_hp = self
                .ships
                .get(&s_id)
                .map(|s| s.hp)
                .unwrap_or(0)
                .saturating_sub(BULLET_DAMAGE);
            if new_hp == 0 {
                let (death_pos, death_vel) = self
                    .ships
                    .get(&s_id)
                    .map(|s| (s.pos, s.vel))
                    .unwrap_or((Vec2::ZERO, Vec2::ZERO));
                self.ships.remove(&s_id);
                events.push(GameEvent::ShipDied {
                    entity_id: s_id,
                    killer: shooter_player,
                    pos: death_pos,
                    vel: death_vel,
                });
                self.spawn_explosion_particles(death_pos, death_vel, EXPLOSION_PARTICLE_COUNT);
            } else {
                if let Some(s) = self.ships.get_mut(&s_id) {
                    s.hp = new_hp;
                }
                events.push(GameEvent::HitScored {
                    shooter: shooter_player.unwrap_or(0),
                    victim: victim_player,
                    damage: BULLET_DAMAGE,
                });
            }
        }

        // 7. Wrap surviving bullets. MUST be after the hit test — wrapping
        // earlier would make the swept hit segment from the pre-step pos
        // to a wrapped pos slice across the entire map.
        if self.map.edge_wrap {
            let (w, h) = (self.map.width, self.map.height);
            for bullet in self.bullets.values_mut() {
                bullet.pos = wrap_pos(bullet.pos, w, h);
            }
        }

        self.tick = self.tick.wrapping_add(1);
        events
    }

    fn step_cannons(&mut self) {
        let Some(grid) = self.map.blocks.as_ref() else { return };
        let bs = grid.block_size;
        let now = self.tick;

        // For each cannon, lead-aim each candidate ship (using the
        // mid-range bullet speed for the geometry — actual per-shot speed
        // is rolled below), keep only those whose intercept direction lies
        // within the fan AND whose bullet would land within its lifetime.
        // Pick the closest valid target; if none, skip firing entirely so
        // the cannon stays silent until something walks in.
        let cos_fan = math::cos(CANNON_FIRE_FAN_RAD);
        let aim_speed =
            (CANNON_BULLET_SPEED_MIN + CANNON_BULLET_SPEED_MAX) * 0.5;
        let max_intercept_seconds = BULLET_LIFETIME_SECONDS;
        struct FireIntent {
            cell: (u32, u32),
            shooter: EntityId,
            tip: Vec2,
            aim_dir: Vec2,
        }
        let mut intents: Vec<FireIntent> = Vec::new();
        for ((x, y), cannon) in &self.cannons {
            if !cannon.alive || now < cannon.fire_at_tick {
                continue;
            }
            let block = grid.get(*x as i64, *y as i64);
            let Some((_, base_dir)) = block.cannon_fire() else {
                continue;
            };
            let cx = (*x as f32 + 0.5) * bs;
            let cy = (*y as f32 + 0.5) * bs;
            let cannon_pos = Vec2::new(cx, cy);
            let tip_offset = bs * (0.5 + CANNON_TRIANGLE_FRAC) + CANNON_SIT_OFFSET;
            let tip = Vec2::new(
                cx + base_dir.x * tip_offset,
                cy + base_dir.y * tip_offset,
            );
            let mut best: Option<(Vec2, f32)> = None; // (aim_dir, dist)
            for ship in self.ships.values() {
                let to_ship = ship.pos - cannon_pos;
                let dist = to_ship.length();
                if dist < 1e-3 {
                    continue;
                }
                let Some((aim_dir, t)) = physics::lead_aim(
                    tip,
                    Vec2::ZERO, // cannons are stationary
                    ship.pos,
                    ship.vel,
                    aim_speed,
                ) else {
                    continue;
                };
                // Aim must lie inside the fan around the cannon's nominal
                // direction — otherwise the cannon would have to swivel
                // unrealistically far.
                let cos_aim = aim_dir.x * base_dir.x + aim_dir.y * base_dir.y;
                if cos_aim < cos_fan {
                    continue;
                }
                // Skip targets too far for the bullet to reach within its
                // lifetime (avoids cannons pinging the void at maps' edges).
                if t > max_intercept_seconds {
                    continue;
                }
                match best {
                    Some((_, d)) if dist >= d => {}
                    _ => best = Some((aim_dir, dist)),
                }
            }
            if let Some((aim_dir, _)) = best {
                intents.push(FireIntent {
                    cell: (*x, *y),
                    shooter: cannon.entity_id,
                    tip,
                    aim_dir,
                });
            }
            // No valid target: keep fire_at_tick where it is so the cannon
            // re-checks every subsequent step until something appears.
        }
        for it in intents {
            // Aim noise — small angular jitter around the perfect lead
            // shot so cannons aren't pixel-perfect snipers.
            let r = self.next_rand();
            let noise = (r - 0.5) * 2.0 * CANNON_AIM_NOISE_RAD;
            let aim_angle = math::atan2(it.aim_dir.x, -it.aim_dir.y);
            let fire_angle = aim_angle + noise;
            let dir = forward(fire_angle);
            // Speed randomized per shot — slower than player bullets.
            let speed_r = self.next_rand();
            let speed = CANNON_BULLET_SPEED_MIN
                + speed_r * (CANNON_BULLET_SPEED_MAX - CANNON_BULLET_SPEED_MIN);
            let vel = dir * speed;
            let id = self.alloc_id();
            self.bullets.insert(
                id,
                Bullet {
                    entity_id: id,
                    shooter: it.shooter,
                    pos: it.tip,
                    vel,
                    mass: BULLET_MASS,
                    age_seconds: 0.0,
                },
            );
            let next_delay = self.random_fire_delay_ticks();
            if let Some(c) = self.cannons.get_mut(&it.cell) {
                c.fire_at_tick = now + next_delay;
            }
        }

        // Revive dead cannons whose timer expired. Their first post-respawn
        // shot also gets a random delay.
        let to_revive: Vec<(u32, u32)> = self
            .cannons
            .iter()
            .filter(|(_, c)| !c.alive && now >= c.respawn_at_tick)
            .map(|(cell, _)| *cell)
            .collect();
        for cell in to_revive {
            let initial_delay = self.random_fire_delay_ticks();
            if let Some(c) = self.cannons.get_mut(&cell) {
                c.alive = true;
                c.hp = CANNON_HP;
                c.fire_at_tick = now + initial_delay;
            }
        }
    }

    fn step_bullet_cannon_hits(&mut self) {
        let bs_opt = self.map.blocks.as_ref().map(|g| g.block_size);
        let Some(bs) = bs_opt else { return };
        let respawn_ticks = (CANNON_RESPAWN_SECONDS / TICK_DT_SECONDS) as u32;
        let now = self.tick;

        // Set of shooter ids belonging to any cannon — bullets fired by
        // cannons skip the cannon-vs-cannon hit test (no friendly fire).
        let cannon_shooters: std::collections::BTreeSet<EntityId> =
            self.cannons.values().map(|c| c.entity_id).collect();

        let hits: Vec<(EntityId, (u32, u32))> = {
            let grid = self.map.blocks.as_ref().unwrap();
            let mut hs = Vec::new();
            'bullets: for (b_id, bullet) in &self.bullets {
                if cannon_shooters.contains(&bullet.shooter) {
                    continue;
                }
                for (cell, cannon) in &self.cannons {
                    if !cannon.alive {
                        continue;
                    }
                    let block = grid.get(cell.0 as i64, cell.1 as i64);
                    let Some((min, max)) = cannon_hit_aabb(cell.0, cell.1, block, bs) else {
                        continue;
                    };
                    if bullet.pos.x >= min.x
                        && bullet.pos.x <= max.x
                        && bullet.pos.y >= min.y
                        && bullet.pos.y <= max.y
                    {
                        hs.push((*b_id, *cell));
                        continue 'bullets;
                    }
                }
            }
            hs
        };

        for (b_id, cell) in hits {
            self.bullets.remove(&b_id);
            // Snapshot center first so the spawn call below doesn't race the
            // grid borrow.
            let center = {
                let grid = self.map.blocks.as_ref().unwrap();
                let block = grid.get(cell.0 as i64, cell.1 as i64);
                cannon_triangle_center(cell.0, cell.1, block, bs).unwrap_or(Vec2::new(
                    (cell.0 as f32 + 0.5) * bs,
                    (cell.1 as f32 + 0.5) * bs,
                ))
            };
            let mut killed = false;
            if let Some(cannon) = self.cannons.get_mut(&cell) {
                if !cannon.alive {
                    continue;
                }
                cannon.hp = cannon.hp.saturating_sub(BULLET_DAMAGE);
                if cannon.hp == 0 {
                    cannon.alive = false;
                    cannon.respawn_at_tick = now + respawn_ticks;
                    killed = true;
                }
            }
            if killed {
                // Full ship-style burst — same particle count, same kick.
                // The debris pushes any nearby ship around just like a ship
                // explosion would.
                self.spawn_explosion_particles(center, Vec2::ZERO, EXPLOSION_PARTICLE_COUNT);
            }
        }
    }
}

/// World-space centroid of the cannon's visible firing triangle. Used as
/// the explosion origin so debris launches from where the player saw the
/// turret die — not from inside the wall block where the cell center is.
pub fn cannon_triangle_center(x: u32, y: u32, block: Block, bs: f32) -> Option<Vec2> {
    let (_, dir) = block.cannon_fire()?;
    let cx = (x as f32 + 0.5) * bs;
    let cy = (y as f32 + 0.5) * bs;
    // Triangle base is at `0.5*bs + SIT_OFFSET` from the cell center along
    // the firing direction; tip is `TRIANGLE_FRAC*bs` further. Centroid is
    // 1/3 of the way from base toward tip.
    let depth = bs * (0.5 + CANNON_TRIANGLE_FRAC / 3.0) + CANNON_SIT_OFFSET;
    Some(Vec2::new(cx + dir.x * depth, cy + dir.y * depth))
}

/// AABB of the cannon's visible firing triangle, in world coordinates. None
/// for non-cannon blocks. Used for bullet-vs-cannon and ship-vs-cannon hit
/// detection — the wall block itself is handled by normal wall collision.
/// Base is inset by `CANNON_BASE_INSET` on each end + the whole triangle is
/// shifted out by `CANNON_SIT_OFFSET` so the hitbox tracks the visual.
pub fn cannon_hit_aabb(x: u32, y: u32, block: Block, bs: f32) -> Option<(Vec2, Vec2)> {
    let f = CANNON_TRIANGLE_FRAC;
    let inset = CANNON_BASE_INSET;
    let sit = CANNON_SIT_OFFSET;
    let lx = x as f32 * bs;
    let ly = y as f32 * bs;
    let hx = lx + bs;
    let hy = ly + bs;
    match block {
        Block::CannonFireUp => Some((
            Vec2::new(lx + inset, ly - sit - f * bs),
            Vec2::new(hx - inset, ly - sit),
        )),
        Block::CannonFireDown => Some((
            Vec2::new(lx + inset, hy + sit),
            Vec2::new(hx - inset, hy + sit + f * bs),
        )),
        Block::CannonFireLeft => Some((
            Vec2::new(lx - sit - f * bs, ly + inset),
            Vec2::new(lx - sit, hy - inset),
        )),
        Block::CannonFireRight => Some((
            Vec2::new(hx + sit, ly + inset),
            Vec2::new(hx + sit + f * bs, hy - inset),
        )),
        _ => None,
    }
}


/// Apply the per-tick thruster-wash impulse from each thrusting ship to
/// every other ship and bullet inside that ship's cone.
fn apply_thruster_wash(world: &mut World, dt: f32) {
    // Gather sources to dodge the borrow checker (we can't iterate ships
    // immutably while pushing on ships mutably below).
    let sources: Vec<(EntityId, Vec2, Vec2)> = world
        .ships
        .values()
        .filter(|s| s.thrusting)
        .map(|s| (s.entity_id, s.pos, forward(s.angle) * -1.0))
        .collect();

    for (source_id, apex, axis) in sources {
        // Push other ships.
        for ship in world.ships.values_mut() {
            if ship.entity_id == source_id {
                continue;
            }
            if let Some(force) = physics::cone_force(
                ship.pos,
                apex,
                axis,
                WASH_CONE_LENGTH,
                WASH_CONE_HALF_ANGLE_RAD,
                WASH_FORCE_AT_MOUTH,
            ) {
                ship.vel += force * dt;
            }
        }
        // Push bullets — including bullets from the source ship; the wash
        // reasonably blows your own muzzle bullets sideways too.
        for bullet in world.bullets.values_mut() {
            if let Some(force) = physics::cone_force(
                bullet.pos,
                apex,
                axis,
                WASH_CONE_LENGTH,
                WASH_CONE_HALF_ANGLE_RAD,
                WASH_FORCE_AT_MOUTH,
            ) {
                bullet.vel += force * dt;
            }
        }
    }
}

/// Per-tick particle update: move, age, then check this tick's swept path
/// against walls (kill on contact) and ship triangles (kill on contact and
/// transfer momentum into the ship). Iterating ships in BTreeMap order keeps
/// the result deterministic when one particle could hit two ships.
fn step_particles(world: &mut World, dt: f32) {
    // Snapshot ship triangles up front so we don't hold a borrow on
    // world.ships while iterating world.particles below.
    let ship_tris: Vec<(EntityId, [Vec2; 3])> = world
        .ships
        .iter()
        .map(|(id, s)| (*id, s.world_vertices()))
        .collect();

    let walls = world.map.walls.clone();
    // Accumulate per-ship velocity deltas and apply at the end so iteration
    // order doesn't affect intermediate triangle positions (they were
    // captured above anyway, but applying mid-loop would still be racy if we
    // ever add ship-ship coupling).
    let mut ship_dv: BTreeMap<EntityId, Vec2> = BTreeMap::new();

    let edge_wrap = world.map.edge_wrap;
    let map_w = world.map.width;
    let map_h = world.map.height;
    world.particles.retain_mut(|p| {
        let prev = p.pos;
        p.pos += p.vel * dt;
        p.age += dt;
        if p.age >= p.life {
            return false;
        }
        for w in &walls {
            if physics::segments_intersect(prev, p.pos, w.a, w.b) {
                return false;
            }
        }
        for (id, tri) in &ship_tris {
            if physics::segment_hits_triangle(prev, p.pos, tri) {
                ship_dv
                    .entry(*id)
                    .and_modify(|dv| *dv += p.vel * p.mass)
                    .or_insert_with(|| p.vel * p.mass);
                return false;
            }
        }
        if edge_wrap {
            p.pos = wrap_pos(p.pos, map_w, map_h);
        }
        true
    });

    for (id, dv) in ship_dv {
        if let Some(ship) = world.ships.get_mut(&id) {
            // Divide by ship mass at apply time so the recorded dv reads as
            // pure momentum.
            ship.vel += dv * (1.0 / ship.mass);
        }
    }
}

/// New-bullet intent emitted by `try_fire`. The world assigns the entity id.
#[derive(Clone, Copy, Debug)]
pub struct NewBullet {
    pub shooter: EntityId,
    pub pos: Vec2,
    pub vel: Vec2,
}

/// The worst wall impact a ship took during one `apply_ship_dynamics` call.
/// `vertex_idx` indexes `entities::SHIP_BODY` — 0/1 are the back corners,
/// 2 is the nose. Server checks against the per-vertex kill threshold; the
/// client just ignores it (kills are server-authoritative).
#[derive(Clone, Copy, Debug)]
pub struct WallImpact {
    pub vertex_idx: u8,
    pub impact_speed: f32,
}

/// Step a single ship: input → turn / thrust / integrate / wall collide /
/// cooldown decay (and recoil if firing). Does NOT spawn the bullet — see
/// `try_fire` for that. The split lets the client predict ship motion without
/// having to deal with bullet authority.
pub fn apply_ship_dynamics(ship: &mut Ship, input: &TickInput, map: &Map) -> Option<WallImpact> {
    // Record what we're applying so receivers of snapshots can extrapolate
    // this ship forward identically.
    ship.last_input = *input;
    apply_ship_dynamics_dt(ship, input, map, TICK_DT_SECONDS)
}

/// Same dynamics step as `apply_ship_dynamics` but with arbitrary `dt`. Used
/// by the client to do a sub-tick extrapolation step when rendering remote
/// ships at a fractional point between snapshots. Does NOT update
/// `ship.last_input` — extrapolation is "this is what's still happening," not
/// a new applied input.
///
/// Per-tick sequence:
///   1. Rotate (full angular delta — bounded by SHIP_TURN_RATE × dt; no CCD
///      on rotation, since the per-tick swing is small).
///   2. Apply thrust to velocity.
///   3. Translate via TOI sweep:
///        - if the post-step pose has no overlap → just take the step.
///        - if it overlaps but the pre-step pose did not → binary-search
///          for the first overlap fraction along the motion ray, stop at
///          the last clean fraction, sample the contact normal at the
///          first-overlap fraction, and bounce/slide once.
///        - if even the pre-step pose overlapped (residual / respawn) →
///          fall back to iterative MTV depenetration with per-iter velocity
///          response. This path is the rare cleanup case, not the hot path.
///   4. Wall-crash kill threshold uses the pre-bounce velocity.
///   5. Wrap, then fire / cooldown / charge.
pub fn apply_ship_dynamics_dt(
    ship: &mut Ship,
    input: &TickInput,
    map: &Map,
    dt: f32,
) -> Option<WallImpact> {
    // 1. Rotation — full delta every tick, no sweep. The Elm version did the
    //    same: rotation always commits, residual overlap is repaired by the
    //    MTV pass below. Sweeping rotation makes the ship feel "stuck"
    //    against walls because rotating in directions that would graze a
    //    vertex slows to a crawl.
    let turn_dir = (input.turn_right as i32 - input.turn_left as i32) as f32;
    ship.angle = math::wrap_angle(ship.angle + turn_dir * SHIP_TURN_RATE * dt);

    // 2. Thrust → velocity.
    ship.thrusting = input.thrust;
    if input.thrust {
        ship.vel += forward(ship.angle) * (SHIP_THRUST_ACCEL * dt);
    }
    if SHIP_LINEAR_DAMPING > 0.0 {
        let damp = (1.0 - SHIP_LINEAR_DAMPING * dt).max(0.0);
        ship.vel = ship.vel * damp;
    }

    // 3. Translate via xpilot-ng style swept collision. Process the motion
    //    in up to a couple of "segments" — each one is a sweep to first
    //    contact, then a bounce, then continue sweeping the remainder of
    //    the tick. This way a ship that bounces in the middle of a tick
    //    spends the rest of the tick moving with the new velocity, not
    //    teleporting to the contact point and stopping there.
    let entry_vel = ship.vel;
    let mut impact_normal: Option<Vec2> = None;
    let mut remaining_dt = dt;

    // Up to 3 sweep+bounce passes per tick. Almost always 1; corner-bouncing
    // off two walls in one frame is the only case that needs >1.
    for _ in 0..3 {
        if remaining_dt <= 1e-6 {
            break;
        }
        let from_pos = ship.pos;
        let delta = ship.vel * remaining_dt;
        let target = from_pos + delta;
        let hit = swept_collide_translation(from_pos, delta, ship.angle, map);
        match hit {
            None => {
                // Path is clear — commit the rest of the motion and we're done.
                ship.pos = target;
                break;
            }
            Some(h) => {
                // Stop at first contact, then bounce. Tiny CONTACT_SLOP so
                // we sit just outside the wall (next tick's sweep starts
                // clean instead of registering a 0-depth re-hit).
                let safe_t = (h.t - 1e-4).max(0.0);
                ship.pos = from_pos + delta * safe_t;
                ship.pos += h.normal * CONTACT_SLOP;
                ship.vel = physics::reflect(ship.vel, h.normal, WALL_RESTITUTION);
                if impact_normal.is_none() {
                    impact_normal = Some(h.normal);
                }
                // Spend the remaining fraction of the tick with the new vel.
                remaining_dt *= 1.0 - h.t;
            }
        }
    }

    // Pre-step residual overlap (e.g. respawn placed ship inside a wall, or
    // rotation embedded a vertex). The swept pass above doesn't help here
    // because the motion ray starts INSIDE the wall. Fall back to the MTV
    // depenetration loop.
    if has_wall_overlap_at(ship.pos, ship.angle, map) {
        if let Some(n) = resolve_wall_overlaps(ship, map) {
            if impact_normal.is_none() {
                impact_normal = Some(n);
            }
        }
    }

    let mut impact = None;
    if let Some(normal) = impact_normal {
        let impact_speed = (-entry_vel.dot(normal)).max(0.0);
        let into = normal * -1.0;
        let verts = ship.world_vertices();
        let mut idx = 0u8;
        let mut best = f32::MIN;
        for (i, v) in verts.iter().enumerate() {
            let d = v.dot(into);
            if d > best {
                best = d;
                idx = i as u8;
            }
        }
        impact = Some(WallImpact {
            vertex_idx: idx,
            impact_speed,
        });
    }

    if map.edge_wrap {
        ship.pos = wrap_pos(ship.pos, map.width, map.height);
    }

    // 4. Cooldown decay, charge regen, predicted recoil. Bullet emission
    // itself is the server's job (see `try_fire`), but the client mirrors
    // the cooldown reset, charge deduction, and recoil so prediction stays
    // in lockstep with the server.
    ship.fire_cooldown = (ship.fire_cooldown - dt).max(0.0);
    ship.shot_charge = (ship.shot_charge + SHOT_CHARGE_RATE * dt).min(SHOT_CHARGE_MAX);
    if input.fire
        && ship.fire_cooldown <= 0.0
        && ship.shot_charge >= SHOT_CHARGE_COST
        && !muzzle_blocked(ship, map)
    {
        ship.fire_cooldown = BULLET_COOLDOWN_SECONDS;
        ship.shot_charge -= SHOT_CHARGE_COST;
        let dir = forward(ship.angle);
        ship.vel = ship.vel - dir * (BULLET_SPEED * BULLET_MASS / ship.mass);
    }

    impact
}

/// True when firing would spawn a bullet on the wrong side of a wall.
/// Two checks:
///   1. The pivot→muzzle segment crosses a wall (nose-against-wall case).
///   2. The muzzle position itself sits inside a solid wall cell — catches
///      the case where the shooter is somehow already embedded in walls
///      (no boundary edge between pivot and muzzle to detect via #1).
///      Without this, a stuck bot lobs bullets that fly through wall
///      structures with nothing to collide against.
fn muzzle_blocked(ship: &Ship, map: &Map) -> bool {
    let dir = forward(ship.angle);
    let muzzle = ship.pos + dir * (SHIP_NOSE_OFFSET + 2.0);
    for w in &map.walls {
        if physics::segments_intersect(ship.pos, muzzle, w.a, w.b) {
            return true;
        }
    }
    if let Some(grid) = map.blocks.as_ref() {
        let bs = grid.block_size;
        let bx = (muzzle.x / bs).floor() as i64;
        let by = (muzzle.y / bs).floor() as i64;
        let (lookup_x, lookup_y) = if map.edge_wrap {
            (
                bx.rem_euclid(grid.width as i64),
                by.rem_euclid(grid.height as i64),
            )
        } else {
            (bx, by)
        };
        let cell = grid.get(lookup_x, lookup_y);
        if pos_in_solid_part(muzzle, cell, bx, by, bs) {
            return true;
        }
    }
    false
}

/// Returns the new-bullet intent if this ship just fired this tick. Caller
/// (the world) assigns the entity id and inserts.
///
/// MUST be called AFTER `apply_ship_dynamics` for the same tick — that fn
/// resets the cooldown when fire is held, so we'd never see `cooldown == max`
/// here if we re-checked. The convention: the cooldown that's `≈ max` directly
/// after `apply_ship_dynamics` is the signal that this tick is the firing tick.
pub fn try_fire(ship: &Ship, input: &TickInput, map: &Map) -> Option<NewBullet> {
    // We detect "this is the firing tick" by checking that fire is held AND
    // the cooldown is at (or just below) max — apply_ship_dynamics set it
    // exactly to BULLET_COOLDOWN_SECONDS this tick if we fired.
    if !input.fire {
        return None;
    }
    // Sentinel: float-compare against the max with a tiny epsilon.
    if (ship.fire_cooldown - BULLET_COOLDOWN_SECONDS).abs() > 1e-5 {
        return None;
    }
    // Defensive: cooldown gate in `apply_ship_dynamics` already blocks
    // muzzle-in-wall shots so this rarely matches, but covers callers
    // (predicted bullets) that might construct intermediate ship state where
    // the cooldown looks ready but the wall is in the way.
    if muzzle_blocked(ship, map) {
        return None;
    }
    let dir = forward(ship.angle);
    let muzzle = ship.pos + dir * (SHIP_NOSE_OFFSET + 2.0);
    // Bullet velocity = ship vel + forward * speed. Note: ship.vel here
    // already includes the recoil from `apply_ship_dynamics`. That's fine —
    // we want the bullet's frame of reference to be the post-recoil ship.
    let bvel = ship.vel + dir * BULLET_SPEED;
    Some(NewBullet { shooter: ship.entity_id, pos: muzzle, vel: bvel })
}

/// Step a bullet by one fixed tick. Returns `true` if the bullet should be
/// despawned (lifetime expired or hit a wall). Wall test uses the unwrapped
/// trajectory so we don't false-positive against walls "in the middle" of
/// the swept path after a wrap.
///
/// **Does not wrap** — caller wraps after the bullet-vs-ship hit test runs,
/// otherwise the swept hit segment from the pre-step pos to a wrapped pos
/// would span the entire map and turn every wrap-frame into an instakill
/// laser.
pub fn step_bullet(bullet: &mut Bullet, map: &Map) -> bool {
    let dt = TICK_DT_SECONDS;
    let prev = bullet.pos;
    bullet.pos += bullet.vel * dt;
    bullet.age_seconds += dt;
    if bullet.age_seconds >= BULLET_LIFETIME_SECONDS {
        return true;
    }
    for wall in &map.walls {
        if physics::segments_intersect(prev, bullet.pos, wall.a, wall.b) {
            return true;
        }
    }
    // Defensive: bullet ended this tick inside a solid wall cell. Catches
    // (a) torus wraps that landed inside a wall, (b) muzzle spawns that
    // slipped past muzzle_blocked, (c) parallel/numerical edge cases where
    // segments_intersect missed a grazing crossing.
    if let Some(grid) = map.blocks.as_ref() {
        let bs = grid.block_size;
        let bx = (bullet.pos.x / bs).floor() as i64;
        let by = (bullet.pos.y / bs).floor() as i64;
        let (lookup_x, lookup_y) = if map.edge_wrap {
            (
                bx.rem_euclid(grid.width as i64),
                by.rem_euclid(grid.height as i64),
            )
        } else {
            (bx, by)
        };
        let cell = grid.get(lookup_x, lookup_y);
        if pos_in_solid_part(bullet.pos, cell, bx, by, bs) {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::map::{arena_map, BlockGrid, SpawnPoint, WallSegment};

    /// Build a tiny grid map from char rows. `x` = wall, `.` = space,
    /// `s/a/w/q` = the four triangle slope variants, `R/C/D/F` = active
    /// cannons (firing up/down/left/right). Block size 35 to match the .xp
    /// format.
    fn grid_map(rows: &[&str], edge_wrap: bool) -> Map {
        let height = rows.len() as u32;
        let width = rows[0].len() as u32;
        let bs = 35.0;
        let mut grid = BlockGrid::empty(width, height, bs);
        for (y, row) in rows.iter().enumerate() {
            for (x, ch) in row.chars().enumerate() {
                let b = match ch {
                    'x' => Block::Wall,
                    's' => Block::TriUL,
                    'a' => Block::TriUR,
                    'w' => Block::TriLL,
                    'q' => Block::TriLR,
                    'R' => Block::CannonFireUp,
                    'C' => Block::CannonFireDown,
                    'D' => Block::CannonFireLeft,
                    'F' => Block::CannonFireRight,
                    _ => Block::Space,
                };
                grid.set(x as u32, y as u32, b);
            }
        }
        let mut m = Map {
            name: "test".into(),
            width: width as f32 * bs,
            height: height as f32 * bs,
            edge_wrap,
            blocks: Some(grid),
            spawns: vec![SpawnPoint { pos: Vec2::ZERO, angle: 0.0 }],
            walls: Vec::new(),
        };
        m.rebuild_walls();
        m
    }

    fn empty_inputs() -> BTreeMap<EntityId, TickInput> {
        BTreeMap::new()
    }

    #[test]
    fn ship_drifts_when_no_input() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(50.0, 0.0);
        let p0 = w.ships[&id].pos;
        for _ in 0..60 {
            w.step(&empty_inputs());
        }
        let p1 = w.ships[&id].pos;
        assert!((p1.x - p0.x - 50.0).abs() < 1.0, "drifted {}", p1.x - p0.x);
        assert!((p1.y - p0.y).abs() < 0.01);
    }

    #[test]
    fn thrust_adds_velocity_in_forward_direction() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { thrust: true, ..Default::default() });
        for _ in 0..30 {
            w.step(&inputs);
        }
        let v = w.ships[&id].vel;
        assert!(v.y < -50.0, "expected negative-Y vel, got {:?}", v);
        assert!(v.x.abs() < 0.01, "no x velocity expected, got {}", v.x);
    }

    #[test]
    fn ship_bounces_off_top_wall() {
        // Initial vel kept under SHIP_NOSE_KILL_SPEED so the bounce is
        // observable instead of the ship blowing up on impact.
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 200.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(0.0, -200.0);
        for _ in 0..120 {
            w.step(&empty_inputs());
        }
        let v = w.ships[&id].vel;
        assert!(v.y > 0.0, "vel.y should reverse, got {:?}", v);
        assert!(v.y < 200.0, "vel.y should be smaller than initial");
    }

    #[test]
    fn rotating_into_wall_pushes_ship_out() {
        let mut w = World::new(arena_map());
        w.map.walls.clear();
        w.map.walls.push(WallSegment {
            a: Vec2::new(0.0, 594.0),
            b: Vec2::new(1800.0, 594.0),
        });
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { turn_right: true, ..Default::default() });
        let y0 = w.ships[&id].pos.y;
        for _ in 0..30 {
            w.step(&inputs);
        }
        let y1 = w.ships[&id].pos.y;
        assert!(y1 > y0, "ship should be pushed downward by rotation; y0={} y1={}", y0, y1);
    }

    #[test]
    fn fire_creates_bullet_with_cooldown_and_mass() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { fire: true, ..Default::default() });
        w.step(&inputs);
        assert_eq!(w.bullets.len(), 1);
        let b = w.bullets.values().next().unwrap();
        assert!(b.mass > 0.0);
        assert_eq!(b.shooter, id);
        // Cooldown blocks immediate refire.
        w.step(&inputs);
        assert_eq!(w.bullets.len(), 1);
    }

    #[test]
    fn bullet_dies_after_lifetime() {
        let mut w = World::new(arena_map());
        let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.0);
        let mut inputs = empty_inputs();
        inputs.insert(id, TickInput { fire: true, ..Default::default() });
        w.step(&inputs);
        assert_eq!(w.bullets.len(), 1);
        let extra_ticks = (BULLET_LIFETIME_SECONDS / TICK_DT_SECONDS) as u32 + 60;
        for _ in 0..extra_ticks {
            w.step(&empty_inputs());
        }
        assert_eq!(w.bullets.len(), 0);
    }

    #[test]
    fn bullet_kills_target_at_close_range() {
        // Shooter facing up (angle 0 → forward = (0, -1)). Target 100 units
        // above (smaller y). Bullet at 220 u/s reaches in ~30 ticks.
        let mut w = World::new(arena_map());
        let shooter = w.spawn_ship(1, Vec2::new(900.0, 700.0), 0.0);
        let target = w.spawn_ship(2, Vec2::new(900.0, 600.0), 0.0);

        let mut inputs = BTreeMap::new();
        inputs.insert(shooter, TickInput { fire: true, ..Default::default() });

        let mut killed = false;
        for _ in 0..240 {
            let events = w.step(&inputs);
            for ev in &events {
                if let crate::protocol::GameEvent::ShipDied { entity_id, .. } = ev {
                    if *entity_id == target {
                        killed = true;
                    }
                }
            }
            if killed {
                break;
            }
        }
        assert!(killed, "target should have died");
    }

    #[test]
    fn deterministic_replay() {
        let make = || {
            let mut w = World::new(arena_map());
            let id = w.spawn_ship(1, Vec2::new(900.0, 600.0), 0.5);
            (w, id)
        };
        let (mut a, ida) = make();
        let (mut b, idb) = make();
        for tick in 0..240 {
            let mut inputs_a = empty_inputs();
            let mut inputs_b = empty_inputs();
            let i = TickInput {
                thrust: tick % 7 != 0,
                turn_left: tick % 13 == 0,
                turn_right: tick % 11 == 0,
                fire: tick % 5 == 0,
                ..Default::default()
            };
            inputs_a.insert(ida, i);
            inputs_b.insert(idb, i);
            a.step(&inputs_a);
            b.step(&inputs_b);
        }
        assert_eq!(a.ships[&ida].pos, b.ships[&idb].pos);
        assert_eq!(a.ships[&ida].vel, b.ships[&idb].vel);
        assert_eq!(a.ships[&ida].angle, b.ships[&idb].angle);
        assert_eq!(a.bullets.len(), b.bullets.len());
    }

    #[test]
    fn client_prediction_matches_server_replay() {
        // Property: stepping a ship via apply_ship_dynamics N times produces
        // the same ship state as stepping a single-ship World N times. This
        // is what makes M3 reconciliation work — re-applying unacked inputs
        // onto a snapshotted ship gives the same answer the server got.
        let map = arena_map();
        let mut server = World::new(map.clone());
        let id = server.spawn_ship(1, Vec2::new(900.0, 600.0), 0.7);

        let mut client_ship = server.ships[&id].clone();

        let inputs: Vec<TickInput> = (0..120)
            .map(|t| TickInput {
                client_tick: t,
                thrust: t % 3 != 0,
                turn_left: t % 11 == 0,
                turn_right: t % 7 == 0,
                fire: false,
                ..Default::default()
            })
            .collect();

        for input in &inputs {
            let mut server_inputs = BTreeMap::new();
            server_inputs.insert(id, *input);
            server.step(&server_inputs);
            apply_ship_dynamics(&mut client_ship, input, &map);
        }

        assert_eq!(client_ship.pos, server.ships[&id].pos);
        assert_eq!(client_ship.vel, server.ships[&id].vel);
        assert_eq!(client_ship.angle, server.ships[&id].angle);
    }

    /// Ship spawned inside a Wall cell: SAT depenetration must push it
    /// completely outside on the very first step. Regression for the
    /// "embedded forever" path that the old unstick handled by teleport.
    #[test]
    fn ship_inside_wall_cell_pushed_out() {
        // 3×3 grid, only center cell solid (35..70 × 35..70).
        let map = grid_map(&[
            "...",
            ".x.",
            "...",
        ], false);
        let mut w = World::new(map);
        let id = w.spawn_ship(1, Vec2::new(52.0, 52.0), 0.0);
        // Bypass the wall-crash death path — set vel to 0 so the impact
        // speed against the wall is well under the kill threshold even when
        // the depenetration push is large.
        w.ships.get_mut(&id).unwrap().vel = Vec2::ZERO;
        w.step(&BTreeMap::new());
        // After one tick the ship must not be inside the wall block.
        assert!(w.ships.contains_key(&id), "ship died on the unstick");
        let p = w.ships[&id].pos;
        let in_cell = p.x >= 35.0 && p.x <= 70.0 && p.y >= 35.0 && p.y <= 70.0;
        assert!(!in_cell, "ship pos {:?} still inside wall cell", p);
    }

    /// Ship drifts gently past a single protruding wall block. The user's
    /// "stuck on a protruding corner" complaint — verify per-tick motion
    /// stays small (no warps) and the ship eventually clears the wall.
    #[test]
    fn slow_drift_past_protruding_corner_doesnt_warp() {
        let map = grid_map(&[
            "...",
            ".x.",
            "...",
        ], false);
        let mut w = World::new(map);
        // Start on the right of the wall (x=110), drifting -X at 12 u/s
        // (well under any threshold). y placed so the ship grazes the
        // bottom-right corner at (70, 70).
        let id = w.spawn_ship(1, Vec2::new(110.0, 75.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(-12.0, 0.0);
        let mut prev = w.ships[&id].pos;
        for _ in 0..600 {
            w.step(&BTreeMap::new());
            let p = w.ships[&id].pos;
            // Per-tick step should stay under a tiny bound — no warps.
            // 12 u/s × 1/60 = 0.2 units of "natural" motion. Allow some
            // slack for the bounce reflection and depenetration push.
            let dx = p.x - prev.x;
            let dy = p.y - prev.y;
            let d2 = dx * dx + dy * dy;
            assert!(d2 < 25.0, "ship warped from {:?} to {:?}", prev, p);
            prev = p;
        }
    }

    /// Ship sits in a 3-sided pocket (walls left/right/bottom, open top).
    /// Without thrust it must NOT get pushed around or "stick" — pos and vel
    /// must stay essentially still after many idle ticks. Regression for the
    /// old per-tick reflection cycling against multiple walls simultaneously.
    #[test]
    fn ship_in_three_sided_pocket_stays_still_when_idle() {
        // 3×3 with walls on left, right, and bottom of the center column.
        let map = grid_map(&[
            "x.x",
            "x.x",
            "xxx",
        ], false);
        let mut w = World::new(map);
        // Pocket interior is x ∈ (35, 70), y ∈ (0, 70). Place ship centered.
        let id = w.spawn_ship(1, Vec2::new(52.0, 35.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::ZERO;
        let p0 = w.ships[&id].pos;
        for _ in 0..120 {
            w.step(&BTreeMap::new());
        }
        let p1 = w.ships[&id].pos;
        let dx = (p1.x - p0.x).abs();
        let dy = (p1.y - p0.y).abs();
        assert!(dx < 0.5, "drifted in x: {} → {}", p0.x, p1.x);
        assert!(dy < 0.5, "drifted in y: {} → {}", p0.y, p1.y);
    }

    /// Rotating against a flat wall must not "fling" the ship. The old
    /// CCD-on-rotation path could displace the ship by several units in one
    /// tick when rotation pushed a vertex deeper. With per-tick depenetration
    /// the swing of any vertex is bounded by SHIP_TURN_RATE × dt × radius
    /// ≈ 1.5 units, so push-out per tick stays small.
    #[test]
    fn rotation_against_wall_doesnt_fling() {
        let map = grid_map(&[
            "...",
            "...",
            "xxx",
        ], false);
        let mut w = World::new(map);
        // Wall row spans y ∈ [70, 105]. Ship sits just above touching at y≈64
        // (back vertices at y=64+6=70). Slow rotation; pos should drift up
        // (-Y) but only a tiny amount per tick.
        let id = w.spawn_ship(1, Vec2::new(52.0, 64.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::ZERO;
        let mut inputs = BTreeMap::new();
        inputs.insert(id, TickInput { turn_right: true, ..Default::default() });
        let mut prev = w.ships[&id].pos;
        for _ in 0..30 {
            w.step(&inputs);
            let p = w.ships[&id].pos;
            let dx = p.x - prev.x;
            let dy = p.y - prev.y;
            assert!(
                dx * dx + dy * dy < 9.0,
                "rotation flung ship from {:?} to {:?}",
                prev,
                p
            );
            prev = p;
        }
    }

    /// Convex-corner bounce eventually moves the ship away from the corner.
    /// With cell-axis-only MTV, a diagonal approach to a corner reflects one
    /// axis at a time (axis-aligned response, like the Elm version), so the
    /// ship may take a few ticks to fully escape — but it MUST end up further
    /// away after a settling period.
    #[test]
    fn convex_corner_bounce_eventually_moves_away() {
        let map = grid_map(&[
            "...",
            ".x.",
            "...",
        ], false);
        let mut w = World::new(map);
        let id = w.spawn_ship(1, Vec2::new(95.0, 95.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(-25.0, -25.0);
        let corner = Vec2::new(70.0, 70.0);

        let initial_vel = w.ships[&id].vel;
        let mut bounced = false;
        for _ in 0..400 {
            w.step(&BTreeMap::new());
            let v = w.ships[&id].vel;
            if v.x.signum() != initial_vel.x.signum() || v.y.signum() != initial_vel.y.signum() {
                bounced = true;
                break;
            }
        }
        assert!(bounced, "ship never bounced");

        let dist_at_bounce = (w.ships[&id].pos - corner).length();
        for _ in 0..120 {
            w.step(&BTreeMap::new());
        }
        let dist_final = (w.ships[&id].pos - corner).length();
        assert!(
            dist_final > dist_at_bounce + 10.0,
            "ship didn't move away from corner: {} → {}",
            dist_at_bounce,
            dist_final
        );
    }

    /// Slow drift into a corner produces a stable post-contact velocity —
    /// no per-tick velocity oscillation (the corner-chatter the
    /// MIN_BOUNCE_SPEED gating fixed). After contact, |dv/tick| stays small.
    #[test]
    fn slow_corner_contact_no_velocity_chatter() {
        let map = grid_map(&[
            "...",
            ".x.",
            "...",
        ], false);
        let mut w = World::new(map);
        // Very slow diagonal drift toward the BR corner.
        let id = w.spawn_ship(1, Vec2::new(85.0, 85.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(-8.0, -8.0);
        let mut prev_vel = w.ships[&id].vel;
        let mut max_dv: f32 = 0.0;
        for _ in 0..400 {
            w.step(&BTreeMap::new());
            let v = w.ships[&id].vel;
            let dv = ((v.x - prev_vel.x).powi(2) + (v.y - prev_vel.y).powi(2)).sqrt();
            if dv > max_dv {
                max_dv = dv;
            }
            prev_vel = v;
        }
        // Slow contact below MIN_BOUNCE_SPEED ⇒ slide. Per-tick vel jump
        // capped by the inward component being zeroed (≤ initial speed).
        // No micro-bounce should produce a vel jump near 2× initial speed.
        assert!(
            max_dv < 12.0,
            "max single-tick velocity jump {} suggests chatter",
            max_dv
        );
    }

    /// Rotation alone, with ship pinned against a wall corner, must not warp
    /// the ship. Regression for the "rotate while broadside-pinned and the
    /// ship pops away" case — was caused by rotation embedding a vertex,
    /// then MTV picking a ship-edge-normal axis and shoving the ship sideways.
    /// Fixed by (a) rotation TOI sweep capping the angular delta when it
    /// would embed and (b) MTV restricted to cell-edge axes only.
    #[test]
    fn rotation_while_pinned_at_corner_doesnt_warp() {
        let map = grid_map(&[
            "...",
            ".x.",
            "...",
        ], false);
        let mut w = World::new(map);
        // Drift broadside into corner first.
        let id = w.spawn_ship(1, Vec2::new(40.0, 95.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(15.0, 0.0);
        for _ in 0..200 {
            w.step(&BTreeMap::new());
            if w.ships[&id].vel.x.abs() < 1.0 {
                break;
            }
        }
        // Now rotate in place. Track per-tick motion — should never exceed
        // a couple units (rotation can swing a vertex by ~1.5 u/tick at
        // SHIP_TURN_RATE × dt; anything over 5 u is a real warp).
        let mut inputs = BTreeMap::new();
        inputs.insert(id, TickInput { turn_right: true, ..Default::default() });
        let mut prev = w.ships[&id].pos;
        for tick in 0..120 {
            w.step(&inputs);
            let p = w.ships[&id].pos;
            let dx = (p.x - prev.x).abs();
            let dy = (p.y - prev.y).abs();
            assert!(
                dx <= 5.0 && dy <= 5.0,
                "tick {tick} warped during rotation: prev {prev:?} → {p:?}",
            );
            prev = p;
        }
    }

    /// Ship sliding tangent to a flat wall, with one vertex grazing the
    /// wall's corner. The corner is what gets contacted (not the wall edge),
    /// which is the case that polygon-MTV bungles. Swept collision should:
    ///   - find the contact at the corner (vertex-vs-edge sweep)
    ///   - reflect ONLY the inward component (perpendicular to ship edge)
    ///   - leave the tangent component intact so the ship continues sliding
    /// Verifies the ship doesn't stop and doesn't reverse direction along
    /// its sliding axis.
    #[test]
    fn vertex_grazes_corner_keeps_tangent_velocity() {
        // 7 cols × 5 rows = world 245 × 175. Wall block at (3,2) → world
        // (105..140, 70..105).
        let map = grid_map(&[
            ".......",
            ".......",
            "...x...",
            ".......",
            ".......",
        ], false);
        let mut w = World::new(map);
        // Ship sliding -X across the top of the wall (cell top at y=70).
        // Pivot.y = 64 so back vertices at y=70 graze the wall's top edge.
        // Spawn well to the right of the wall so the contact happens during
        // the drift, not at frame 0.
        let id = w.spawn_ship(1, Vec2::new(190.0, 64.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(-50.0, 0.0);
        let initial_vx = w.ships[&id].vel.x;
        // Slide for many ticks; tangent (vel.x) should not reverse sign.
        for tick in 0..200 {
            w.step(&BTreeMap::new());
            let v = w.ships[&id].vel;
            assert!(
                v.x.signum() == initial_vx.signum() || v.x.abs() < 1.0,
                "tick {tick}: tangent vel reversed: {:?}",
                v
            );
        }
    }

    /// Slow broadside drift into the OUTSIDE corner of a square wall block.
    /// With TOI sweep, the ship should advance at most one natural step per
    /// tick — never get popped multiple ship-widths backwards because the
    /// MTV escape vector turned out to be larger than the original motion.
    /// This is the test that fails for post-step MTV depenetration and
    /// passes for proper time-of-impact resolution.
    #[test]
    fn slow_broadside_corner_contact_max_position_correction() {
        // 5×5 grid, single wall block at (2,2) → world bounds 70..105 × 70..105.
        let map = grid_map(&[
            ".....",
            ".....",
            "..x..",
            ".....",
            ".....",
        ], false);
        let mut w = World::new(map);
        // Ship facing -Y (angle 0); back edge of triangle is the broadside.
        // Drift +X very slowly so the back-right vertex grazes the wall's
        // left edge (x=70) at the bottom corner.
        let id = w.spawn_ship(1, Vec2::new(40.0, 95.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(15.0, 0.0);
        let natural_step = 15.0 / 60.0; // units/tick
        // Allow generous slack for slop + tiny float wiggle, but nothing close
        // to "half a ship length" (~10 units).
        let max_per_tick = natural_step + 0.5;
        let mut prev = w.ships[&id].pos;
        for tick in 0..600 {
            w.step(&BTreeMap::new());
            let p = w.ships[&id].pos;
            let dx = (p.x - prev.x).abs();
            let dy = (p.y - prev.y).abs();
            assert!(
                dx <= max_per_tick && dy <= max_per_tick,
                "tick {tick} warped: prev {prev:?} → {p:?} (|dx|={dx} |dy|={dy} max={max_per_tick})",
            );
            prev = p;
        }
    }

    /// Convex tip of a slope tile (TriUL → outward TR vertex at (cell.hx,
    /// cell.ly) — hypotenuse meets top edge). Drifting tangent to the
    /// hypotenuse should slide past the tip without warping or sticking.
    #[test]
    fn slope_tip_glancing_pass_doesnt_warp() {
        // TriUL at cell (1,1): solid TL=(35,35), TR=(70,35), BL=(35,70).
        // Hypotenuse TR=(70,35) ↔ BL=(35,70). The TR vertex is a convex tip
        // sticking up-and-right into open space.
        let map = grid_map(&[
            "...",
            ".s.",
            "...",
        ], false);
        let mut w = World::new(map);
        // Drift LEFT (-X) through the open band just above the slope tip
        // (y just above 35, x crossing 70). Should pass without warping.
        let id = w.spawn_ship(1, Vec2::new(110.0, 25.0), 0.0);
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(-30.0, 0.0);
        let mut prev = w.ships[&id].pos;
        for _ in 0..240 {
            w.step(&BTreeMap::new());
            let p = w.ships[&id].pos;
            let d = ((p.x - prev.x).powi(2) + (p.y - prev.y).powi(2)).sqrt();
            assert!(d < 5.0, "warped from {:?} to {:?}", prev, p);
            prev = p;
        }
    }

    /// Slope tile (TriUL: filled top-left wedge): ship drifting into the
    /// solid wedge gets pushed out along the hypotenuse normal (toward the
    /// open BR), not warped. Regression for the diagonal jump-spots bug.
    #[test]
    fn slope_tile_pushes_along_hypotenuse_normal() {
        // Single TriUL block at (0,0): solid corners TL=(0,0), TR=(35,0),
        // BL=(0,35); empty BR. Ship drifting -X-Y (toward TL) at slow speed.
        let map = grid_map(&[
            "s..",
            "...",
            "...",
        ], false);
        let mut w = World::new(map);
        let id = w.spawn_ship(1, Vec2::new(50.0, 50.0), 0.0);
        // Drift slowly into the slope.
        w.ships.get_mut(&id).unwrap().vel = Vec2::new(-15.0, -15.0);
        for _ in 0..120 {
            w.step(&BTreeMap::new());
            // Ship should never be alive AND deep inside the solid wedge.
            if let Some(s) = w.ships.get(&id) {
                // Solid wedge: x+y < 35 (in cell-local coords; cell starts at 0,0).
                let p = s.pos;
                if p.x >= 0.0 && p.x <= 35.0 && p.y >= 0.0 && p.y <= 35.0 {
                    assert!(
                        p.x + p.y > 25.0,
                        "ship pivot {:?} embedded deep in slope wedge",
                        p
                    );
                }
            }
        }
    }

    #[test]
    fn cannon_fires_at_a_ship_in_range() {
        // Cannons no longer fire without a target. Place one stationary
        // ship directly above the CannonFireUp so the lead-aim solver
        // returns a valid intercept and the cannon fires.
        let m = grid_map(&[
            "       ",
            "       ",
            "       ",
            "   R   ",
            "       ",
            "       ",
        ], false);
        let mut w = World::new(m);
        assert_eq!(w.cannons.len(), 1);
        let cannon_id = w.cannons.values().next().unwrap().entity_id;
        for c in w.cannons.values_mut() {
            c.fire_at_tick = 0;
        }
        // Ship 100 units directly above the cannon, stationary.
        let bs = 35.0_f32;
        let cx = (3.0 + 0.5) * bs;
        let cy = (3.0 + 0.5) * bs;
        w.spawn_ship(7, Vec2::new(cx, cy - 100.0), 0.0);
        w.step(&empty_inputs());
        let b = w
            .bullets
            .values()
            .find(|b| b.shooter == cannon_id)
            .expect("cannon should have fired at the ship");
        assert!(b.vel.y < 0.0, "cannon-up bullet should head up, vel={:?}", b.vel);
        let speed = (b.vel.x * b.vel.x + b.vel.y * b.vel.y).sqrt();
        assert!(
            speed >= CANNON_BULLET_SPEED_MIN - 1.0
                && speed <= CANNON_BULLET_SPEED_MAX + 1.0,
            "speed {} not in [{}, {}]",
            speed,
            CANNON_BULLET_SPEED_MIN,
            CANNON_BULLET_SPEED_MAX
        );
    }

    #[test]
    fn cannon_holds_fire_when_no_one_in_range() {
        // Same map as above but with no ships. Cannon should NOT fire.
        let m = grid_map(&[
            "       ",
            "       ",
            "       ",
            "   R   ",
            "       ",
            "       ",
        ], false);
        let mut w = World::new(m);
        for c in w.cannons.values_mut() {
            c.fire_at_tick = 0;
        }
        for _ in 0..30 {
            w.step(&empty_inputs());
        }
        assert!(
            w.bullets.is_empty(),
            "no targets → cannons should hold fire, got {} bullets",
            w.bullets.len()
        );
    }

    #[test]
    fn cannon_aims_at_ship_when_one_is_in_the_fan() {
        // CannonFireUp in middle of an open arena with one ship up + to the
        // right (well inside the ±60° fan). Force fire on tick 0 and verify
        // the bullet velocity is angled toward the ship rather than the
        // straight-up nominal direction.
        let m = grid_map(&[
            "       ",
            "       ",
            "       ",
            "   R   ",
            "       ",
            "       ",
        ], false);
        let mut w = World::new(m);
        let bs = 35.0_f32;
        let cannon_cx = (3.0 + 0.5) * bs;
        let cannon_cy = (3.0 + 0.5) * bs;
        // Ship 60 units to the right and 200 above the cannon — angle to it
        // is atan2(60, 200) ≈ 0.29 rad (~17°), well inside the 60° fan.
        let ship_x = cannon_cx + 60.0;
        let ship_y = cannon_cy - 200.0;
        w.spawn_ship(7, Vec2::new(ship_x, ship_y), 0.0);
        for c in w.cannons.values_mut() {
            c.fire_at_tick = 0;
        }
        let cannon_id = w.cannons.values().next().unwrap().entity_id;
        w.step(&empty_inputs());
        let b = w
            .bullets
            .values()
            .find(|b| b.shooter == cannon_id)
            .expect("cannon should have fired");
        // Bullet should head up AND to the right toward the ship.
        assert!(b.vel.x > 0.0, "should aim right toward ship, vel={:?}", b.vel);
        assert!(b.vel.y < 0.0, "should still go up");
        // Bullet angle should be near 0.29 rad ± noise (15°). Bound generously
        // to allow the noise to land anywhere in [0.29 - 0.26, 0.29 + 0.26].
        let bullet_angle = math::atan2(b.vel.x, -b.vel.y);
        assert!(
            (bullet_angle - 0.29).abs() < 0.35,
            "bullet angle {} not near aim (~0.29)",
            bullet_angle
        );
    }

    #[test]
    fn ship_running_into_cannon_kills_both() {
        // CannonFireRight in the middle of an open arena. Ship sits inside
        // the cannon's right-side triangle bbox at start. One step → both
        // dead.
        let m = grid_map(&[
            "       ",
            "       ",
            "   F   ",
            "       ",
            "       ",
        ], true);
        let mut w = World::new(m);
        let cell = (3u32, 2u32);
        assert!(w.cannons.get(&cell).unwrap().alive);
        // Place ship in the cannon's firing triangle bbox: just to the right
        // of the wall block, vertical center of the cell.
        let bs = 35.0_f32;
        let ship_x = (cell.0 as f32 + 1.0) * bs + bs * 0.1; // inside the bbox
        let ship_y = (cell.1 as f32 + 0.5) * bs;
        let sid = w.spawn_ship(7, Vec2::new(ship_x, ship_y), 0.0);
        let events = w.step(&empty_inputs());
        assert!(!w.ships.contains_key(&sid), "ship should be dead");
        assert!(!w.cannons.get(&cell).unwrap().alive, "cannon should be dead");
        // ShipDied event with no killer (cannon doesn't credit a player).
        assert!(events.iter().any(|e| matches!(
            e,
            GameEvent::ShipDied { entity_id, killer: None, .. } if *entity_id == sid
        )));
    }

    #[test]
    fn cannon_dies_to_a_bullet_and_respawns() {
        // CannonFireDown next to open space. Spawn a bullet with full damage
        // hitting the cannon's hit-bbox. Verify alive flips, then run for the
        // respawn duration and verify it comes back.
        let m = grid_map(&[
            "   ",
            " C ",
            "   ",
        ], true);
        let mut w = World::new(m);
        let cell = (1u32, 1u32);
        assert!(w.cannons.get(&cell).unwrap().alive);

        // Bullet sitting in the cannon's downward triangle bbox. Use a
        // shooter id that's not a cannon so the cannon-shooter filter
        // doesn't exclude it.
        let bid = w.alloc_id();
        let bs = 35.0_f32;
        let cx = 1.5 * bs;
        let cy = 2.0 * bs + 0.2 * bs; // inside the bbox just below the block
        w.bullets.insert(
            bid,
            Bullet {
                entity_id: bid,
                shooter: 999_999, // not a cannon, not a ship
                pos: Vec2::new(cx, cy),
                vel: Vec2::ZERO,
                mass: BULLET_MASS,
                age_seconds: 0.0,
            },
        );
        w.step(&empty_inputs());
        assert!(!w.cannons.get(&cell).unwrap().alive, "cannon should be dead");
        assert!(!w.bullets.contains_key(&bid), "bullet should be consumed");

        let respawn_ticks = (CANNON_RESPAWN_SECONDS / TICK_DT_SECONDS) as u32;
        for _ in 0..(respawn_ticks + 1) {
            w.step(&empty_inputs());
        }
        assert!(
            w.cannons.get(&cell).unwrap().alive,
            "cannon should have respawned after {}s",
            CANNON_RESPAWN_SECONDS
        );
    }
}
