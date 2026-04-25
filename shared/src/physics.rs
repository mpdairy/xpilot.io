// Collision helpers and integration primitives. The actual sim step lives in
// `world.rs` so it can orchestrate spawn / despawn across multiple entity
// kinds in a deterministic order.

use crate::math::{self, Vec2};

/// Convex polygon vs line segment overlap (SAT-style).
///
/// Returns `(push_normal, depth)` such that translating the polygon by
/// `push_normal * depth` separates it from the segment along the segment's
/// perpendicular direction. The push always points away from the wall (toward
/// the polygon's centroid). `None` if no overlap.
///
/// We don't use the full SAT MTV because we want the response to always be
/// perpendicular to the wall — that gives clean sliding when the ship grazes a
/// wall. Detection still uses the necessary SAT axes (segment perp + segment
/// dir) so we don't false-positive when the polygon is past either segment end.
pub fn polygon_segment_collide(verts: &[Vec2], a: Vec2, b: Vec2) -> Option<(Vec2, f32)> {
    if verts.len() < 3 {
        return None;
    }

    let wall_dir_raw = b - a;
    let wall_len_sq = wall_dir_raw.length_squared();
    if wall_len_sq < 1e-12 {
        return None;
    }
    let wall_len = math::sqrt(wall_len_sq);
    let wall_dir = wall_dir_raw * (1.0 / wall_len);
    let wall_perp = Vec2::new(-wall_dir.y, wall_dir.x);

    // 1. Polygon must straddle the wall plane.
    let wall_perp_proj = a.dot(wall_perp);
    let (pmin_p, pmax_p) = project_poly(verts, wall_perp);
    if pmax_p <= wall_perp_proj || pmin_p >= wall_perp_proj {
        return None;
    }

    // 2. Polygon must overlap the segment along the wall direction (not past
    //    either endpoint).
    let (pmin_d, pmax_d) = project_poly(verts, wall_dir);
    let s_a_d = a.dot(wall_dir);
    let s_b_d = b.dot(wall_dir);
    let smin_d = s_a_d.min(s_b_d);
    let smax_d = s_a_d.max(s_b_d);
    if pmax_d <= smin_d || pmin_d >= smax_d {
        return None;
    }

    // Push direction: away from wall, toward the polygon's centroid.
    let mut centroid = Vec2::ZERO;
    for v in verts {
        centroid += *v;
    }
    centroid = centroid * (1.0 / verts.len() as f32);
    let centroid_proj = centroid.dot(wall_perp);
    if centroid_proj >= wall_perp_proj {
        Some((wall_perp, wall_perp_proj - pmin_p))
    } else {
        Some((wall_perp * -1.0, pmax_p - wall_perp_proj))
    }
}

fn project_poly(verts: &[Vec2], axis: Vec2) -> (f32, f32) {
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

/// Convex polygon vs convex polygon SAT collision. Returns the minimum
/// translation vector `(axis, depth)` that, applied to `a` as
/// `a.pos += axis * depth`, separates `a` from `b`. `axis` always points
/// roughly from b's centroid toward a's centroid. `None` if no overlap.
///
/// Unlike `polygon_segment_collide` (which only tests the wall's perpendicular
/// because we want clean sliding against an infinitely thin wall), this picks
/// the actual minimum-overlap axis across all edges of both polygons. That's
/// the right thing for ship-vs-tile because the tile is a real 2D shape with
/// corners — the "exit" direction depends on which face you're closest to.
pub fn polygon_polygon_collide(a: &[Vec2], b: &[Vec2]) -> Option<(Vec2, f32)> {
    if a.len() < 3 || b.len() < 3 {
        return None;
    }
    let mut min_depth = f32::MAX;
    let mut min_axis = Vec2::ZERO;
    if !sat_test_axes(a, b, a, &mut min_depth, &mut min_axis) {
        return None;
    }
    if !sat_test_axes(a, b, b, &mut min_depth, &mut min_axis) {
        return None;
    }

    // Orient so the axis points from b toward a (push a out of b).
    let mut centroid_a = Vec2::ZERO;
    for v in a {
        centroid_a += *v;
    }
    let centroid_a = centroid_a * (1.0 / a.len() as f32);
    let mut centroid_b = Vec2::ZERO;
    for v in b {
        centroid_b += *v;
    }
    let centroid_b = centroid_b * (1.0 / b.len() as f32);
    if (centroid_a - centroid_b).dot(min_axis) < 0.0 {
        min_axis = min_axis * -1.0;
    }
    Some((min_axis, min_depth))
}

/// Project both `a` and `b` onto each edge-perpendicular axis of `edge_source`.
/// Returns false on the first axis with no overlap (a separating axis exists,
/// no collision). Otherwise updates `min_depth` / `min_axis` with the smallest
/// overlap found and returns true.
fn sat_test_axes(
    a: &[Vec2],
    b: &[Vec2],
    edge_source: &[Vec2],
    min_depth: &mut f32,
    min_axis: &mut Vec2,
) -> bool {
    for i in 0..edge_source.len() {
        let p1 = edge_source[i];
        let p2 = edge_source[(i + 1) % edge_source.len()];
        let edge = p2 - p1;
        let len_sq = edge.length_squared();
        if len_sq < 1e-12 {
            continue;
        }
        let inv = 1.0 / math::sqrt(len_sq);
        let axis = Vec2::new(-edge.y * inv, edge.x * inv);
        let (a_min, a_max) = project_poly(a, axis);
        let (b_min, b_max) = project_poly(b, axis);
        // True MTV depth: the smaller of the two possible exit distances.
        // (Naively using `min(a_max, b_max) - max(a_min, b_min)` collapses to
        // the smaller polygon's extent when one is fully inside the other,
        // which is wrong — we want how far to push to escape, not the size of
        // the intersection.)
        let depth = (a_max - b_min).min(b_max - a_min);
        if depth <= 0.0 {
            return false;
        }
        if depth < *min_depth {
            *min_depth = depth;
            *min_axis = axis;
        }
    }
    true
}

/// True iff `p` lies inside (or on the edge of) the triangle `verts`.
/// Sign-of-cross-product test; works for any vertex winding.
pub fn point_in_triangle(p: Vec2, verts: &[Vec2; 3]) -> bool {
    let sign = |a: Vec2, b: Vec2, c: Vec2| (a.x - c.x) * (b.y - c.y) - (b.x - c.x) * (a.y - c.y);
    let d1 = sign(p, verts[0], verts[1]);
    let d2 = sign(p, verts[1], verts[2]);
    let d3 = sign(p, verts[2], verts[0]);
    let has_neg = d1 < 0.0 || d2 < 0.0 || d3 < 0.0;
    let has_pos = d1 > 0.0 || d2 > 0.0 || d3 > 0.0;
    !(has_neg && has_pos)
}

/// Swept point-vs-triangle: true if the segment from `prev` to `now` enters
/// or starts inside the triangle. Catches fast bullets that would tunnel
/// through if we only sampled the endpoint.
pub fn segment_hits_triangle(prev: Vec2, now: Vec2, verts: &[Vec2; 3]) -> bool {
    if point_in_triangle(now, verts) || point_in_triangle(prev, verts) {
        return true;
    }
    for i in 0..3 {
        if segments_intersect(prev, now, verts[i], verts[(i + 1) % 3]) {
            return true;
        }
    }
    false
}

/// True iff the two triangles overlap. Checks vertex containment in either
/// direction plus all 9 edge-pair intersections. Sufficient for convex
/// triangles.
pub fn triangles_overlap(a: &[Vec2; 3], b: &[Vec2; 3]) -> bool {
    for v in a {
        if point_in_triangle(*v, b) {
            return true;
        }
    }
    for v in b {
        if point_in_triangle(*v, a) {
            return true;
        }
    }
    for i in 0..3 {
        let a0 = a[i];
        let a1 = a[(i + 1) % 3];
        for j in 0..3 {
            let b0 = b[j];
            let b1 = b[(j + 1) % 3];
            if segments_intersect(a0, a1, b0, b1) {
                return true;
            }
        }
    }
    false
}

/// If `p` is inside the cone with apex at `apex`, axis direction `axis_unit`
/// (must be normalized), `length`, and half-angle `half_angle_rad`, returns
/// the force vector to apply (along the axis, with magnitude scaled by
/// linear distance falloff from `max_force` at the apex to 0 at the tip).
/// Otherwise returns `None`.
pub fn cone_force(
    p: Vec2,
    apex: Vec2,
    axis_unit: Vec2,
    length: f32,
    half_angle_rad: f32,
    max_force: f32,
) -> Option<Vec2> {
    let to_p = p - apex;
    let dist = to_p.length();
    if dist < 1e-3 || dist > length {
        return None;
    }
    let along = to_p.dot(axis_unit);
    if along <= 0.0 || along > length {
        return None;
    }
    // Angle test via cosines (avoids acos): cos(angle) = along / dist.
    let cos_angle = along / dist;
    let cos_half = math::cos(half_angle_rad);
    if cos_angle < cos_half {
        return None;
    }
    let strength = (1.0 - along / length) * max_force;
    Some(axis_unit * strength)
}

/// Circle vs line segment overlap. Returns (outward normal, penetration
/// depth) if the circle overlaps; `None` otherwise.
pub fn circle_segment_collide(c: Vec2, r: f32, a: Vec2, b: Vec2) -> Option<(Vec2, f32)> {
    let ab = b - a;
    let ac = c - a;
    let len_sq = ab.length_squared();
    let t = if len_sq > 0.0 {
        (ac.dot(ab) / len_sq).clamp(0.0, 1.0)
    } else {
        0.0
    };
    let closest = a + ab * t;
    let d = c - closest;
    let dist_sq = d.length_squared();
    if dist_sq >= r * r {
        return None;
    }
    let dist = math::sqrt(dist_sq);
    let normal = if dist > 1e-6 {
        d * (1.0 / dist)
    } else {
        // Degenerate: circle center exactly on the segment. Push along the
        // segment's perpendicular so we never produce NaN.
        let n = Vec2::new(-ab.y, ab.x);
        let nl = n.length();
        if nl > 1e-6 { n * (1.0 / nl) } else { Vec2::new(1.0, 0.0) }
    };
    Some((normal, r - dist))
}

/// Standard 2D segment-segment intersection test (no contact point).
pub fn segments_intersect(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2) -> bool {
    let r = p2 - p1;
    let s = p4 - p3;
    let rxs = r.x * s.y - r.y * s.x;
    if libm::fabsf(rxs) < 1e-7 {
        return false; // parallel; ignore collinear touching for v1
    }
    let qp = p3 - p1;
    let t = (qp.x * s.y - qp.y * s.x) / rxs;
    let u = (qp.x * r.y - qp.y * r.x) / rxs;
    (0.0..=1.0).contains(&t) && (0.0..=1.0).contains(&u)
}

/// Reflect velocity about a contact normal, scaling by (1 + restitution).
/// Only reflects if the velocity is moving INTO the surface.
pub fn reflect(vel: Vec2, normal: Vec2, restitution: f32) -> Vec2 {
    let vn = vel.dot(normal);
    if vn < 0.0 {
        vel - normal * (vn * (1.0 + restitution))
    } else {
        vel
    }
}

/// Lead-shot solver. Returns the unit aim direction (in world frame) the
/// shooter should aim its bullet, plus the time-to-hit, such that a bullet
/// fired now from `shooter_pos` with WORLD velocity `shooter_vel + bullet_speed * dir`
/// intersects a target moving from `target_pos` at `target_vel` (assuming
/// both maintain their current velocities).
///
/// `bullet_speed` is the bullet's speed in the shooter's frame — i.e. how
/// fast the bullet leaves the muzzle on top of the shooter's own motion.
/// Pass `shooter_vel = Vec2::ZERO` for a stationary shooter (cannons).
///
/// Returns `None` if no real intercept exists (target out-runs the bullet,
/// or coincident positions). The caller is responsible for any further
/// constraints (range, line-of-sight, fan limits, etc.) — this is just the
/// pure geometry.
///
/// Standard quadratic-in-t derivation:
///   bullet_pos(t) = S + (v_s + b*d) * t
///   target_pos(t) = T + v_t * t
///   set equal, isolate b*d*t = (T - S) + (v_t - v_s)*t = P + V*t
///   take squared magnitudes:  b²t² = |P + V*t|²
///   → (V·V - b²)*t² + 2*P·V*t + P·P = 0
pub fn lead_aim(
    shooter_pos: Vec2,
    shooter_vel: Vec2,
    target_pos: Vec2,
    target_vel: Vec2,
    bullet_speed: f32,
) -> Option<(Vec2, f32)> {
    let p = target_pos - shooter_pos;
    let v = target_vel - shooter_vel;
    if p.length_squared() < 1e-8 {
        return None;
    }
    let a = v.dot(v) - bullet_speed * bullet_speed;
    let b = 2.0 * p.dot(v);
    let c = p.dot(p);
    let t = if a.abs() < 1e-6 {
        // Degenerate quadratic — bullet exactly matches relative speed.
        // Falls through to the linear case b*t + c = 0.
        if b.abs() < 1e-6 {
            return None;
        }
        -c / b
    } else {
        let disc = b * b - 4.0 * a * c;
        if disc < 0.0 {
            return None;
        }
        let sqrt_disc = math::sqrt(disc);
        let t1 = (-b - sqrt_disc) / (2.0 * a);
        let t2 = (-b + sqrt_disc) / (2.0 * a);
        // Smallest strictly-positive root; bullets only travel forward in
        // time. Both negative → target is behind us in spacetime, no hit.
        let mut best = f32::INFINITY;
        for cand in [t1, t2] {
            if cand > 0.0 && cand < best {
                best = cand;
            }
        }
        if best.is_infinite() {
            return None;
        }
        best
    };
    if t <= 0.0 {
        return None;
    }
    // Aim vector = (intercept point) - (shooter's position at time t).
    // For a stationary shooter the second term is zero; for a moving one
    // it shifts the aim because the bullet inherits the shooter's velocity.
    let aim_world = (target_pos + target_vel * t) - (shooter_pos + shooter_vel * t);
    let len = aim_world.length();
    if len < 1e-6 {
        return None;
    }
    Some((aim_world * (1.0 / len), t))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tri(verts: [(f32, f32); 3]) -> [Vec2; 3] {
        [
            Vec2::new(verts[0].0, verts[0].1),
            Vec2::new(verts[1].0, verts[1].1),
            Vec2::new(verts[2].0, verts[2].1),
        ]
    }

    #[test]
    fn poly_far_from_wall_no_overlap() {
        let t = tri([(-7.0, 6.0), (7.0, 6.0), (0.0, -15.0)]);
        // Wall way to the right.
        let r = polygon_segment_collide(&t, Vec2::new(100.0, -50.0), Vec2::new(100.0, 50.0));
        assert!(r.is_none());
    }

    #[test]
    fn poly_straddles_wall_plane_but_past_segment_no_overlap() {
        let t = tri([(-7.0, 6.0), (7.0, 6.0), (0.0, -15.0)]);
        // Horizontal wall at y=0 but only from x=20..30 — triangle straddles
        // the wall PLANE but is far to the left of the segment.
        let r = polygon_segment_collide(&t, Vec2::new(20.0, 0.0), Vec2::new(30.0, 0.0));
        assert!(r.is_none());
    }

    #[test]
    fn poly_pokes_through_horizontal_wall() {
        // Triangle with nose at (0, -15) and base at y=6. Wall at y=0 from x=-50..50.
        // Triangle straddles: nose above wall, base below. Centroid at (0, -1) above wall (-Y is up).
        let t = tri([(-7.0, 6.0), (7.0, 6.0), (0.0, -15.0)]);
        let (n, d) = polygon_segment_collide(&t, Vec2::new(-50.0, 0.0), Vec2::new(50.0, 0.0)).unwrap();
        // Push direction should be -Y (toward centroid which is at y=-1 < 0).
        assert!(n.y < -0.99, "expected push -Y, got {:?}", n);
        // Depth = base extent below wall = 6 units.
        assert!((d - 6.0).abs() < 1e-3, "expected depth 6, got {}", d);
    }

    #[test]
    fn rotated_poly_back_edge_against_wall_then_into_it() {
        // Place a horizontal wall just below y=6 (wall at y=6.0). Triangle's
        // back edge sits on (-7, 6)..(7, 6). Snug — no overlap.
        let t = tri([(-7.0, 6.0), (7.0, 6.0), (0.0, -15.0)]);
        let snug = polygon_segment_collide(&t, Vec2::new(-50.0, 6.0), Vec2::new(50.0, 6.0));
        assert!(snug.is_none(), "snug should not overlap");

        // Now rotate triangle slightly toward the wall (push back vertices below 6).
        // Use a 0.2 rad rotation about origin: back-right (7, 6) → (~7.86, 4.49) — lifts above 6 actually.
        // Use a tiny shift instead: lower triangle by 1 unit.
        let lowered = tri([(-7.0, 7.0), (7.0, 7.0), (0.0, -14.0)]);
        let (n, d) = polygon_segment_collide(&lowered, Vec2::new(-50.0, 6.0), Vec2::new(50.0, 6.0)).unwrap();
        // Centroid at y = (7+7-14)/3 = 0, above wall (y=6 is below 0 in screen-down terms).
        // Wait, y=0 < y=6 means the centroid is ABOVE the wall (smaller y on screen).
        // Push should be in -Y (toward centroid).
        assert!(n.y < -0.99, "{:?}", n);
        // Depth = 1 (the amount the back edge dipped past wall).
        assert!((d - 1.0).abs() < 1e-3, "depth {}", d);
    }

    #[test]
    fn circle_segment_no_overlap() {
        let r = circle_segment_collide(Vec2::new(0.0, 0.0), 1.0, Vec2::new(5.0, 5.0), Vec2::new(10.0, 5.0));
        assert!(r.is_none());
    }

    #[test]
    fn circle_segment_overlap_pushes_outward() {
        // Horizontal segment at y=2; circle at origin with r=3 → overlaps by 1.
        let (n, p) = circle_segment_collide(
            Vec2::new(0.0, 0.0),
            3.0,
            Vec2::new(-5.0, 2.0),
            Vec2::new(5.0, 2.0),
        )
        .unwrap();
        assert!((p - 1.0).abs() < 1e-4, "penetration {}", p);
        // Normal should point from segment toward circle center, i.e. -Y.
        assert!(n.y < -0.99, "normal y {}", n.y);
    }

    #[test]
    fn segments_cross() {
        assert!(segments_intersect(
            Vec2::new(-1.0, 0.0),
            Vec2::new(1.0, 0.0),
            Vec2::new(0.0, -1.0),
            Vec2::new(0.0, 1.0),
        ));
    }

    #[test]
    fn segments_parallel_dont_intersect() {
        assert!(!segments_intersect(
            Vec2::new(0.0, 0.0),
            Vec2::new(1.0, 0.0),
            Vec2::new(0.0, 1.0),
            Vec2::new(1.0, 1.0),
        ));
    }

    #[test]
    fn reflect_into_wall() {
        // Moving down (+Y) into a horizontal floor (normal +X up screen-wise: -Y).
        let v = Vec2::new(2.0, 5.0);
        let n = Vec2::new(0.0, -1.0);
        let r = reflect(v, n, 0.5);
        // Y component should flip and shrink: 5 → -2.5.
        assert!((r.y + 2.5).abs() < 1e-4, "got {}", r.y);
        // Tangential preserved.
        assert!((r.x - 2.0).abs() < 1e-4);
    }

    fn quad(verts: [(f32, f32); 4]) -> Vec<Vec2> {
        verts.iter().map(|(x, y)| Vec2::new(*x, *y)).collect()
    }

    #[test]
    fn poly_poly_disjoint() {
        let ship = vec![
            Vec2::new(-7.0, 6.0),
            Vec2::new(7.0, 6.0),
            Vec2::new(0.0, -15.0),
        ];
        // Cell well to the right of ship.
        let cell = quad([(50.0, -10.0), (90.0, -10.0), (90.0, 30.0), (50.0, 30.0)]);
        assert!(polygon_polygon_collide(&ship, &cell).is_none());
    }

    #[test]
    fn poly_poly_grazing_overlap_pushes_perpendicular() {
        // Triangle pivot at origin; right-back vertex at x=7, y=6. Square
        // covering x in [5, 35], y in [-10, 20]: ship's right edge pokes 2
        // units into the square.
        let ship = vec![
            Vec2::new(-7.0, 6.0),
            Vec2::new(7.0, 6.0),
            Vec2::new(0.0, -15.0),
        ];
        let cell = quad([(5.0, -10.0), (35.0, -10.0), (35.0, 20.0), (5.0, 20.0)]);
        let (axis, depth) = polygon_polygon_collide(&ship, &cell).unwrap();
        // Push direction should be -X (away from cell, toward ship's centroid
        // at roughly (0, -1)).
        assert!(axis.x < -0.99, "expected -X push, got {:?}", axis);
        assert!((depth - 2.0).abs() < 1e-3, "expected depth 2, got {}", depth);
    }

    #[test]
    fn poly_poly_full_containment() {
        // Triangle ship fully inside a 50×40 cell — depenetration should pick
        // the axis with the smallest exit distance (not the ship's extent
        // along that axis, which is the trap the naive overlap formula falls
        // into).
        let ship = vec![
            Vec2::new(-7.0, 6.0),
            Vec2::new(7.0, 6.0),
            Vec2::new(0.0, -15.0),
        ];
        // Cell x ∈ [-25, 25], y ∈ [-20, 20]. Exits:
        //  +X: 25 - (-7) = 32;  -X: 7 - (-25) = 32
        //  +Y: 20 - (-15) = 35; -Y: 6 - (-20) = 26  ← min
        let cell = quad([(-25.0, -20.0), (25.0, -20.0), (25.0, 20.0), (-25.0, 20.0)]);
        let (axis, depth) = polygon_polygon_collide(&ship, &cell).unwrap();
        assert!(axis.y < -0.99, "expected -Y push, got {:?}", axis);
        assert!((depth - 26.0).abs() < 1e-3, "expected depth 26, got {}", depth);
    }

    #[test]
    fn poly_poly_against_diagonal_triangle() {
        // Triangle slope: filled top-left of a 35×35 cell at origin.
        // Solid corners: TL=(0,0), TR=(35,0), BL=(0,35). Hypotenuse TR↔BL.
        let slope = vec![
            Vec2::new(0.0, 0.0),
            Vec2::new(35.0, 0.0),
            Vec2::new(0.0, 35.0),
        ];
        // Ship pivot at (20, 20) — well into the solid wedge.
        let ship = vec![
            Vec2::new(13.0, 26.0),
            Vec2::new(27.0, 26.0),
            Vec2::new(20.0, 5.0),
        ];
        let (axis, _depth) = polygon_polygon_collide(&ship, &slope).unwrap();
        // Push should have positive X and Y (out toward BR — away from the
        // solid TL wedge along the hypotenuse normal).
        assert!(axis.x > 0.0 && axis.y > 0.0, "expected +X+Y push, got {:?}", axis);
    }

    #[test]
    fn reflect_no_op_when_moving_away() {
        let v = Vec2::new(2.0, -5.0);
        let n = Vec2::new(0.0, -1.0);
        let r = reflect(v, n, 0.5);
        assert_eq!(r, v);
    }

    /// Verifies lead_aim by firing the proposed bullet and checking that the
    /// intercept point on the target's path matches where the bullet ends up
    /// at time t. Tolerant of float drift.
    fn assert_intercepts(
        s_pos: Vec2,
        s_vel: Vec2,
        t_pos: Vec2,
        t_vel: Vec2,
        b_speed: f32,
    ) {
        let (dir, t) = lead_aim(s_pos, s_vel, t_pos, t_vel, b_speed)
            .expect("lead_aim should return a solution");
        let bullet_world_vel = s_vel + dir * b_speed;
        let bullet_at_t = s_pos + bullet_world_vel * t;
        let target_at_t = t_pos + t_vel * t;
        let err = (bullet_at_t - target_at_t).length();
        assert!(err < 0.01, "bullet at {:?}, target at {:?}, err={}", bullet_at_t, target_at_t, err);
        assert!(t > 0.0, "intercept time should be positive, got {}", t);
        assert!((dir.length() - 1.0).abs() < 1e-3, "dir not unit, len={}", dir.length());
    }

    #[test]
    fn lead_aim_stationary_target() {
        // Stationary shooter, stationary target → shoot straight at it.
        assert_intercepts(
            Vec2::new(0.0, 0.0),
            Vec2::new(0.0, 0.0),
            Vec2::new(100.0, 0.0),
            Vec2::new(0.0, 0.0),
            50.0,
        );
    }

    #[test]
    fn lead_aim_crossing_target() {
        // Stationary shooter at origin. Target at (200, 0) moving up at 30
        // u/s. Bullet 80 u/s. Need to lead — aim point should be ABOVE the
        // current target position.
        let (dir, t) = lead_aim(
            Vec2::ZERO,
            Vec2::ZERO,
            Vec2::new(200.0, 0.0),
            Vec2::new(0.0, -30.0),
            80.0,
        )
        .expect("solution exists");
        assert!(dir.y < 0.0, "should aim up to lead the target, dir={:?}", dir);
        assert!(t > 0.0);
        assert_intercepts(
            Vec2::ZERO,
            Vec2::ZERO,
            Vec2::new(200.0, 0.0),
            Vec2::new(0.0, -30.0),
            80.0,
        );
    }

    #[test]
    fn lead_aim_no_solution_when_target_outruns_bullet_directly_away() {
        // Target moving directly away faster than the bullet — no possible
        // intercept (bullet never catches up).
        let r = lead_aim(
            Vec2::ZERO,
            Vec2::ZERO,
            Vec2::new(100.0, 0.0),
            Vec2::new(200.0, 0.0),
            50.0,
        );
        assert!(r.is_none(), "no solution expected, got {:?}", r);
    }

    #[test]
    fn lead_aim_moving_shooter_inherits_velocity() {
        // Moving shooter: bullet gets the shooter's velocity added on. The
        // returned aim direction must compensate so the WORLD-velocity
        // bullet still hits the target.
        assert_intercepts(
            Vec2::ZERO,
            Vec2::new(50.0, 0.0),  // shooter drifting right at 50
            Vec2::new(200.0, 100.0),
            Vec2::new(-30.0, 20.0), // target moving up-left
            150.0,
        );
    }
}
