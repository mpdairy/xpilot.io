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

    #[test]
    fn reflect_no_op_when_moving_away() {
        let v = Vec2::new(2.0, -5.0);
        let n = Vec2::new(0.0, -1.0);
        let r = reflect(v, n, 0.5);
        assert_eq!(r, v);
    }
}
