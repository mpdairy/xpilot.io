# xpilot-ng reference source

Snapshot of the relevant collision files from xpilot-ng (the modern fork of
the original XPilot, GPL-2). Kept here as a design reference — NOT part of
our build. Pulled from the `ksoderbl/xpilot-ng` GitHub mirror.

We used these to redesign our ship-vs-wall collision into the feature-level
swept approach (each ship vertex sweeps against nearby line segments + each
nearby wall corner is checked against ship edges in relative motion). See
`shared/src/world.rs` — `nearby_wall_features`, `sweep_point_vs_segment`,
`sweep_ship_edges_vs_point`, `swept_collide_translation`.

## Files

- `walls.c` — wall collision + bounce. Key functions:
    - `Move_player()` (~line 2851) — per-tick entry; loops sweep + bounce
      until all motion is consumed.
    - `Shape_move()` (~1074) — feature-level sweep: ship vertices vs wall
      segments PLUS wall vertices vs ship edges. Earliest hit wins.
    - `Lines_check()` (~805) — parametric segment-vs-segment intersection in
      fixed-point ("clicks").
    - `Bounce_player()` (~600) — velocity reflection across the contacted
      line, with restitution + several friction modes.
- `walls.h` — types: `shape_t` (ship polygon), `linet` (wall line segment
  with precomputed cos/sin of 2× wall angle for cheap reflection),
  `move_t`, `move_state_t`, `struct collans` (collision answer).
- `collision.c` — object-vs-object (ship-vs-ship, bullet-vs-ship). Not used
  by us yet but here for the same reason.

## Why we keep this

If we hit another collision-related design question (slope tile bounce
direction, ship-vs-bullet edge cases, lag-compensated rewinding, etc.) it's
worth re-reading this code first. xpilot-ng has 20+ years of feel-tuning
baked in — we want to crib their working answers, not redesign from scratch.

License is GPL-2 — we don't link or copy code into our binary, just look at
it. Our code is independent reimplementation.
