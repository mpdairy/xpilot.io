use serde::{Deserialize, Serialize};

use crate::constants::{WORLD_HEIGHT, WORLD_WIDTH};
use crate::math::Vec2;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WallSegment {
    pub a: Vec2,
    pub b: Vec2,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SpawnPoint {
    pub pos: Vec2,
    pub angle: f32,
}

/// Single-cell map element. The four `Tri*` variants are XPilot's slope
/// blocks — the suffix names the corner that's *filled* (UL = upper-left, etc).
/// `Cannon*` variants are OPEN cells (not walls) that emit a spawn point at
/// the cell center facing the named direction. Visually rendered as a single
/// white line along the opposite cell edge — the line acts as the "back
/// wall" the ship rests against, with its perpendicular giving the facing
/// direction. (Classic XPilot's r/c/d/f cannons, repurposed as oriented
/// spawns.)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Block {
    Space = 0,
    Wall = 1,
    /// `s` — filled upper-left, hypotenuse upper-right ↔ lower-left.
    TriUL = 2,
    /// `a` — filled upper-right, hypotenuse upper-left ↔ lower-right.
    TriUR = 3,
    /// `w` — filled lower-left, hypotenuse upper-left ↔ lower-right.
    TriLL = 4,
    /// `q` — filled lower-right, hypotenuse upper-right ↔ lower-left.
    TriLR = 5,
    Base = 6,
    CannonUp = 7,
    CannonDown = 8,
    CannonLeft = 9,
    CannonRight = 10,
    /// Active cannon — full wall block that fires periodically in the named
    /// direction and can be destroyed. Respawns after `CANNON_RESPAWN_SECONDS`.
    /// Distinct from the `Cannon*` (lowercase r/c/d/f) spawn-direction
    /// markers above; these (uppercase R/C/D/F in the .xp grid) are the real
    /// classic-XPilot turret.
    CannonFireUp = 11,
    CannonFireDown = 12,
    CannonFireLeft = 13,
    CannonFireRight = 14,
}

impl Block {
    pub fn from_u8(b: u8) -> Block {
        match b {
            1 => Block::Wall,
            2 => Block::TriUL,
            3 => Block::TriUR,
            4 => Block::TriLL,
            5 => Block::TriLR,
            6 => Block::Base,
            7 => Block::CannonUp,
            8 => Block::CannonDown,
            9 => Block::CannonLeft,
            10 => Block::CannonRight,
            11 => Block::CannonFireUp,
            12 => Block::CannonFireDown,
            13 => Block::CannonFireLeft,
            14 => Block::CannonFireRight,
            _ => Block::Space,
        }
    }

    pub fn is_wall(self) -> bool {
        matches!(
            self,
            Block::Wall
                | Block::TriUL
                | Block::TriUR
                | Block::TriLL
                | Block::TriLR
                | Block::CannonFireUp
                | Block::CannonFireDown
                | Block::CannonFireLeft
                | Block::CannonFireRight
        )
    }
    pub fn has_top(self) -> bool {
        matches!(
            self,
            Block::Wall
                | Block::TriUL
                | Block::TriUR
                | Block::CannonFireUp
                | Block::CannonFireDown
                | Block::CannonFireLeft
                | Block::CannonFireRight
        )
    }
    pub fn has_bottom(self) -> bool {
        matches!(
            self,
            Block::Wall
                | Block::TriLL
                | Block::TriLR
                | Block::CannonFireUp
                | Block::CannonFireDown
                | Block::CannonFireLeft
                | Block::CannonFireRight
        )
    }
    pub fn has_left(self) -> bool {
        matches!(
            self,
            Block::Wall
                | Block::TriUL
                | Block::TriLL
                | Block::CannonFireUp
                | Block::CannonFireDown
                | Block::CannonFireLeft
                | Block::CannonFireRight
        )
    }
    pub fn has_right(self) -> bool {
        matches!(
            self,
            Block::Wall
                | Block::TriUR
                | Block::TriLR
                | Block::CannonFireUp
                | Block::CannonFireDown
                | Block::CannonFireLeft
                | Block::CannonFireRight
        )
    }

    /// For a cannon block, the spawn-point direction (engine angle) — the
    /// ship spawns at the cell's center. None for non-cannons.
    /// Engine convention: angle 0 = up (-y), π/2 = right, π = down, -π/2 = left.
    pub fn cannon_angle(self) -> Option<f32> {
        use core::f32::consts::PI;
        match self {
            Block::CannonUp => Some(0.0),
            Block::CannonRight => Some(PI * 0.5),
            Block::CannonDown => Some(PI),
            Block::CannonLeft => Some(-PI * 0.5),
            _ => None,
        }
    }

    /// For an active cannon, the firing direction (engine angle) and the
    /// unit vector pointing in that direction. None for non-cannons.
    pub fn cannon_fire(self) -> Option<(f32, Vec2)> {
        use core::f32::consts::PI;
        match self {
            Block::CannonFireUp => Some((0.0, Vec2::new(0.0, -1.0))),
            Block::CannonFireRight => Some((PI * 0.5, Vec2::new(1.0, 0.0))),
            Block::CannonFireDown => Some((PI, Vec2::new(0.0, 1.0))),
            Block::CannonFireLeft => Some((-PI * 0.5, Vec2::new(-1.0, 0.0))),
            _ => None,
        }
    }
}

/// Block-based map representation — the source of truth shipped from server
/// to client. Both sides reconstruct the wall-segment list from this; the
/// server never sends a wall list over the wire.
///
/// Cells are row-major with `y = 0` at the bottom of the world (Cartesian,
/// y-up — same as the original xpilot engine).
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct BlockGrid {
    pub width: u32,
    pub height: u32,
    pub block_size: f32,
    /// One byte per cell. See `Block` for the encoding.
    pub cells: Vec<u8>,
}

impl BlockGrid {
    pub fn get(&self, x: i64, y: i64) -> Block {
        if x < 0 || y < 0 || x as u32 >= self.width || y as u32 >= self.height {
            return Block::Space;
        }
        Block::from_u8(self.cells[(y as u32 * self.width + x as u32) as usize])
    }

    pub fn set(&mut self, x: u32, y: u32, b: Block) {
        self.cells[(y * self.width + x) as usize] = b as u8;
    }

    pub fn empty(width: u32, height: u32, block_size: f32) -> Self {
        Self {
            width,
            height,
            block_size,
            cells: vec![0u8; (width * height) as usize],
        }
    }

    /// Bounding-box walls for the world this grid spans. Returned separately
    /// so callers (xp_map) can decide whether to include them — wrap maps
    /// don't want hard edges.
    pub fn bounding_walls(&self) -> Vec<WallSegment> {
        let w = self.width as f32 * self.block_size;
        let h = self.height as f32 * self.block_size;
        vec![
            WallSegment { a: Vec2::new(0.0, 0.0), b: Vec2::new(w, 0.0) },
            WallSegment { a: Vec2::new(w, 0.0), b: Vec2::new(w, h) },
            WallSegment { a: Vec2::new(w, h), b: Vec2::new(0.0, h) },
            WallSegment { a: Vec2::new(0.0, h), b: Vec2::new(0.0, 0.0) },
        ]
    }

    /// Build the collision wall list from the grid. Axis-aligned edges are
    /// merged into runs (one segment per corridor side) so a 200×200 map
    /// stays under ~1500 segments. Triangle hypotenuses are emitted
    /// individually — no easy collinear merge.
    pub fn build_walls(&self) -> Vec<WallSegment> {
        let bs = self.block_size;
        let w = self.width as i64;
        let h = self.height as i64;
        let mut walls: Vec<WallSegment> = Vec::new();

        // Horizontal edges at integer y. Engine is y-down, so cell (x, y-1)
        // is ABOVE cell (x, y); the boundary lies at the bottom edge of the
        // upper cell and the top edge of the lower cell — query each
        // accordingly.
        for y in 0..=h {
            let mut run_start: Option<i64> = None;
            let close_run =
                |walls: &mut Vec<WallSegment>, run_start: &mut Option<i64>, end: i64| {
                    if let Some(s) = run_start.take() {
                        walls.push(WallSegment {
                            a: Vec2::new(s as f32 * bs, y as f32 * bs),
                            b: Vec2::new(end as f32 * bs, y as f32 * bs),
                        });
                    }
                };
            for x in 0..w {
                let edge = self.get(x, y - 1).has_bottom() ^ self.get(x, y).has_top();
                if edge {
                    if run_start.is_none() {
                        run_start = Some(x);
                    }
                } else {
                    close_run(&mut walls, &mut run_start, x);
                }
            }
            close_run(&mut walls, &mut run_start, w);
        }

        // Vertical edges at integer x between cols x-1 and x.
        for x in 0..=w {
            let mut run_start: Option<i64> = None;
            let close_run =
                |walls: &mut Vec<WallSegment>, run_start: &mut Option<i64>, end: i64| {
                    if let Some(s) = run_start.take() {
                        walls.push(WallSegment {
                            a: Vec2::new(x as f32 * bs, s as f32 * bs),
                            b: Vec2::new(x as f32 * bs, end as f32 * bs),
                        });
                    }
                };
            for y in 0..h {
                let edge = self.get(x - 1, y).has_right() ^ self.get(x, y).has_left();
                if edge {
                    if run_start.is_none() {
                        run_start = Some(y);
                    }
                } else {
                    close_run(&mut walls, &mut run_start, y);
                }
            }
            close_run(&mut walls, &mut run_start, h);
        }

        // Triangle hypotenuses. In y-down engine: top-left = (lx, ly),
        // top-right = (hx, ly), bottom-left = (lx, hy), bottom-right =
        // (hx, hy). Each hypotenuse joins the two non-filled corners.
        for y in 0..h {
            for x in 0..w {
                let lx = x as f32 * bs;
                let ly = y as f32 * bs;
                let hx = lx + bs;
                let hy = ly + bs;
                let (a, b) = match self.get(x, y) {
                    // Filled top-left: empty bottom-right; hyp top-right ↔ bottom-left.
                    Block::TriUL => (Vec2::new(hx, ly), Vec2::new(lx, hy)),
                    // Filled top-right: empty bottom-left; hyp top-left ↔ bottom-right.
                    Block::TriUR => (Vec2::new(lx, ly), Vec2::new(hx, hy)),
                    // Filled bottom-left: empty top-right; hyp top-left ↔ bottom-right.
                    Block::TriLL => (Vec2::new(lx, ly), Vec2::new(hx, hy)),
                    // Filled bottom-right: empty top-left; hyp top-right ↔ bottom-left.
                    Block::TriLR => (Vec2::new(hx, ly), Vec2::new(lx, hy)),
                    _ => continue,
                };
                walls.push(WallSegment { a, b });
            }
        }

        walls
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Map {
    pub name: String,
    pub width: f32,
    pub height: f32,
    /// Toroidal world. When true, ship/bullet/particle positions wrap modulo
    /// world dims and the outer bounding box is omitted.
    pub edge_wrap: bool,
    /// Source of truth for walls. None for hand-built test maps that
    /// populate `walls` directly.
    pub blocks: Option<BlockGrid>,
    pub spawns: Vec<SpawnPoint>,
    /// Collision walls. Recomputed from `blocks` on both sides; not on the
    /// wire (the server already stripped its wall list to save bandwidth).
    /// Hand-built maps (arena_map) populate this directly and leave
    /// `blocks = None`.
    #[serde(skip)]
    pub walls: Vec<WallSegment>,
}

impl Map {
    /// Recompute `walls` from `blocks`. Idempotent. Call after deserializing
    /// a Map that came across the wire (the wire form has empty `walls`).
    /// No-op when `blocks` is None — hand-built maps own their wall list.
    pub fn rebuild_walls(&mut self) {
        let Some(grid) = self.blocks.as_ref() else { return };
        let mut walls = grid.build_walls();
        if !self.edge_wrap {
            walls.extend(grid.bounding_walls());
        }
        self.walls = walls;
    }
}

/// M1 hardcoded test map — a small bordered arena with a couple internal
/// walls so dogfights stay close-quarters and players can actually find each
/// other. Used by the world.rs test suite; live games load `.xp` files via
/// `xp_map::parse`.
pub fn arena_map() -> Map {
    let w = WORLD_WIDTH;
    let h = WORLD_HEIGHT;
    let walls = vec![
        WallSegment { a: Vec2::new(0.0, 0.0), b: Vec2::new(w, 0.0) },
        WallSegment { a: Vec2::new(w, 0.0), b: Vec2::new(w, h) },
        WallSegment { a: Vec2::new(w, h), b: Vec2::new(0.0, h) },
        WallSegment { a: Vec2::new(0.0, h), b: Vec2::new(0.0, 0.0) },
        WallSegment { a: Vec2::new(750.0, 450.0), b: Vec2::new(1050.0, 450.0) },
        WallSegment { a: Vec2::new(1050.0, 450.0), b: Vec2::new(1050.0, 750.0) },
    ];
    let half_pi = core::f32::consts::FRAC_PI_2;
    let spawns = vec![
        SpawnPoint { pos: Vec2::new(w * 0.2, h * 0.5), angle: half_pi },
        SpawnPoint { pos: Vec2::new(w * 0.8, h * 0.5), angle: -half_pi },
    ];
    Map {
        name: "arena".into(),
        width: w,
        height: h,
        edge_wrap: false,
        blocks: None,
        spawns,
        walls,
    }
}
