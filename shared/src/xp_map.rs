// Parser for classic XPilot block-based `.xp` / `.map` files.
//
// Format reference: ../../xpilot_maps.md (and `doc/README.MAPS` in the
// xpilot 4.5.5 source). Phase 1 only — we recognize walls (full + 4
// triangles), bases, and space; everything else (cannons, fuel, items,
// gravity, wormholes, checkpoints) becomes empty space.

use crate::map::{Block, BlockGrid, Map, SpawnPoint};
use crate::math::Vec2;

/// World units per block. Original XPilot uses 35; matches our existing
/// scale (ship triangle is ~24 units wide, so single-block corridors fit
/// with a hair of clearance, like the original).
pub const BLOCK_SZ: f32 = 35.0;

#[derive(Debug)]
pub enum ParseError {
    MissingKey(&'static str),
    BadInt(String),
    NoMapData,
    GridShorterThanHeight { expected: usize, got: usize },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MissingKey(k) => write!(f, "missing required key: {}", k),
            ParseError::BadInt(s) => write!(f, "expected integer, got: {}", s),
            ParseError::NoMapData => write!(f, "no mapData multiline block"),
            ParseError::GridShorterThanHeight { expected, got } => {
                write!(f, "mapData has {} rows, mapHeight is {}", got, expected)
            }
        }
    }
}

impl std::error::Error for ParseError {}

fn block_from_char(c: char) -> Block {
    match c {
        'x' => Block::Wall,
        's' => Block::TriUL,
        'a' => Block::TriUR,
        'w' => Block::TriLL,
        'q' => Block::TriLR,
        '_' | '0'..='9' => Block::Base,
        _ => Block::Space,
    }
}

pub fn parse(content: &str) -> Result<Map, ParseError> {
    let (opts, map_data) = tokenize(content);
    let map_data = map_data.ok_or(ParseError::NoMapData)?;

    let width = parse_int(&opts, "mapwidth")?;
    let height = parse_int(&opts, "mapheight")?;
    let name = opts
        .iter()
        .find(|(k, _)| k == "mapname")
        .map(|(_, v)| v.clone())
        .unwrap_or_else(|| "unnamed".into());
    let edge_wrap = opts
        .iter()
        .find(|(k, _)| k == "edgewrap")
        .map(|(_, v)| matches!(v.trim().to_ascii_lowercase().as_str(), "yes" | "true" | "on" | "1"))
        .unwrap_or(false);

    if map_data.len() < height {
        return Err(ParseError::GridShorterThanHeight {
            expected: height,
            got: map_data.len(),
        });
    }

    let mut grid = BlockGrid::empty(width as u32, height as u32, BLOCK_SZ);
    // Engine convention: world y grows DOWN (canvas-style — angle 0 means
    // forward = (0, -1) i.e. "up on screen"). The .xp file is written with
    // the visually-top row first, which is exactly what we want with no flip.
    for (row, line) in map_data.iter().take(height).enumerate() {
        for (x, c) in line.chars().take(width).enumerate() {
            grid.set(x as u32, row as u32, block_from_char(c));
        }
    }

    let mut spawns: Vec<SpawnPoint> = Vec::new();
    for y in 0..height as i64 {
        for x in 0..width as i64 {
            if matches!(grid.get(x, y), Block::Base) {
                let cx = (x as f32 + 0.5) * BLOCK_SZ;
                let cy = (y as f32 + 0.5) * BLOCK_SZ;
                // Bases face up = angle 0 in this engine (forward = (0, -1)).
                spawns.push(SpawnPoint {
                    pos: Vec2::new(cx, cy),
                    angle: 0.0,
                });
            }
        }
    }

    let mut map = Map {
        name,
        width: width as f32 * BLOCK_SZ,
        height: height as f32 * BLOCK_SZ,
        edge_wrap,
        blocks: Some(grid),
        spawns,
        walls: Vec::new(),
    };
    map.rebuild_walls();
    Ok(map)
}

fn tokenize(content: &str) -> (Vec<(String, String)>, Option<Vec<String>>) {
    let mut opts: Vec<(String, String)> = Vec::new();
    let mut map_data: Option<Vec<String>> = None;
    let mut iter = content.lines();
    while let Some(raw) = iter.next() {
        let line = strip_comment(raw);
        if line.trim().is_empty() {
            continue;
        }
        let Some((k_raw, v_raw)) = line.split_once(':') else { continue };
        let key = normalize_key(k_raw);
        let value = v_raw.trim_start();

        if let Some(rest) = value.strip_prefix("\\multiline:") {
            let delim = rest.trim();
            let mut buf: Vec<String> = Vec::new();
            for l in iter.by_ref() {
                let stripped = l.trim_end_matches('\r');
                if stripped == delim {
                    break;
                }
                buf.push(stripped.to_string());
            }
            if key == "mapdata" {
                map_data = Some(buf);
            } else {
                opts.push((key, buf.join("\n")));
            }
        } else {
            opts.push((key, value.trim().to_string()));
        }
    }
    (opts, map_data)
}

fn strip_comment(line: &str) -> &str {
    line.split_once('#').map(|(l, _)| l).unwrap_or(line)
}

fn normalize_key(k: &str) -> String {
    k.chars()
        .filter(|c| !c.is_whitespace())
        .flat_map(char::to_lowercase)
        .collect()
}

fn parse_int(opts: &[(String, String)], key: &'static str) -> Result<usize, ParseError> {
    let raw = opts
        .iter()
        .find(|(k, _)| k == key)
        .map(|(_, v)| v)
        .ok_or(ParseError::MissingKey(key))?;
    raw.trim()
        .parse::<usize>()
        .map_err(|_| ParseError::BadInt(raw.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minimal_map() {
        let src = "\
mapWidth: 5
mapHeight: 3
mapName: Tiny
mapData: \\multiline: END
xxxxx
x _ x
xxxxx
END
";
        let m = parse(src).unwrap();
        assert_eq!(m.name, "Tiny");
        assert_eq!(m.width, 5.0 * BLOCK_SZ);
        assert_eq!(m.height, 3.0 * BLOCK_SZ);
        assert!(!m.edge_wrap);
        assert_eq!(m.spawns.len(), 1);
        let s = &m.spawns[0];
        assert!((s.pos.x - 2.5 * BLOCK_SZ).abs() < 1e-3);
        assert!((s.pos.y - 1.5 * BLOCK_SZ).abs() < 1e-3);
    }

    #[test]
    fn solid_block_emits_four_walls_after_culling_plus_bbox() {
        let src = "\
mapWidth: 3
mapHeight: 3
mapData: \\multiline: END

 x

END
";
        let m = parse(src).unwrap();
        assert_eq!(m.walls.len(), 8);
    }

    #[test]
    fn wrap_flag_skips_bounding_box() {
        let src = "\
mapWidth: 3
mapHeight: 3
edgeWrap: yes
mapData: \\multiline: END

 x

END
";
        let m = parse(src).unwrap();
        assert!(m.edge_wrap);
        // Just the 4 sides of the single block, no bounding box.
        assert_eq!(m.walls.len(), 4);
    }

    #[test]
    fn long_corridor_merges_into_one_top_segment() {
        let src = "\
mapWidth: 5
mapHeight: 3
mapData: \\multiline: END

xxxxx

END
";
        let m = parse(src).unwrap();
        assert_eq!(m.walls.len(), 8);
    }

    #[test]
    fn comments_and_blank_lines_ignored() {
        let src = "\
# top comment
mapWidth: 1   # inline comment too
mapHeight: 1

mapData: \\multiline: ZZZ

ZZZ
";
        let m = parse(src).unwrap();
        assert_eq!(m.walls.len(), 4);
    }

    #[test]
    fn missing_mapdata_errors() {
        let err = parse("mapWidth: 1\nmapHeight: 1\n").unwrap_err();
        assert!(matches!(err, ParseError::NoMapData));
    }
}
