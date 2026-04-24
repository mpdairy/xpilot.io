# XPilot Classic Map Format (`.xp`)

Quick reference for parsing classic block-based XPilot maps. For xpilot.io, we really only need **walls** and **respawn points (bases)** to get started, but this doc covers enough of the rest to leave a decent runway for later.

> **Heads up:** "XPilot" has two map formats:
> - **Classic / block-based** (`.xp`, `.map`): a keyword-value config file with a fixed-width ASCII grid. Each char = one block. This is what we want.
> - **XPilot NG / polygon-based** (`.xp2`, XML): a newer, polygon-based format from XPilot-NG (2001+). More expressive but way more complex. Classic maps got auto-converted into polygons at load time in NG.
>
> This doc is the classic one. Authoritative sources: `doc/README.MAPS` and `src/server/map.c` in the xpilot 4.5.5 source tree.

## File structure

A `.xp` file is a plain-text config file. Each line is either:

- A `key: value` pair (case-insensitive key, whitespace in keys ignored — `mapWidth`, `mapwidth`, and `Map Width` all parse the same)
- A multiline value intro: `key: \multiline: <delimiter>`, followed by literal lines, terminated by a line exactly matching `<delimiter>`
- A comment (anything after `#` to end of line is ignored)
- Blank / whitespace-only (ignored)

Order of keys doesn't matter — you can put `mapData` at the top, middle, or bottom. But the dimensions (`mapWidth`, `mapHeight`) need to agree with the grid that follows.

### Minimum viable map

```
mapWidth: 10
mapHeight: 10
mapName: InnerSpace
mapAuthor: Ted Lemon
mapData: \multiline: EndOfMap
xxxxxxxxxx
x        x
x        x
x   _    x
x        x
x        x
x    _   x
x        x
x        x
xxxxxxxxxx
EndOfMap
```

The `\multiline:` delimiter can be anything — `EndOfMap`, `foo`, `ZZZ`, whatever. The delimiter line is not included in the data.

## Keys we care about right now

| Key | Type | Notes |
|---|---|---|
| `mapWidth` | int | Width in blocks |
| `mapHeight` | int | Height in blocks |
| `mapName` | string | Display name |
| `mapAuthor` | string | Author credit |
| `mapData` | multiline | The grid — `mapHeight` lines, each `mapWidth` chars wide |
| `teamPlay` | bool (yes/no) | If yes, base digits `0-9` indicate team |
| `edgeWrap` | bool | If yes, map wraps at edges instead of bouncing |
| `gravity` | float | Global gravity (-0.14 default); irrelevant for now but worth preserving |

There are 100+ other keys (weapons, item probs, fuel rates, wall bounce physics, etc.). We don't need to interpret them for xpilot.io — but when we parse a file we should **preserve unknown keys** as a `Map[String, String]` attached to the map so we can round-trip or expose them later.

## Coordinate system — read carefully

This is the one thing that's easy to get wrong.

- A block is `BLOCK_SZ = 35` pixels in the native engine (not that it matters for us — we pick our own scale).
- In-game coordinates are **Cartesian**: `(0, 0)` is the **bottom-left**, Y grows **upward**.
- In the file, the **first line of `mapData` is the TOP of the world** (highest y). Parsing walks `y` from `height - 1` down to `0`.

So if you naively `zip [0..]` over the lines, you get screen-space coords (y grows downward). To match the engine's coord system, either flip the lines or flip y during parsing:

```haskell
-- parsed: [[Char]] where head is top row
let height = length lines
let blocks = [ (x, height - 1 - row, c)
             | (row, line) <- zip [0..] lines
             , (x, c)      <- zip [0..] line
             ]
```

Whichever convention you pick for xpilot.io internals, **pick one and stick with it**. I'd suggest matching the original (Cartesian, y-up) since it's what all existing XPilot tooling assumes and the base direction constants (`DIR_UP = 32`) will otherwise make no sense.

## Block character table

This is the complete set, straight from `map.c` in xpilot 4.5.5. Grouped by what we need for v1 vs later.

### Phase 1: walls, space, respawns

| Char | Meaning | Notes |
|---|---|---|
| (space), `.` | Empty space | Also the default for any unknown char |
| `x` | Filled wall block | Solid 1×1 square |
| `s` | Triangle wall, filled corner = upper-left | "REC_LU" |
| `a` | Triangle wall, filled corner = upper-right | "REC_RU" |
| `w` | Triangle wall, filled corner = lower-left | "REC_LD" |
| `q` | Triangle wall, filled corner = lower-right | "REC_RD" |
| `_` | Base / respawn point (no team) | Faces up by default |
| `0`–`9` | Base for team N | Only meaningful when `teamPlay: yes`; else treated as base with no team |

For the triangle walls, the letter's physical position on the qwerty keyboard matches the filled corner (`q` top-left key → lower-right... wait, no — look at the code: `s` → REC_LU, `a` → REC_RU, `w` → REC_LD, `q` → REC_RD). The mnemonic is the **empty corner** points toward the key's position on the keyboard relative to a center. Honestly easiest to just memorize or keep the table handy. The `a`/`s`/`w`/`q` quartet means the four slopes, and `x` is the full block.

Base direction (`_` and digits): the parser sets `dir = DIR_UP` initially, then later runs `Find_base_dir()` which orients the base opposite to local gravity. For flat-gravity maps, bases face up. For our purposes, just treat all bases as "up-facing" for now.

### Phase 2+: everything else

Worth knowing about so our parser doesn't choke on them — even if we just skip.

**Cannons** (fire bullets at players; direction = which way the barrel points):

| Char | Direction |
|---|---|
| `r` | Up |
| `c` | Down |
| `d` | Left |
| `f` | Right |

**Fuel / items / targets:**

| Char | Meaning |
|---|---|
| `#` | Fuel station (refuel depot) |
| `*` | Treasure / ball (filled — starts with a ball) |
| `^` | Treasure box (empty — target for other teams) |
| `!` | Target (destructible block) |
| `%` | Item concentrator |
| `&` | Asteroid concentrator |
| `$` | Base attractor (pulls players toward nearest base) |

**Gravity wells** (apply force to nearby objects):

| Char | Direction |
|---|---|
| `+` | Positive (attractor, pulls in) |
| `-` | Negative (repeller, pushes out) |
| `>` | Clockwise swirl |
| `<` | Counter-clockwise swirl |
| `i` | Pushes up |
| `m` | Pushes down |
| `j` | Pushes left |
| `k` | Pushes right |

**Wormholes** (teleporters):

| Char | Meaning |
|---|---|
| `@` | Normal (in/out) wormhole |
| `(` | In-only wormhole |
| `)` | Out-only wormhole |

**Checkpoints** (race mode only; ignored if `timing: no`):

| Char | Meaning |
|---|---|
| `A`–`Z` | Checkpoint N (flown in alphabetical order) |

### Defaulting

From `map.c`: any character not explicitly listed — including the literal `.` and space — becomes `SPACE`. So a `?` or a stray tab won't crash the parser; it just becomes empty space. We should do the same, and optionally log a warning.

## Recommended internal representation

For Flint-style clean data types:

```haskell
data Map = Map
  { mapWidth   :: Int
  , mapHeight  :: Int
  , mapName    :: Text
  , mapAuthor  :: Text
  , teamPlay   :: Bool
  , edgeWrap   :: Bool
  , blocks     :: Vector (Vector Block)  -- indexed [y][x], y=0 is bottom
  , extraOpts  :: Map Text Text          -- preserved unknown keys
  }

data Block
  = Space
  | Wall WallShape
  | Base (Maybe Int)       -- team number if set
  | Fuel
  | Cannon Direction
  | Target
  | Treasure { full :: Bool }
  | Wormhole WormKind
  | Grav GravKind
  | Checkpoint Char        -- 'A'..'Z'
  | ItemConcentrator
  | AsteroidConcentrator
  | BaseAttractor

data WallShape = Full | TriLU | TriRU | TriLD | TriRD
data Direction = DirUp | DirDown | DirLeft | DirRight
```

For v1, you can collapse the whole right-hand side of `Block` into `Unknown Char` and only materialize `Space`, `Wall`, and `Base`. Just keep the char around so the full parser slots in later without re-reading the file.

## Parsing sketch

1. Read lines, split on `:` to get key/value (careful — values can contain `:` if it's part of `\multiline:`).
2. Normalize keys: lowercase, strip internal whitespace.
3. When you hit a `\multiline: <delim>` value, consume lines until a line equals `<delim>` exactly.
4. Once all options are parsed:
   - Validate: `mapData` exists, `length mapData == mapHeight`, every line's length ≥ `mapWidth` (short lines get space-padded per `Map_missing_error`; extra chars are discarded).
   - Build the grid with y flipped (first line = y = height-1).
5. For each char, dispatch via the table above.

## Where to get maps

Good news — the classic xpilot source tree (mirrored on GitHub at `pilotniq/xpilot`) ships with **20 classic `.xp` maps** under `lib/maps/`, including:

- **`blood-music2.xp`** — yes, "Blood's Music II", 100×100, by Blood/TIMID/DR.DEATH (1995). I've pulled this out and attached it.
- `globe.xp` — The Globe, the classic newbie map
- `newdarkhell.xp`, `newdarkhell2.xp` — NDH
- `tournament.xp`, `teamball.xp` — other team maps
- `planetx.xp`, `cloudscape.xp`, `fuzz.xp`, `tourmination.xp`, etc.

To get the rest:

```
git clone https://github.com/pilotniq/xpilot.git
cd xpilot/lib/maps
```

**Other sources** (broader, older collection):
- `https://web.mit.edu/games/lib/xpilot/maps/` — MIT mirror, `.map` files (same format, just different extension). Browsable.
- The BloodsPilot project on SourceForge bundles curated Blood's Music variants.
- `budwin.net/insectoid/xpilot/` — community archive with additional custom maps.

For reference note: the original **"Blood's Music"** (not II) is what most people mean when they say "Blood's Music" — it's a 1994 Patrick Kenny map. "Blood's Music II" is a 1995 follow-up from different authors. Both are worth supporting; the II is the one shipped with xpilot 4.5.5. If you want the original Patrick Kenny version specifically, it'll be in older xpilot tarballs or via the MIT archive — check for filenames like `bloods.map` or `bloodsmusic.map`.
