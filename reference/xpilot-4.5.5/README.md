# XPilot 4.5.5 reference (the original, NOT xpilot-ng)

Snapshot of UI / HUD / messaging code from the **original XPilot 4.5.5**
(by Bjørn Stabell, Ken Ronny Schouten, Bert Gijsbers et al., GPL-2).
Pulled from the xpilotgame SourceForge project on 2026-04-30:

  https://sourceforge.net/projects/xpilotgame/files/xpilot-4/xpilot-4.5.5/

This is a **design reference only** — not part of the build. We're a
Rust + WASM rewrite, not a port. Code stays here so we can answer
"how did the original do X?" without re-fetching the tarball each time.

If you want the rest of the source (server, robots, mapedit, audio,
networking) refetch the tarball — it's 1.1 MB and 99% of it is
irrelevant to our front-end concerns.

## What's here

```
src/client/
  painthud.c       HUD overlays — energy/fuel gauges, lock/target indicator
                   (`Paint_lock`), message rendering (`Paint_messages`).
  paint.c          Top-level frame loop — calls all the Paint_* fns.
  paint.h          Shared paint-side types and externs.
  paintdata.c      Setup of message buffers, color globals, font metrics.
  paintdata.h      MSG_LEN, MSG_DURATION, MSG_FLASH, MAX_MSGS, etc.
  talk.c           "Talk" (chat) input window — modal text entry, history,
                   selection. The M-key entry box equivalent.
  talk.h           Talk window types/protos.
  talkmacros.c     Tiny helpers used by talk.c.
  paintradar.c     Mini-radar drawing (top-left in the original layout).
src/common/
  const.h          Game-wide constants, possibly with MSG_LEN duplicated.
LICENSE.txt        GPL-2.
```

## Findings worth remembering

**Message layout** (`Paint_messages` in `painthud.c` ~line 753):

- Two separate buffers: `TalkMsg[]` (player chat) and `GameMsg[]` (server
  events including kills).
- **Talks render at the TOP-LEFT**, growing downward:
  `top_y = BORDER + messageFont->ascent`, then `top_y += SPACING` per line.
- **Game messages render at the BOTTOM-LEFT**, growing upward:
  `bot_y = ext_view_height - messageFont->descent - BORDER`, then
  `bot_y -= SPACING` per line.
- Both anchored at `x = BORDER` (BORDER = 10).
- Per-message life timer (`MSG_DURATION` ticks down via `msg->life--`).
- "Flashed" (recent) messages drawn in `RED`; older drawn in `oldMessagesColor`.
- Reverse-scroll mode (`SHOW_REVERSE_SCROLL` instrument bit) flips the
  iteration order so newest is at the bottom of each list.

So: our split — player chat top, kill notifications bottom — matches the
original layout exactly.

**Lock indicator** (`Paint_lock` in `painthud.c` ~line 254):

- Color: `hudLockColor` for non-allies, `hudColor` (regular HUD green) for
  allies / teammates.
- Position: `0.6 * HUD_SIZE` along the lock direction, anchored on the HUD
  centre — i.e. **inside the HUD box at 60 % of the half-extent**, not on
  the edge. Computed via `tcos(lock_dir)` / `-tsin(lock_dir)`.
- Size: `MIN(mapdiag / lock_dist, 10)` — INVERSE of distance, capped at 10
  px. Min 1 px. Closer target → bigger dot.
- Blink: only when `lock_dist <= WARNING_DISTANCE`. Beyond warning
  distance, dot is solid; inside it, drawn every other frame (`warningCount++ % 2`).
- Also draws: target's name above the HUD, distance number above-right of
  HUD, and (at low scale) a tiny silhouette of the locked ship.

So: our green dot matches in colour (we already use `#070`), but our
position is at the box edge — OG had it at 60 % of the half-extent. Our
size formula is linear (we lerp 2–5 px from near to far) — OG used
`min(mapdiag / dist, 10)` which is inverse-proportional. OG also blinked
when very close; we don't.

**Talk window** (`talk.c`):

- Modal text input opened with M key.
- Position: `TALK_WINDOW_Y = draw_height * 3/4 - TALK_WINDOW_HEIGHT/2` —
  about three-quarters down the screen.
- Width: `draw_width - 2 * (TALK_WINDOW_X + TALK_OUTSIDE_BORDER)` — almost
  full-width, with a 50-px left margin.
- History + selection support (Ctrl-key bindings for cursor / kill / yank).

So: our chat-input box is dead-center; OG had it at 75 % down. Either is
fine — the centre choice avoids covering the message stream.

## Re-fetch

```bash
curl -L -A "Wget/1.21" -o xpilot-4.5.5.tar.bz2 \
  "https://sourceforge.net/projects/xpilotgame/files/xpilot-4/xpilot-4.5.5/xpilot-4.5.5.tar.bz2/download"
tar xjf xpilot-4.5.5.tar.bz2
```

(SourceForge needs a non-curl User-Agent or it returns the interstitial
HTML instead of the binary.)
