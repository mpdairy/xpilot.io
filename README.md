# xpilot.io

Browser multiplayer space combat in the spirit of the original
[XPilot](https://en.wikipedia.org/wiki/XPilot). Server-authoritative Rust
sim, WASM client, WebRTC DataChannel transport. Loads classic XPilot `.xp`
maps from `maps/`.

## Run

```
cargo run -p server --release             # listens on :8080
cd client && trunk serve                  # http://localhost:3000
```

The server picks `maps/tournament.xp` by default. Override:

```
XPILOT_MAP=globe cargo run -p server
```

## Looking for the Elm version?

The original Elm prototype (mpdairy, 2016ish) lives in [`old/`](old/) and
is preserved as a static reference. The `master` branch is now the v2
Rust + WASM rewrite — see [`CLAUDE.md`](CLAUDE.md) for the architecture and
roadmap, and [`xpilot_maps.md`](xpilot_maps.md) for the `.xp` map format.
