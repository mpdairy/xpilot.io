use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;

use axum::{routing::get, Router};
use shared::map::Map;
use tower_http::cors::{Any, CorsLayer};

mod bot;
mod connection;
mod lobby;
mod room;
mod webrtc_session;

use lobby::Lobby;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info,server=debug")),
        )
        .init();

    let map = match load_default_map() {
        Ok(m) => m,
        Err(e) => {
            tracing::error!("could not load map: {}", e);
            std::process::exit(1);
        }
    };
    tracing::info!(
        name = %map.name,
        width = map.width,
        height = map.height,
        walls = map.walls.len(),
        spawns = map.spawns.len(),
        "loaded map"
    );

    let lobby = Arc::new(Lobby::new(map));

    let app = Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/ws", get(connection::ws_handler))
        .layer(CorsLayer::new().allow_origin(Any))
        .with_state(lobby);

    let addr: SocketAddr = "0.0.0.0:8080".parse().unwrap();
    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    tracing::info!("listening on {}", addr);

    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .await
    .unwrap();
}

/// Resolve and load the map for the single room. Selection precedence:
/// 1. `XPILOT_MAP` env var (basename without extension)
/// 2. `tournament` (smallest of the bundled classics)
///
/// Map directory comes from `XPILOT_MAPS_DIR` (default `./maps`).
fn load_default_map() -> Result<Map, String> {
    let dir: PathBuf = std::env::var_os("XPILOT_MAPS_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("maps"));
    let name = std::env::var("XPILOT_MAP").unwrap_or_else(|_| "tournament".into());
    let path = dir.join(format!("{}.xp", name));
    let bytes = std::fs::read(&path)
        .map_err(|e| format!("read {}: {}", path.display(), e))?;
    // Classic .xp files are predominantly ASCII but some carry author names
    // in legacy 8-bit encodings (e.g. globe.xp's "Björn"). Lossy decode is
    // good enough since we only use the text bytes for keys and the grid.
    let src = String::from_utf8_lossy(&bytes);
    shared::xp_map::parse(&src).map_err(|e| format!("parse {}: {}", path.display(), e))
}
