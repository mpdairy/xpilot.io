use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;

use axum::{routing::get, Router};
use tower_http::cors::{Any, CorsLayer};

mod bot;
mod connection;
mod lobby;
mod room;
mod webrtc_session;

use lobby::{Lobby, MapRegistry};

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info,server=debug")),
        )
        .init();

    let dir: PathBuf = std::env::var_os("XPILOT_MAPS_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("maps"));
    let maps = match MapRegistry::load_from_dir(&dir) {
        Ok(m) => m,
        Err(e) => {
            tracing::error!("could not load maps: {}", e);
            std::process::exit(1);
        }
    };
    tracing::info!(
        count = maps.names_sorted.len(),
        names = ?maps.names_sorted,
        "loaded maps"
    );
    let default_map = std::env::var("XPILOT_DEFAULT_MAP").unwrap_or_else(|_| "tournament".into());
    if !maps.contains(&default_map) {
        tracing::error!(
            "default map '{}' not in maps dir; available: {:?}",
            default_map,
            maps.names_sorted
        );
        std::process::exit(1);
    }

    let lobby = Arc::new(Lobby::new(maps, default_map));

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
