//! Shared test support: a controllable in-process HTTP mock and config helpers.
//!
//! The mock counts requests, tracks peak in-flight concurrency, can delay each
//! response, and returns a fixed status + JSON body — enough to assert request
//! counts, the concurrency ceiling, retry behavior (via a *dead port* for
//! transport failures, since an HTTP 500 is deliberately not retried), and
//! validation outcomes.
//!
//! Shared across test binaries; each uses a different subset of the helpers.
#![allow(dead_code)]

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use axum::extract::State;
use axum::http::HeaderMap;
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::Json;

use gauntlet_core::BenchmarkConfig;

#[derive(Clone)]
pub struct MockState {
    count: Arc<AtomicUsize>,
    in_flight: Arc<AtomicUsize>,
    max_in_flight: Arc<AtomicUsize>,
    status: u16,
    body: serde_json::Value,
    delay_ms: u64,
    /// The `Authorization` header seen on each request, in arrival order.
    auth_headers: Arc<Mutex<Vec<Option<String>>>>,
}

/// A running mock server: its base URL plus observation counters.
pub struct Mock {
    pub base: String,
    state: MockState,
}

impl Mock {
    /// Total requests the server has handled.
    pub fn request_count(&self) -> usize {
        self.state.count.load(Ordering::SeqCst)
    }

    /// Peak simultaneous in-flight requests observed.
    pub fn max_in_flight(&self) -> usize {
        self.state.max_in_flight.load(Ordering::SeqCst)
    }

    /// The `Authorization` header value seen on each request, in arrival order.
    pub fn auth_headers(&self) -> Vec<Option<String>> {
        self.state.auth_headers.lock().unwrap().clone()
    }
}

/// Start a mock returning `status` + `body`, sleeping `delay_ms` per request.
pub async fn start(status: u16, body: serde_json::Value, delay_ms: u64) -> Mock {
    let state = MockState {
        count: Arc::new(AtomicUsize::new(0)),
        in_flight: Arc::new(AtomicUsize::new(0)),
        max_in_flight: Arc::new(AtomicUsize::new(0)),
        status,
        body,
        delay_ms,
        auth_headers: Arc::new(Mutex::new(Vec::new())),
    };
    let app = axum::Router::new()
        .fallback(handler)
        .with_state(state.clone());
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    Mock {
        base: format!("http://{addr}"),
        state,
    }
}

async fn handler(State(s): State<MockState>, headers: HeaderMap) -> impl IntoResponse {
    s.count.fetch_add(1, Ordering::SeqCst);
    s.auth_headers.lock().unwrap().push(
        headers
            .get("authorization")
            .and_then(|v| v.to_str().ok())
            .map(str::to_string),
    );
    let now = s.in_flight.fetch_add(1, Ordering::SeqCst) + 1;
    s.max_in_flight.fetch_max(now, Ordering::SeqCst);
    if s.delay_ms > 0 {
        tokio::time::sleep(Duration::from_millis(s.delay_ms)).await;
    }
    s.in_flight.fetch_sub(1, Ordering::SeqCst);
    (
        StatusCode::from_u16(s.status).unwrap_or(StatusCode::OK),
        Json(s.body.clone()),
    )
}

/// A 127.0.0.1 URL with nothing listening — connecting fails with "connection
/// refused", which the engine classifies as a retryable transport error.
pub fn dead_url() -> String {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();
    drop(listener);
    format!("http://{addr}")
}

/// Parse a config from JSON, panicking on failure (tests own their fixtures).
pub fn parse_config(json: &str) -> BenchmarkConfig {
    serde_json::from_str(json).expect("test config must parse")
}
