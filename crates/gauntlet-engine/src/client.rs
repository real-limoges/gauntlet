//! HTTP request execution — the single point that touches the network client.
//!
//! Per ADR `M3-A-client`, the client is `reqwest` (rustls). This module is the
//! only place that knows that: it builds the pooled client and issues one request,
//! returning either a [`RawResponse`] (any HTTP status — 4xx/5xx are *responses*,
//! not failures) or a [`TransportError`] classified for retry.

use bytes::Bytes;
use std::time::Duration;

use gauntlet_core::{Endpoint, HttpMethod, Settings};

use crate::error::{EngineError, Result};

/// A completed HTTP exchange. Any status code counts — a 500 is a response, and
/// is never retried.
#[derive(Clone, Debug)]
pub struct RawResponse {
    pub status: u16,
    pub body: Bytes,
}

/// A transport-level failure (no HTTP response). `retryable` mirrors the Haskell
/// policy: connection refusals and timeouts are retried; everything else is not.
#[derive(Clone, Debug)]
pub struct TransportError {
    pub message: String,
    pub retryable: bool,
}

/// Build the shared pooled client from settings: request timeout and, if set,
/// the per-host idle connection cap.
pub fn build_client(settings: &Settings) -> Result<reqwest::Client> {
    let mut builder = reqwest::Client::builder().timeout(Duration::from_secs(
        settings.request_timeout_secs.get() as u64,
    ));
    if let Some(max) = settings.max_connections {
        builder = builder.pool_max_idle_per_host(max.get() as usize);
    }
    builder
        .build()
        .map_err(|e| EngineError::Client(e.to_string()))
}

fn method(m: HttpMethod) -> reqwest::Method {
    match m {
        HttpMethod::Get => reqwest::Method::GET,
        HttpMethod::Post => reqwest::Method::POST,
        HttpMethod::Put => reqwest::Method::PUT,
        HttpMethod::Delete => reqwest::Method::DELETE,
        HttpMethod::Patch => reqwest::Method::PATCH,
    }
}

/// Issue one request and fully drain the body. Bearer `token`, when present, is
/// applied unless the endpoint already carries an `Authorization` header.
pub async fn send(
    client: &reqwest::Client,
    endpoint: &Endpoint,
    token: Option<&str>,
) -> std::result::Result<RawResponse, TransportError> {
    let mut req = client.request(method(endpoint.method), &endpoint.url);
    let has_auth = endpoint
        .headers
        .iter()
        .any(|(k, _)| k.eq_ignore_ascii_case("authorization"));
    for (k, v) in &endpoint.headers {
        req = req.header(k, v);
    }
    if let Some(t) = token {
        if !has_auth {
            req = req.bearer_auth(t);
        }
    }
    if let Some(body) = &endpoint.body {
        // Serialize as bytes (not `.json()`) so the endpoint's own Content-Type
        // header is respected rather than forced to application/json.
        req = req.body(serde_json::to_vec(body).expect("a JSON value serializes"));
    }

    match req.send().await {
        Ok(resp) => {
            let status = resp.status().as_u16();
            match resp.bytes().await {
                Ok(body) => Ok(RawResponse { status, body }),
                Err(e) => Err(classify(&e)),
            }
        }
        Err(e) => Err(classify(&e)),
    }
}

/// Classify a reqwest error: connection refusals and timeouts are retryable.
fn classify(e: &reqwest::Error) -> TransportError {
    let retryable = e.is_connect() || e.is_timeout();
    let kind = if e.is_timeout() {
        "timeout"
    } else if e.is_connect() {
        "connection error"
    } else if e.is_body() || e.is_decode() {
        "body error"
    } else {
        "request error"
    };
    TransportError {
        message: format!("{kind}: {e}"),
        retryable,
    }
}
