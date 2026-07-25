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
use crate::validation::CompiledSpec;

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

/// Per-host idle connection cap when `max_connections` is unset.
///
/// Matches the Haskell `managerConnCount`/`managerIdleConnectionCount` default.
/// reqwest's own default is effectively unbounded, which would silently change
/// what every existing config measures: a run at `concurrency: 50` would open 50
/// parallel connections where it used to queue on 10.
const DEFAULT_MAX_CONNECTIONS: usize = 10;

/// Build the shared pooled client from settings: request timeout and the
/// per-host idle connection cap.
pub fn build_client(settings: &Settings) -> Result<reqwest::Client> {
    let max_connections = settings
        .max_connections
        .map(|m| m.get() as usize)
        .unwrap_or(DEFAULT_MAX_CONNECTIONS);

    reqwest::Client::builder()
        .timeout(Duration::from_secs(
            settings.request_timeout_secs.get() as u64
        ))
        .pool_max_idle_per_host(max_connections)
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

/// An endpoint with every per-request cost already paid: the URL parsed, the
/// headers resolved into a `HeaderMap`, the bearer token applied, and the body
/// serialized once into refcounted [`Bytes`].
///
/// This exists to keep client-side work out of the measured window. Building the
/// request per call — re-parsing the URL, re-inserting headers, and above all
/// re-running `serde_json::to_vec` on the body — would land inside
/// `Instant::elapsed()` and be reported as server latency, with an error that
/// grows with body size. The Haskell hoisted the same work via `prepareRequest`.
#[derive(Clone, Debug)]
pub struct PreparedEndpoint {
    method: reqwest::Method,
    url: reqwest::Url,
    headers: reqwest::header::HeaderMap,
    body: Option<Bytes>,
    /// Carried along so the measurement loop has one thing to clone per request,
    /// with its `matches` patterns already compiled.
    pub validate: Option<CompiledSpec>,
}

/// Resolve an endpoint into a [`PreparedEndpoint`], once, before measuring.
///
/// A malformed URL or header fails here rather than once per request, so a bad
/// config is a startup error instead of N identical transport failures.
pub fn prepare(endpoint: &Endpoint, token: Option<&str>) -> Result<PreparedEndpoint> {
    use reqwest::header::{HeaderMap, HeaderName, HeaderValue, AUTHORIZATION};

    let url = reqwest::Url::parse(&endpoint.url)
        .map_err(|e| EngineError::Client(format!("invalid url {:?}: {e}", endpoint.url)))?;

    let mut headers = HeaderMap::new();
    for (k, v) in &endpoint.headers {
        let name = HeaderName::from_bytes(k.as_bytes())
            .map_err(|e| EngineError::Client(format!("invalid header name {k:?}: {e}")))?;
        let value = HeaderValue::from_str(v)
            .map_err(|e| EngineError::Client(format!("invalid value for header {k:?}: {e}")))?;
        headers.append(name, value);
    }

    // `HeaderMap` lookup is case-insensitive, so this covers an `authorization`
    // header supplied in any casing.
    if let Some(token) = token {
        if !headers.contains_key(AUTHORIZATION) {
            let mut value = HeaderValue::from_str(&format!("Bearer {token}"))
                .map_err(|e| EngineError::Client(format!("token is not a valid header: {e}")))?;
            value.set_sensitive(true);
            headers.insert(AUTHORIZATION, value);
        }
    }

    // Serialized as bytes (not via `.json()`) so the endpoint's own Content-Type
    // is respected rather than forced to application/json.
    let body = endpoint
        .body
        .as_ref()
        .map(|b| serde_json::to_vec(b).map(Bytes::from))
        .transpose()
        .map_err(|e| EngineError::Client(format!("could not serialize request body: {e}")))?;

    let validate = endpoint
        .validate
        .clone()
        .map(CompiledSpec::new)
        .transpose()
        .map_err(EngineError::Client)?;

    Ok(PreparedEndpoint {
        method: method(endpoint.method),
        url,
        headers,
        body,
        validate,
    })
}

/// Issue one prepared request and fully drain the body.
pub async fn send(
    client: &reqwest::Client,
    prepared: &PreparedEndpoint,
) -> std::result::Result<RawResponse, TransportError> {
    let mut req = client
        .request(prepared.method.clone(), prepared.url.clone())
        .headers(prepared.headers.clone());
    if let Some(body) = &prepared.body {
        // `Bytes` is refcounted — this clone does not re-serialize or re-allocate.
        req = req.body(body.clone());
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
