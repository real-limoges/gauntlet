//! M3-A spike: reqwest vs hyper client-side overhead.
//!
//! For a *measurement* tool, client-side overhead and (worse) its jitter are
//! measurement error. This harness isolates client overhead by firing many
//! requests at an in-process localhost mock that does ~zero work, then reports
//! the per-request latency floor (min), median, p99, and stddev for each client,
//! both sequential and at a fixed concurrency.
//!
//! Run: `cargo run -p gauntlet-engine --example client_overhead`
//! The numbers + verdict are recorded in `docs/adr/M3-A-client.md`.

use std::sync::Arc;
use std::time::Instant;

use axum::{routing::get, Json, Router};
use bytes::Bytes;
use http_body_util::{BodyExt, Full};
use hyper_util::client::legacy::Client;
use hyper_util::rt::TokioExecutor;
use tokio::sync::Semaphore;

const WARMUP: usize = 2_000;
const N: usize = 20_000;
const CONCURRENCY: usize = 32;

#[tokio::main]
async fn main() {
    // --- in-process mock: a trivial JSON 200, no work -----------------------
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let app = Router::new().route(
        "/ping",
        get(|| async { Json(serde_json::json!({"ok": true})) }),
    );
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    let url = format!("http://{addr}/ping");

    // --- clients ------------------------------------------------------------
    let rq = reqwest::Client::builder().build().unwrap();
    let hy: Client<_, Full<Bytes>> = Client::builder(TokioExecutor::new()).build_http();

    println!("M3-A client-overhead spike");
    println!("  target   : {url}");
    println!("  warmup   : {WARMUP}");
    println!("  requests : {N}  (sequential and at concurrency {CONCURRENCY})\n");

    // warm both clients (connection pool, TLS handshake n/a here, JIT paths).
    for _ in 0..WARMUP {
        reqwest_once(&rq, &url).await;
        hyper_once(&hy, &url).await;
    }

    // --- sequential ---------------------------------------------------------
    let rq_seq = seq(|i| {
        let rq = rq.clone();
        let url = url.clone();
        async move {
            let _ = i;
            reqwest_once(&rq, &url).await
        }
    })
    .await;
    let hy_seq = seq(|i| {
        let hy = hy.clone();
        let url = url.clone();
        async move {
            let _ = i;
            hyper_once(&hy, &url).await
        }
    })
    .await;

    // --- concurrent ---------------------------------------------------------
    let rq_con = concurrent({
        let rq = rq.clone();
        let url = url.clone();
        move || {
            let rq = rq.clone();
            let url = url.clone();
            async move { reqwest_once(&rq, &url).await }
        }
    })
    .await;
    let hy_con = concurrent({
        let hy = hy.clone();
        let url = url.clone();
        move || {
            let hy = hy.clone();
            let url = url.clone();
            async move { hyper_once(&hy, &url).await }
        }
    })
    .await;

    report("reqwest  sequential", &rq_seq);
    report("hyper    sequential", &hy_seq);
    report("reqwest  concurrent", &rq_con);
    report("hyper    concurrent", &hy_con);
}

/// One reqwest GET, fully draining the body. Returns elapsed microseconds.
async fn reqwest_once(client: &reqwest::Client, url: &str) -> f64 {
    let t = Instant::now();
    let resp = client.get(url).send().await.unwrap();
    let _ = resp.bytes().await.unwrap();
    t.elapsed().as_nanos() as f64 / 1_000.0
}

/// One hyper GET, fully draining the body. Returns elapsed microseconds.
async fn hyper_once(client: &Client<HttpConnector, Full<Bytes>>, url: &str) -> f64 {
    let t = Instant::now();
    let req = hyper::Request::builder()
        .uri(url)
        .body(Full::new(Bytes::new()))
        .unwrap();
    let resp = client.request(req).await.unwrap();
    let _ = resp.into_body().collect().await.unwrap().to_bytes();
    t.elapsed().as_nanos() as f64 / 1_000.0
}

type HttpConnector = hyper_util::client::legacy::connect::HttpConnector;

/// Fire N requests one at a time, collecting per-request microseconds.
async fn seq<F, Fut>(mut f: F) -> Vec<f64>
where
    F: FnMut(usize) -> Fut,
    Fut: std::future::Future<Output = f64>,
{
    let mut out = Vec::with_capacity(N);
    for i in 0..N {
        out.push(f(i).await);
    }
    out
}

/// Fire N requests with at most CONCURRENCY in flight, collecting per-request
/// microseconds. The semaphore caps in-flight exactly as the engine loop will.
async fn concurrent<F, Fut>(f: F) -> Vec<f64>
where
    F: Fn() -> Fut + Clone + Send + 'static,
    Fut: std::future::Future<Output = f64> + Send + 'static,
{
    let sem = Arc::new(Semaphore::new(CONCURRENCY));
    let mut set = tokio::task::JoinSet::new();
    for _ in 0..N {
        let permit = sem.clone().acquire_owned().await.unwrap();
        let f = f.clone();
        set.spawn(async move {
            let v = f().await;
            drop(permit);
            v
        });
    }
    let mut out = Vec::with_capacity(N);
    while let Some(r) = set.join_next().await {
        out.push(r.unwrap());
    }
    out
}

fn report(label: &str, samples: &[f64]) {
    use gauntlet_stats::{mean, percentile, std_dev};
    let min = samples.iter().copied().fold(f64::INFINITY, f64::min);
    println!(
        "{label}: floor {:7.1}us  median {:7.1}us  p99 {:8.1}us  stddev {:7.1}us  (mean {:.1})",
        min,
        percentile(0.50, samples),
        percentile(0.99, samples),
        std_dev(samples),
        mean(samples),
    );
}
