//! `gauntlet-engine` — the async measurement loop: tokio worker pool, HTTP
//! execution, rate limiting, response validation, warmup, lifecycle hooks.
//!
//! Entry point: [`run_benchmark`]. Targets run **sequentially** (so they don't
//! contend for the machine during measurement); within a target, endpoints run
//! concurrently, each internally gated by its own concurrency semaphore and load
//! limiter. The output is `TestingResponse` sample vectors that
//! `gauntlet_stats::calculate_stats` consumes.
//!
//! HTTP client choice is recorded in ADR `M3-A-client` (reqwest). The measurement
//! clock is monotonic (`Instant`), an intentional improvement over the Haskell.

pub mod client;
pub mod context;
pub mod csv_out;
pub mod endpoint;
pub mod error;
pub mod exec;
pub mod lifecycle;
pub mod rate_limiter;
pub mod validation;

use std::path::Path;
use std::sync::{Arc, Mutex};

use gauntlet_core::{build_endpoints, BenchmarkConfig, NamedTarget};
use tokio::task::JoinSet;

pub use context::RunContext;
pub use endpoint::EndpointResult;
pub use error::{EngineError, Result};

use csv_out::CsvSink;

/// One target's results: its name and the per-endpoint outcomes.
#[derive(Clone, Debug)]
pub struct TargetResult {
    pub name: String,
    pub endpoints: Vec<EndpointResult>,
}

/// A whole benchmark run: one entry per target, in run order.
#[derive(Clone, Debug)]
pub struct BenchmarkRun {
    pub targets: Vec<TargetResult>,
}

/// Run a full benchmark: validate, build the client, then for each target run
/// lifecycle setup → warmup → endpoints → teardown. When `csv_path` is set, every
/// response is appended to the latency CSV.
pub async fn run_benchmark(
    config: &BenchmarkConfig,
    csv_path: Option<&Path>,
) -> Result<BenchmarkRun> {
    config.validate()?;

    let client = client::build_client(&config.settings)?;
    let csv = match csv_path {
        Some(path) => Some(Arc::new(Mutex::new(CsvSink::create(path)?))),
        None => None,
    };

    let mut targets = Vec::with_capacity(config.targets.len());
    for target in &config.targets {
        let result = run_target(config, target, &client, &csv).await?;
        targets.push(result);
    }

    if let Some(csv) = &csv {
        csv.lock().expect("csv mutex not poisoned").flush()?;
    }

    Ok(BenchmarkRun { targets })
}

async fn run_target(
    config: &BenchmarkConfig,
    target: &NamedTarget,
    client: &reqwest::Client,
    csv: &Option<Arc<Mutex<CsvSink>>>,
) -> Result<TargetResult> {
    // --- setup + health check ----------------------------------------------
    if let Some(lifecycle) = &target.lifecycle {
        if let Some(setup) = &lifecycle.setup {
            lifecycle::run_hook(&target.name, "setup", setup).await?;
        }
        if let Some(hc) = &lifecycle.health_check {
            lifecycle::wait_healthy(&target.name, hc).await?;
        }
    }

    // teardown runs regardless of how the benchmark below turns out.
    let outcome = run_target_endpoints(config, target, client, csv).await;

    if let Some(lifecycle) = &target.lifecycle {
        if let Some(teardown) = &lifecycle.teardown {
            if let Err(e) = lifecycle::run_hook(&target.name, "teardown", teardown).await {
                eprintln!("warning: {e}");
            }
        }
    }

    outcome
}

async fn run_target_endpoints(
    config: &BenchmarkConfig,
    target: &NamedTarget,
    client: &reqwest::Client,
    csv: &Option<Arc<Mutex<CsvSink>>>,
) -> Result<TargetResult> {
    let endpoints = build_endpoints(&target.url, &config.payloads);
    let ctx = Arc::new(RunContext {
        client: client.clone(),
        token: None,
        settings: config.settings.clone(),
        csv: csv.clone(),
    });

    // Warm up the first endpoint, discarding results.
    if config.settings.warmup.iterations > 0 {
        if let Some(first) = endpoints.first() {
            endpoint::warmup(&ctx, first, config.settings.warmup.iterations).await;
        }
    }

    // Run endpoints concurrently; each pairs with its payload (by build order).
    let mut set = JoinSet::new();
    for (ep, payload) in endpoints.into_iter().zip(config.payloads.iter()) {
        let ctx = ctx.clone();
        let target_name = target.name.clone();
        let payload_name = payload.name.clone();
        set.spawn(endpoint::run_endpoint(ctx, target_name, payload_name, ep));
    }

    // Collect, then restore payload order (concurrent completion is unordered).
    let mut results: Vec<EndpointResult> = Vec::new();
    while let Some(joined) = set.join_next().await {
        let result = joined.expect("endpoint task did not panic")?;
        results.push(result);
    }
    results.sort_by_key(|r| payload_index(config, &r.name));

    Ok(TargetResult {
        name: target.name.clone(),
        endpoints: results,
    })
}

fn payload_index(config: &BenchmarkConfig, name: &str) -> usize {
    config
        .payloads
        .iter()
        .position(|p| p.name == name)
        .unwrap_or(usize::MAX)
}
