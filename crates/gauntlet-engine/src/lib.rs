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

pub mod auth;
pub mod client;
pub mod context;
pub mod csv_out;
pub mod endpoint;
pub mod error;
pub mod event;
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
pub use event::{BenchmarkEvent, EventSink};

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
    run_benchmark_with_events(config, csv_path, None).await
}

/// As [`run_benchmark`], but also streaming progress to a live UI. The sender is
/// unbounded on purpose — see `event`.
pub async fn run_benchmark_with_events(
    config: &BenchmarkConfig,
    csv_path: Option<&Path>,
    events: EventSink,
) -> Result<BenchmarkRun> {
    config.validate()?;

    let client = client::build_client(&config.settings)?;
    // Resolved once per run: the token is the same for every target and request.
    let token = match &config.settings.secrets {
        Some(path) => auth::read_token(path)?,
        None => None,
    };
    let csv = match csv_path {
        Some(path) => Some(Arc::new(Mutex::new(CsvSink::create(path)?))),
        None => None,
    };

    let mut targets = Vec::with_capacity(config.targets.len());
    for (index, target) in config.targets.iter().enumerate() {
        event::emit(
            &events,
            BenchmarkEvent::TargetStarted {
                name: target.name.clone(),
                index: index + 1,
                total_requests: expected_requests(config),
            },
        );
        let result =
            match run_target(config, target, &client, token.as_deref(), &csv, &events).await {
                Ok(result) => result,
                Err(e) => {
                    event::emit(
                        &events,
                        BenchmarkEvent::Failed {
                            message: e.to_string(),
                        },
                    );
                    return Err(e);
                }
            };
        targets.push(result);
    }
    event::emit(&events, BenchmarkEvent::Finished);

    if let Some(csv) = &csv {
        csv.lock().expect("csv mutex not poisoned").flush()?;
    }

    Ok(BenchmarkRun { targets })
}

/// Requests one target is expected to issue: iterations x endpoints. A
/// duration-based load mode has no such number up front, so the UI shows
/// elapsed progress instead and this is only a hint.
fn expected_requests(config: &BenchmarkConfig) -> usize {
    config.settings.iterations.get() as usize * config.payloads.len()
}

async fn run_target(
    config: &BenchmarkConfig,
    target: &NamedTarget,
    client: &reqwest::Client,
    token: Option<&str>,
    csv: &Option<Arc<Mutex<CsvSink>>>,
    events: &EventSink,
) -> Result<TargetResult> {
    // --- branch switch + setup + health check ------------------------------
    // The branch moves first: a setup hook that builds or restarts the service
    // has to see the revision it is meant to build.
    if let Some(branch) = &target.branch {
        lifecycle::switch_branch(&target.name, branch).await?;
    }

    if let Some(lifecycle) = &target.lifecycle {
        if let Some(setup) = &lifecycle.setup {
            event::emit(
                &events.clone(),
                BenchmarkEvent::Status {
                    message: format!("Setting up {}...", target.name),
                },
            );
            lifecycle::run_hook(&target.name, "setup", setup).await?;
        }
        if let Some(hc) = &lifecycle.health_check {
            lifecycle::wait_healthy(&target.name, hc).await?;
        }
    }

    // teardown runs regardless of how the benchmark below turns out.
    let outcome = run_target_endpoints(config, target, client, token, csv, events).await;

    if let Some(lifecycle) = &target.lifecycle {
        if let Some(teardown) = &lifecycle.teardown {
            if let Err(e) = lifecycle::run_hook(&target.name, "teardown", teardown).await {
                gauntlet_core::log::warn(e.to_string());
            }
        }
    }

    outcome
}

async fn run_target_endpoints(
    config: &BenchmarkConfig,
    target: &NamedTarget,
    client: &reqwest::Client,
    token: Option<&str>,
    csv: &Option<Arc<Mutex<CsvSink>>>,
    events: &EventSink,
) -> Result<TargetResult> {
    let endpoints = build_endpoints(&target.url, &config.payloads);
    let ctx = Arc::new(RunContext {
        client: client.clone(),
        token: token.map(str::to_string),
        settings: config.settings.clone(),
        csv: csv.clone(),
        events: events.clone(),
    });

    // Warm up the first endpoint, discarding results.
    if config.settings.warmup.iterations > 0 {
        if let Some(first) = endpoints.first() {
            endpoint::warmup(&ctx, first, config.settings.warmup.iterations).await?;
        }
    }

    // One set of load gates for the whole target, shared by every endpoint below:
    // `concurrency` and `target_rpm` describe the load on the target, not on each
    // payload. Built after warmup so the limiter's clock starts with measurement.
    let gates = endpoint::LoadGates::for_target(&config.settings);

    // Run endpoints concurrently; each pairs with its payload (by build order).
    let mut set = JoinSet::new();
    let endpoint_count = endpoints.len();
    for (index, (ep, payload)) in endpoints
        .into_iter()
        .zip(config.payloads.iter())
        .enumerate()
    {
        event::emit(
            events,
            BenchmarkEvent::EndpointStarted {
                name: payload.name.clone(),
                index: index + 1,
                total: endpoint_count,
            },
        );
        let ctx = ctx.clone();
        let gates = gates.clone();
        let target_name = target.name.clone();
        let payload_name = payload.name.clone();
        set.spawn(endpoint::run_endpoint(
            ctx,
            gates,
            target_name,
            payload_name,
            ep,
        ));
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
