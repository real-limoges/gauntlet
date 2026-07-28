//! The four subcommands. Every one returns a [`RunOutcome`] rather than exiting;
//! see the crate root for the exit-code contract.

use std::path::{Path, PathBuf};
use std::time::SystemTime;

use anyhow::{Context, Result};
use gauntlet_core::{load_benchmark_config, BenchmarkConfig, NamedTarget};
use gauntlet_report::{
    compare_to_baseline, Baseline, BaselineStore, BenchmarkReport, MetricRegression, MultiReporter,
    RegressionResult, RegressionThresholds, Reporter, RunOutcome, StatsSnapshot, TargetReport,
    TerminalReporter,
};
use gauntlet_stats::{compare_bayesian, BenchmarkStats};

use crate::adapter;
use crate::cli::{BaselineMode, BenchmarkArgs};
use crate::reporters;

// ---------------------------------------------------------------------------
// benchmark
// ---------------------------------------------------------------------------

pub async fn benchmark(args: &BenchmarkArgs) -> Result<RunOutcome> {
    let config = load_benchmark_config(&args.config)
        .with_context(|| format!("loading config {}", args.config.display()))?;
    gauntlet_core::log::set_level(config.settings.log_level);

    let timestamp = chrono::Local::now().format("%Y-%m-%dT%H-%M-%S").to_string();
    let csv_path =
        (!args.no_csv).then(|| args.results_dir.join(format!("latencies-{timestamp}.csv")));

    let started = SystemTime::now();
    let run = if use_tui(args) {
        match run_with_tui(&config, csv_path.as_deref()).await? {
            Some(run) => run,
            // The operator cancelled. An interrupted run has no trustworthy
            // measurements, so it reports an error rather than a clean result
            // and no baseline is written.
            None => {
                gauntlet_core::log::warn("benchmark cancelled");
                return Ok(RunOutcome::Error);
            }
        }
    } else {
        gauntlet_engine::run_benchmark(&config, csv_path.as_deref()).await?
    };
    let finished = SystemTime::now();

    let report = adapter::to_report(&run);

    // Built once and kept, so the regression pass below fans out to the same
    // set. See the crate root.
    let reporters = reporters::for_benchmark(args);
    reporters.on_benchmark(&report).await?;

    // Diagnostic only: a trace backend that is down must never fail a clean
    // benchmark.
    report_traces(&config, started, finished).await;

    handle_baselines(args, &report, &reporters).await
}

/// Whether to run the live view: a human at an interactive terminal, keyed off
/// **stdout**. See the crate root.
fn use_tui(args: &BenchmarkArgs) -> bool {
    use std::io::IsTerminal;

    !args.no_tui
        && std::io::stdout().is_terminal()
        && gauntlet_report::detect_ci() == gauntlet_report::CiMode::None
}

/// Run the benchmark under the live view, returning `None` if cancelled.
async fn run_with_tui(
    config: &BenchmarkConfig,
    csv_path: Option<&Path>,
) -> Result<Option<gauntlet_engine::BenchmarkRun>> {
    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();

    // The benchmark owns its own task so the UI can render while it runs, and
    // so cancelling the UI can abort it.
    let config = config.clone();
    let csv_path = csv_path.map(|p| p.to_path_buf());
    let benchmark = tokio::spawn(async move {
        gauntlet_engine::run_benchmark_with_events(&config, csv_path.as_deref(), Some(tx)).await
    });

    let exit = gauntlet_tui::run(rx).await?;

    if exit == gauntlet_tui::Exit::Cancelled {
        benchmark.abort();
        return Ok(None);
    }

    Ok(Some(benchmark.await??))
}

/// Fetch and print Tempo trace analysis, swallowing every failure.
async fn report_traces(config: &BenchmarkConfig, start: SystemTime, end: SystemTime) {
    let Some(tempo) = &config.settings.tempo else {
        return;
    };

    match gauntlet_tracing::analyze(tempo, gauntlet_tracing::TraceWindow { start, end }).await {
        Ok(Some(analysis)) => print!("{}", gauntlet_tracing::render_terminal(&analysis)),
        Ok(None) => {}
        Err(e) => gauntlet_core::log::warn(format!(
            "trace analysis unavailable: {:#}",
            anyhow::Error::from(e)
        )),
    }
}

/// Save and/or compare baselines, one per target. Multi-target runs qualify the
/// name as `<name>--<target>`, so different systems cannot overwrite each
/// other's; a single-target run keeps the bare name.
async fn handle_baselines(
    args: &BenchmarkArgs,
    report: &BenchmarkReport,
    reporters: &MultiReporter,
) -> Result<RunOutcome> {
    let mode = args.baseline_mode();
    if mode == BaselineMode::None {
        return Ok(RunOutcome::Success);
    }

    let store = BaselineStore::new(&args.baseline_dir);
    let created_at = chrono::Utc::now().to_rfc3339();
    let qualify = |name: &str, target: &str| {
        if report.targets.len() > 1 {
            format!("{name}--{target}")
        } else {
            name.to_string()
        }
    };

    let mut regressions = Vec::new();
    for target in &report.targets {
        if let BaselineMode::Save(name) | BaselineMode::SaveAndCompare { save: name, .. } = &mode {
            let baseline = Baseline::capture(
                qualify(name, &target.name),
                created_at.clone(),
                &target.stats,
            );
            match store.save(&baseline) {
                Ok(path) => gauntlet_core::log::info(format!("baseline saved: {}", path.display())),
                // A failed save is worth knowing about but must not fail a run
                // whose measurements were fine.
                Err(e) => gauntlet_core::log::warn(format!("{:#}", anyhow::Error::from(e))),
            }
        }

        if let BaselineMode::Compare(name) | BaselineMode::SaveAndCompare { compare: name, .. } =
            &mode
        {
            let baseline = store
                .load(&qualify(name, &target.name))
                .with_context(|| format!("comparing target {}", target.name))?;
            let result =
                compare_to_baseline(&RegressionThresholds::default(), &baseline, &target.stats);
            regressions.push(result);
        }
    }

    if regressions.is_empty() {
        return Ok(RunOutcome::Success);
    }

    let combined = combine(regressions);
    reporters.on_regression(&combined).await?;
    Ok(RunOutcome::from_regression(combined))
}

/// Fold per-target comparisons into one result: the run passes only if every
/// target passed. Metric names are prefixed with their target so a multi-target
/// failure says *which* target regressed.
fn combine(mut results: Vec<RegressionResult>) -> RegressionResult {
    if results.len() == 1 {
        return results.remove(0);
    }

    let passed = results.iter().all(|r| r.passed);
    let baseline = results
        .iter()
        .map(|r| r.baseline.clone())
        .collect::<Vec<_>>()
        .join(", ");
    let metrics = results
        .into_iter()
        .flat_map(|r| {
            let label = r.baseline.clone();
            r.metrics.into_iter().map(move |m| MetricRegression {
                name: format!("{}/{}", label, m.name),
                ..m
            })
        })
        .collect();

    RegressionResult {
        baseline,
        metrics,
        passed,
    }
}

// ---------------------------------------------------------------------------
// compare
// ---------------------------------------------------------------------------

/// Compare two saved result files offline. Accepts either a baseline file or a
/// bare stats snapshot, so saved baselines can be diffed directly.
pub async fn compare(file_a: &Path, file_b: &Path) -> Result<RunOutcome> {
    let (name_a, stats_a) = load_stats(file_a)?;
    let (name_b, stats_b) = load_stats(file_b)?;

    let report = BenchmarkReport {
        comparisons: vec![gauntlet_report::PairComparison {
            a: name_a.clone(),
            b: name_b.clone(),
            comparison: compare_bayesian(&stats_a, &stats_b),
        }],
        targets: vec![
            TargetReport::new(name_a, stats_a),
            TargetReport::new(name_b, stats_b),
        ],
    };

    TerminalReporter::auto().on_benchmark(&report).await?;
    Ok(RunOutcome::Success)
}

/// Read a stats file, accepting either shape. EMD is unavailable here: both
/// formats store summary statistics, not the raw samples it needs.
fn load_stats(path: &Path) -> Result<(String, BenchmarkStats)> {
    let text =
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;

    let snapshot: StatsSnapshot = match serde_json::from_str::<Baseline>(&text) {
        Ok(baseline) => baseline.stats,
        Err(_) => serde_json::from_str(&text).with_context(|| {
            format!(
                "{} is neither a baseline nor a stats snapshot",
                path.display()
            )
        })?,
    };

    let name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();

    Ok((name, from_snapshot(&snapshot)))
}

/// Rebuild in-memory stats from a snapshot. The histogram is not persisted
/// (nothing reads it back), so it comes back empty and renderers skip it.
fn from_snapshot(s: &StatsSnapshot) -> BenchmarkStats {
    BenchmarkStats {
        total_requests: s.total_requests,
        count_success: s.count_success,
        count_failure: s.count_failure,
        mean_ms: s.mean_ms,
        std_dev_ms: s.std_dev_ms,
        min_ms: s.min_ms,
        max_ms: s.max_ms,
        p50_ms: s.p50_ms,
        p95_ms: s.p95_ms,
        p99_ms: s.p99_ms,
        es_ms: s.es_ms,
        histogram: Vec::new(),
    }
}

// ---------------------------------------------------------------------------
// validate
// ---------------------------------------------------------------------------

pub async fn validate(config_path: &Path, check_endpoints: bool) -> Result<RunOutcome> {
    let config = match load_benchmark_config(config_path) {
        Ok(config) => config,
        Err(e) => {
            gauntlet_core::log::error(format!("config error: {e}"));
            return Ok(RunOutcome::Error);
        }
    };

    gauntlet_core::log::set_level(config.settings.log_level);

    // `gauntlet-core` has no regex dependency, so pattern checking is not part
    // of its validation — but finding config errors without sending requests is
    // exactly this subcommand's job.
    if let Err(errors) = gauntlet_engine::validation::check_patterns(&config) {
        for e in &errors {
            gauntlet_core::log::error(format!("config error: {e}"));
        }
        return Ok(RunOutcome::Error);
    }

    println!("{}", summarize(&config));

    if !check_endpoints {
        return Ok(RunOutcome::Success);
    }

    let client = reqwest::Client::new();
    let mut all_reachable = true;
    for target in &config.targets {
        if !check_target(&client, target).await {
            all_reachable = false;
        }
    }

    Ok(if all_reachable {
        RunOutcome::Success
    } else {
        RunOutcome::Error
    })
}

/// The human-readable config summary. Pure, so it can be tested without a file.
fn summarize(config: &BenchmarkConfig) -> String {
    let mut out = String::from("Config OK\n");
    out.push_str(&format!("Targets ({}):\n", config.targets.len()));
    for target in &config.targets {
        out.push_str(&format!("  {} => {}\n", target.name, target.url));
    }
    out.push_str(&format!("Payloads:    {}\n", config.payloads.len()));
    out.push_str(&format!("Iterations:  {}\n", config.settings.iterations));
    out.push_str(&format!("Concurrency: {}\n", config.settings.concurrency));
    out.push_str(&format!("Load mode:   {:?}", config.settings.load_mode));
    out
}

/// The health-check URL for a target: its configured one, else `<url>/health`.
fn health_url(target: &NamedTarget) -> String {
    target
        .lifecycle
        .as_ref()
        .and_then(|l| l.health_check.as_ref())
        .map(|hc| hc.url.clone())
        .unwrap_or_else(|| format!("{}/health", target.url.trim_end_matches('/')))
}

async fn check_target(client: &reqwest::Client, target: &NamedTarget) -> bool {
    let url = health_url(target);
    let started = std::time::Instant::now();

    // GET, not HEAD: plenty of health endpoints only implement GET and answer
    // HEAD with 501, which would report a healthy service as unreachable. The
    // body is discarded either way.
    match client.get(&url).send().await {
        Ok(response) => {
            let status = response.status();
            println!(
                "  {} {} => HTTP {} ({}ms)",
                target.name,
                url,
                status.as_u16(),
                started.elapsed().as_millis()
            );
            status.is_success() || status.is_redirection()
        }
        Err(e) => {
            println!("  {} {} => unreachable ({e})", target.name, url);
            false
        }
    }
}

// ---------------------------------------------------------------------------
// schema
// ---------------------------------------------------------------------------

/// Print the derived config schema to stdout, or write it to `--out`.
pub fn schema(out: Option<&PathBuf>) -> Result<RunOutcome> {
    let schema = gauntlet_core::config_schema_string();

    match out {
        Some(path) => {
            if let Some(parent) = path.parent().filter(|p| !p.as_os_str().is_empty()) {
                std::fs::create_dir_all(parent)
                    .with_context(|| format!("creating {}", parent.display()))?;
            }
            std::fs::write(path, &schema).with_context(|| format!("writing {}", path.display()))?;
            println!("Wrote {}", path.display());
        }
        None => println!("{schema}"),
    }
    Ok(RunOutcome::Success)
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    use super::*;

    /// Counts the calls it receives, standing in for any non-terminal backend.
    #[derive(Default)]
    struct Counting(Arc<AtomicUsize>);

    #[async_trait::async_trait]
    impl Reporter for Counting {
        async fn on_regression(
            &self,
            _result: &RegressionResult,
        ) -> std::result::Result<(), gauntlet_report::Error> {
            self.0.fetch_add(1, Ordering::Relaxed);
            Ok(())
        }
    }

    /// `handle_baselines` used to build a throwaway `TerminalReporter` for the
    /// regression pass, which left every other backend's `on_regression` dead —
    /// no JUnit regression suite, no `gauntlet_regression_passed` series, and the
    /// whole `CiReporter` (whose only method this is) unreachable in production.
    #[tokio::test]
    async fn regressions_reach_every_configured_reporter_not_just_the_terminal() {
        use crate::cli::{Cli, Command};
        use clap::Parser;

        let dir = std::env::temp_dir().join(format!("gauntlet-dispatch-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir");

        let stats = BenchmarkStats {
            mean_ms: 10.0,
            ..Default::default()
        };
        BaselineStore::new(&dir)
            .save(&Baseline::capture("main", "2026-07-28T00:00:00Z", &stats))
            .expect("baseline saves");

        let args = match Cli::try_parse_from([
            "gauntlet",
            "benchmark",
            "-c",
            "cfg.json",
            "--compare-baseline",
            "main",
            "--baseline-dir",
            dir.to_str().expect("utf-8 temp path"),
        ])
        .expect("arguments parse")
        .command
        {
            Command::Benchmark(a) => *a,
            _ => unreachable!("benchmark subcommand"),
        };

        let hits = Arc::new(AtomicUsize::new(0));
        let reporters = MultiReporter::new(vec![Box::new(Counting(hits.clone()))]);
        let report = BenchmarkReport::single(TargetReport::new("t", stats));

        handle_baselines(&args, &report, &reporters)
            .await
            .expect("comparison runs");

        assert_eq!(
            hits.load(Ordering::Relaxed),
            1,
            "the configured reporter set must see the regression result"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    fn regression(baseline: &str, passed: bool) -> RegressionResult {
        RegressionResult {
            baseline: baseline.into(),
            metrics: vec![MetricRegression {
                name: "mean".into(),
                baseline: 10.0,
                current: if passed { 10.1 } else { 30.0 },
                change: if passed { 0.01 } else { 2.0 },
                threshold: 0.10,
                regressed: !passed,
            }],
            passed,
        }
    }

    #[test]
    fn a_single_targets_result_passes_through_unchanged() {
        let one = regression("main", true);
        assert_eq!(combine(vec![one.clone()]), one);
    }

    #[test]
    fn any_target_regressing_fails_the_whole_run() {
        let combined = combine(vec![regression("a", true), regression("b", false)]);
        assert!(!combined.passed);
        assert_eq!(RunOutcome::from_regression(combined).exit_code(), 1);
    }

    #[test]
    fn all_targets_passing_passes_the_run() {
        let combined = combine(vec![regression("a", true), regression("b", true)]);
        assert!(combined.passed);
        assert_eq!(RunOutcome::from_regression(combined).exit_code(), 0);
    }

    #[test]
    fn combined_metrics_say_which_target_they_came_from() {
        let combined = combine(vec![regression("a", true), regression("b", false)]);
        let names: Vec<&str> = combined.metrics.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, ["a/mean", "b/mean"]);
        assert_eq!(combined.baseline, "a, b");
    }

    #[test]
    fn a_snapshot_round_trips_through_the_baseline_format() {
        let original = BenchmarkStats {
            total_requests: 100,
            count_success: 99,
            count_failure: 1,
            mean_ms: 12.5,
            p99_ms: 40.0,
            histogram: vec![(0.0, 10)],
            ..Default::default()
        };
        let restored = from_snapshot(&StatsSnapshot::from(&original));

        assert_eq!(restored.mean_ms, original.mean_ms);
        assert_eq!(restored.p99_ms, original.p99_ms);
        assert_eq!(restored.count_failure, original.count_failure);
        assert!(
            restored.histogram.is_empty(),
            "the histogram is deliberately not persisted"
        );
    }

    #[test]
    fn compare_reads_both_a_baseline_file_and_a_bare_snapshot() {
        let dir = std::env::temp_dir().join(format!("gauntlet-cli-compare-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();

        let stats = BenchmarkStats {
            mean_ms: 12.5,
            ..Default::default()
        };

        let baseline_path = dir.join("as-baseline.json");
        std::fs::write(
            &baseline_path,
            serde_json::to_string(&Baseline::capture("main", "t", &stats)).unwrap(),
        )
        .unwrap();

        let snapshot_path = dir.join("as-snapshot.json");
        std::fs::write(
            &snapshot_path,
            serde_json::to_string(&StatsSnapshot::from(&stats)).unwrap(),
        )
        .unwrap();

        for path in [&baseline_path, &snapshot_path] {
            let (name, loaded) = load_stats(path).expect("both shapes load");
            assert_eq!(loaded.mean_ms, 12.5);
            assert!(!name.is_empty(), "the file stem names the series");
        }

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_file_that_is_neither_shape_is_rejected_with_the_path() {
        let path = std::env::temp_dir().join(format!("gauntlet-bad-{}.json", std::process::id()));
        std::fs::write(&path, r#"{"unrelated": true}"#).unwrap();

        let err = load_stats(&path).expect_err("nonsense is rejected");
        assert!(err.to_string().contains("neither a baseline"));

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn the_configs_log_level_becomes_the_active_threshold() {
        use gauntlet_core::{log, LogLevel};

        // `settings.log_level` was parsed but ignored after the port: the
        // Haskell built a logger from it and the Rust replacement printed
        // unconditionally. Setting it must actually take effect.
        log::set_level(LogLevel::Error);
        assert!(!log::enabled(LogLevel::Warning));

        log::set_level(LogLevel::Debug);
        assert!(log::enabled(LogLevel::Debug));

        log::set_level(LogLevel::Info);
    }

    #[test]
    fn the_schema_is_valid_json_describing_the_config() {
        let text = gauntlet_core::config_schema_string();
        let parsed: serde_json::Value = serde_json::from_str(&text).unwrap();
        assert!(parsed.get("properties").is_some() || parsed.get("$ref").is_some());
    }
}
