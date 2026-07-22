//! Baseline persistence and regression detection.
//!
//! A baseline is a named snapshot of one target's statistics, saved to
//! `baselines/<name>.json`. A later run compares against it metric by metric;
//! any metric exceeding its threshold fails the run, which is what drives the
//! exit-code contract ([`RunOutcome`]).
//!
//! The on-disk shape is [`StatsSnapshot`] — a deliberate mirror of
//! `BenchmarkStats` rather than a serde derive on the stats type itself, for two
//! reasons: `gauntlet-stats` is dependency-light by design (libm only), and the
//! file format should be free to evolve separately from the in-memory type.
//! Only the four compared metrics plus context are stored; the histogram is
//! dropped, since nothing reads it back and it dominates the file size.

use std::path::{Path, PathBuf};

use gauntlet_stats::BenchmarkStats;
use serde::{Deserialize, Serialize};

use crate::error::{Error, Result};

/// Current baseline file schema. Bump when the shape changes incompatibly.
pub const BASELINE_SCHEMA_VERSION: u32 = 1;

/// Default directory for saved baselines, relative to the working directory.
pub const DEFAULT_BASELINE_DIR: &str = "baselines";

/// The persisted subset of `BenchmarkStats`.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StatsSnapshot {
    pub total_requests: usize,
    pub count_success: usize,
    pub count_failure: usize,
    pub mean_ms: f64,
    pub std_dev_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
    pub p50_ms: f64,
    pub p95_ms: f64,
    pub p99_ms: f64,
    pub es_ms: f64,
}

impl From<&BenchmarkStats> for StatsSnapshot {
    fn from(s: &BenchmarkStats) -> Self {
        Self {
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
        }
    }
}

/// A saved baseline: a named, timestamped statistics snapshot.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Baseline {
    pub schema_version: u32,
    pub name: String,
    /// RFC 3339 timestamp of when the baseline was captured.
    pub created_at: String,
    pub stats: StatsSnapshot,
}

impl Baseline {
    /// Capture `stats` as a baseline named `name`, stamped `created_at`.
    ///
    /// The timestamp is passed in rather than read from the clock so the
    /// function stays pure and testable; the CLI supplies the run's timestamp,
    /// which also keeps a baseline's stamp consistent with its run's artifacts.
    pub fn capture(
        name: impl Into<String>,
        created_at: impl Into<String>,
        stats: &BenchmarkStats,
    ) -> Self {
        Self {
            schema_version: BASELINE_SCHEMA_VERSION,
            name: name.into(),
            created_at: created_at.into(),
            stats: StatsSnapshot::from(stats),
        }
    }
}

/// Thresholds for regression detection, as fractions (`0.10` = 10%).
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct RegressionThresholds {
    pub mean: f64,
    pub p50: f64,
    pub p95: f64,
    pub p99: f64,
}

impl Default for RegressionThresholds {
    /// 10% for mean/p50/p95, 15% for p99 — the tail is noisier, so it gets more
    /// room before it counts as a regression.
    fn default() -> Self {
        Self {
            mean: 0.10,
            p50: 0.10,
            p95: 0.10,
            p99: 0.15,
        }
    }
}

/// One metric's regression status.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MetricRegression {
    pub name: String,
    pub baseline: f64,
    pub current: f64,
    /// Relative change; positive is a regression (slower), negative an
    /// improvement.
    pub change: f64,
    pub threshold: f64,
    pub regressed: bool,
}

/// The outcome of comparing a run against a baseline.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RegressionResult {
    pub baseline: String,
    pub metrics: Vec<MetricRegression>,
    /// True when no metric exceeded its threshold.
    pub passed: bool,
}

impl RegressionResult {
    /// Just the metrics that regressed.
    pub fn regressed(&self) -> impl Iterator<Item = &MetricRegression> {
        self.metrics.iter().filter(|m| m.regressed)
    }
}

/// How a run ended. The discriminants are the process exit codes, and CI
/// depends on them: `0` clean, `1` regression, `2` error.
#[derive(Clone, Debug, PartialEq)]
pub enum RunOutcome {
    Success,
    Regression(RegressionResult),
    Error,
}

impl RunOutcome {
    /// The process exit code for this outcome.
    pub fn exit_code(&self) -> i32 {
        match self {
            RunOutcome::Success => 0,
            RunOutcome::Regression(_) => 1,
            RunOutcome::Error => 2,
        }
    }

    /// Classify a comparison: a passing result is a success, a failing one is a
    /// regression.
    pub fn from_regression(result: RegressionResult) -> Self {
        if result.passed {
            RunOutcome::Success
        } else {
            RunOutcome::Regression(result)
        }
    }
}

/// Compare current statistics against a baseline.
pub fn compare_to_baseline(
    thresholds: &RegressionThresholds,
    baseline: &Baseline,
    current: &BenchmarkStats,
) -> RegressionResult {
    let base = &baseline.stats;
    let metrics = vec![
        check_metric("mean", thresholds.mean, base.mean_ms, current.mean_ms),
        check_metric("p50", thresholds.p50, base.p50_ms, current.p50_ms),
        check_metric("p95", thresholds.p95, base.p95_ms, current.p95_ms),
        check_metric("p99", thresholds.p99, base.p99_ms, current.p99_ms),
    ];

    RegressionResult {
        baseline: baseline.name.clone(),
        passed: !metrics.iter().any(|m| m.regressed),
        metrics,
    }
}

/// Relative change for one metric, and whether it exceeds the threshold.
///
/// A zero baseline can't produce a ratio: zero-to-zero is no change, and
/// zero-to-anything is treated as a 100% regression rather than infinity.
fn check_metric(name: &str, threshold: f64, baseline: f64, current: f64) -> MetricRegression {
    let change = if baseline == 0.0 {
        if current == 0.0 {
            0.0
        } else {
            1.0
        }
    } else {
        (current - baseline) / baseline
    };

    MetricRegression {
        name: name.to_string(),
        baseline,
        current,
        change,
        threshold,
        regressed: change > threshold,
    }
}

/// A directory of saved baselines.
#[derive(Clone, Debug)]
pub struct BaselineStore {
    dir: PathBuf,
}

impl Default for BaselineStore {
    fn default() -> Self {
        Self::new(DEFAULT_BASELINE_DIR)
    }
}

impl BaselineStore {
    pub fn new(dir: impl Into<PathBuf>) -> Self {
        Self { dir: dir.into() }
    }

    pub fn dir(&self) -> &Path {
        &self.dir
    }

    fn path_for(&self, name: &str) -> PathBuf {
        self.dir.join(format!("{name}.json"))
    }

    /// Write a baseline, creating the directory if needed. Returns its path.
    pub fn save(&self, baseline: &Baseline) -> Result<PathBuf> {
        std::fs::create_dir_all(&self.dir).map_err(|source| Error::write(&self.dir, source))?;
        let path = self.path_for(&baseline.name);
        let json = serde_json::to_string_pretty(baseline)?;
        std::fs::write(&path, json).map_err(|source| Error::write(&path, source))?;
        Ok(path)
    }

    /// Read a named baseline.
    ///
    /// A file written by the Haskell implementation is detected and reported as
    /// an unsupported format, so the operator sees an actionable message rather
    /// than a field-level deserialization error.
    pub fn load(&self, name: &str) -> Result<Baseline> {
        let path = self.path_for(name);
        if !path.exists() {
            return Err(Error::BaselineNotFound {
                name: name.to_string(),
                path,
            });
        }
        let text = std::fs::read_to_string(&path).map_err(|source| Error::read(&path, source))?;

        if is_legacy_baseline(&text) {
            return Err(Error::LegacyBaseline { path });
        }

        Ok(serde_json::from_str(&text)?)
    }

    /// Names of every saved baseline, sorted. A missing directory lists as
    /// empty rather than erroring.
    pub fn list(&self) -> Result<Vec<String>> {
        if !self.dir.exists() {
            return Ok(Vec::new());
        }
        let entries =
            std::fs::read_dir(&self.dir).map_err(|source| Error::read(&self.dir, source))?;

        let mut names = Vec::new();
        for entry in entries {
            let path = entry
                .map_err(|source| Error::read(&self.dir, source))?
                .path();
            if path.extension().and_then(|e| e.to_str()) == Some("json") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    names.push(stem.to_string());
                }
            }
        }
        names.sort();
        Ok(names)
    }
}

/// Recognize the Haskell-era baseline encoding by its camelCase record fields.
fn is_legacy_baseline(text: &str) -> bool {
    match serde_json::from_str::<serde_json::Value>(text) {
        Ok(serde_json::Value::Object(map)) => {
            map.contains_key("baselineName") || map.contains_key("baselineStats")
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stats(mean: f64, p50: f64, p95: f64, p99: f64) -> BenchmarkStats {
        BenchmarkStats {
            mean_ms: mean,
            p50_ms: p50,
            p95_ms: p95,
            p99_ms: p99,
            ..Default::default()
        }
    }

    fn baseline_of(stats: &BenchmarkStats) -> Baseline {
        Baseline::capture("main", "2026-07-21T12:00:00Z", stats)
    }

    #[test]
    fn identical_stats_pass() {
        let s = stats(10.0, 9.0, 20.0, 30.0);
        let result = compare_to_baseline(&RegressionThresholds::default(), &baseline_of(&s), &s);
        assert!(result.passed);
        assert_eq!(result.metrics.len(), 4);
        assert!(result.metrics.iter().all(|m| m.change == 0.0));
    }

    #[test]
    fn improvement_is_not_a_regression() {
        let base = baseline_of(&stats(10.0, 9.0, 20.0, 30.0));
        let current = stats(5.0, 4.0, 10.0, 15.0);
        let result = compare_to_baseline(&RegressionThresholds::default(), &base, &current);
        assert!(result.passed);
        assert!(result.metrics.iter().all(|m| m.change < 0.0));
    }

    #[test]
    fn change_just_under_threshold_passes_and_just_over_fails() {
        let base = baseline_of(&stats(100.0, 100.0, 100.0, 100.0));
        let thresholds = RegressionThresholds::default();

        // mean threshold is 10%: 109.9 passes, 110.1 does not.
        let under = compare_to_baseline(&thresholds, &base, &stats(109.9, 100.0, 100.0, 100.0));
        assert!(under.passed);

        let over = compare_to_baseline(&thresholds, &base, &stats(110.1, 100.0, 100.0, 100.0));
        assert!(!over.passed);
        assert_eq!(over.regressed().count(), 1);
        assert_eq!(over.regressed().next().unwrap().name, "mean");
    }

    #[test]
    fn p99_gets_the_looser_fifteen_percent_threshold() {
        let base = baseline_of(&stats(100.0, 100.0, 100.0, 100.0));
        let thresholds = RegressionThresholds::default();

        // +12% on p99 is within its 15% budget; the same move on p95 is not.
        let p99_only = compare_to_baseline(&thresholds, &base, &stats(100.0, 100.0, 100.0, 112.0));
        assert!(p99_only.passed);

        let p95_too = compare_to_baseline(&thresholds, &base, &stats(100.0, 100.0, 112.0, 112.0));
        assert!(!p95_too.passed);
    }

    #[test]
    fn zero_baseline_edge_cases() {
        let base = baseline_of(&stats(0.0, 0.0, 0.0, 0.0));
        let thresholds = RegressionThresholds::default();

        let still_zero = compare_to_baseline(&thresholds, &base, &stats(0.0, 0.0, 0.0, 0.0));
        assert!(still_zero.passed);

        // Zero to nonzero is a 100% regression, not an infinite one.
        let now_nonzero = compare_to_baseline(&thresholds, &base, &stats(5.0, 0.0, 0.0, 0.0));
        assert!(!now_nonzero.passed);
        assert_eq!(now_nonzero.metrics[0].change, 1.0);
    }

    #[test]
    fn exit_codes_are_the_ci_contract() {
        assert_eq!(RunOutcome::Success.exit_code(), 0);
        assert_eq!(RunOutcome::Error.exit_code(), 2);

        let base = baseline_of(&stats(10.0, 10.0, 10.0, 10.0));
        let failing = compare_to_baseline(
            &RegressionThresholds::default(),
            &base,
            &stats(50.0, 10.0, 10.0, 10.0),
        );
        assert_eq!(RunOutcome::from_regression(failing).exit_code(), 1);

        let passing = compare_to_baseline(
            &RegressionThresholds::default(),
            &base,
            &stats(10.0, 10.0, 10.0, 10.0),
        );
        assert_eq!(RunOutcome::from_regression(passing).exit_code(), 0);
    }

    #[test]
    fn save_then_load_round_trips() {
        let dir = tempdir();
        let store = BaselineStore::new(&dir);
        let baseline = baseline_of(&stats(10.0, 9.0, 20.0, 30.0));

        let path = store.save(&baseline).expect("save succeeds");
        assert!(path.exists());
        assert_eq!(store.load("main").expect("load succeeds"), baseline);
    }

    #[test]
    fn loading_a_missing_baseline_names_it() {
        let dir = tempdir();
        let store = BaselineStore::new(&dir);
        let err = store.load("nope").expect_err("missing baseline errors");
        assert!(matches!(err, Error::BaselineNotFound { .. }));
        assert!(err.to_string().contains("nope"));
    }

    #[test]
    fn haskell_era_baselines_are_reported_as_unsupported() {
        let dir = tempdir();
        let store = BaselineStore::new(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("old.json"),
            r#"{"baselineName":"old","baselineTimestamp":"t","baselineStats":{"meanMs":1}}"#,
        )
        .unwrap();

        let err = store.load("old").expect_err("legacy format is rejected");
        assert!(matches!(err, Error::LegacyBaseline { .. }));
        assert!(err.to_string().contains("--save-baseline"));
    }

    #[test]
    fn list_is_sorted_and_ignores_non_json() {
        let dir = tempdir();
        let store = BaselineStore::new(&dir);
        store
            .save(&Baseline::capture("beta", "t", &stats(1.0, 1.0, 1.0, 1.0)))
            .unwrap();
        store
            .save(&Baseline::capture("alpha", "t", &stats(1.0, 1.0, 1.0, 1.0)))
            .unwrap();
        std::fs::write(dir.join("notes.txt"), "ignore me").unwrap();

        assert_eq!(store.list().unwrap(), ["alpha", "beta"]);
    }

    #[test]
    fn listing_a_missing_directory_is_empty_not_an_error() {
        let store = BaselineStore::new(tempdir().join("does-not-exist"));
        assert!(store.list().unwrap().is_empty());
    }

    /// A unique temp directory for one test, removed when the process exits.
    /// Avoids a `tempfile` dependency for the handful of tests that touch disk.
    fn tempdir() -> PathBuf {
        use std::sync::atomic::{AtomicU32, Ordering};
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let dir =
            std::env::temp_dir().join(format!("gauntlet-baseline-test-{}-{n}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        dir
    }
}
