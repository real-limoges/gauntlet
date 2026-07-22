//! `gauntlet-report` — reporter backends and baselines.
//!
//! A [`Reporter`] is a sink for two events: a finished benchmark and a baseline
//! regression check. [`MultiReporter`] fans both out to a set of backends, so
//! output formats compose — the CLI assembles the active set from flags and the
//! rest of the pipeline is unaware of which are enabled.
//!
//! Adding an output format means implementing `Reporter` and adding it to the
//! CLI's reporter list. Nothing else changes.
//!
//! This crate depends only on `gauntlet-core` and `gauntlet-stats`: its input is
//! the plain [`BenchmarkReport`] data model, never a live run. Adapting the
//! engine's `BenchmarkRun` into a report is CLI glue (M6). See ADR `M4-report`.

pub mod baseline;
pub mod chart;
pub mod ci;
pub mod error;
pub mod format;
pub mod html;
pub mod junit;
pub mod markdown;
pub mod model;
pub mod prometheus;
pub mod terminal;

use async_trait::async_trait;

pub use baseline::{
    compare_to_baseline, Baseline, BaselineStore, MetricRegression, RegressionResult,
    RegressionThresholds, RunOutcome, StatsSnapshot, BASELINE_SCHEMA_VERSION, DEFAULT_BASELINE_DIR,
};
pub use chart::{ChartKind, ChartReporter};
pub use ci::{detect_ci, CiMode, CiReporter};
pub use error::{Error, Result};
pub use html::HtmlReporter;
pub use junit::JUnitReporter;
pub use markdown::MarkdownReporter;
pub use model::{BenchmarkReport, PairComparison, Sample, TargetReport};
pub use prometheus::PrometheusReporter;
pub use terminal::TerminalReporter;

/// A backend that turns benchmark events into output.
///
/// Both methods default to doing nothing, so a backend implements only the
/// events it has an opinion about — the CI reporter, for instance, only cares
/// about regressions.
#[async_trait]
pub trait Reporter: Send + Sync {
    /// Called once when a benchmark run finishes.
    async fn on_benchmark(&self, _report: &BenchmarkReport) -> Result<()> {
        Ok(())
    }

    /// Called once after a baseline comparison, whether or not it passed.
    async fn on_regression(&self, _result: &RegressionResult) -> Result<()> {
        Ok(())
    }
}

/// Fans every event out to a set of reporters, in order.
///
/// One backend failing does not rob the others of the event: every reporter is
/// invoked, and the first error is returned once they all have been. A failed
/// HTML write should not cost you the terminal summary.
#[derive(Default)]
pub struct MultiReporter {
    reporters: Vec<Box<dyn Reporter>>,
}

impl MultiReporter {
    pub fn new(reporters: Vec<Box<dyn Reporter>>) -> Self {
        Self { reporters }
    }

    pub fn push(&mut self, reporter: Box<dyn Reporter>) -> &mut Self {
        self.reporters.push(reporter);
        self
    }

    pub fn is_empty(&self) -> bool {
        self.reporters.is_empty()
    }

    pub fn len(&self) -> usize {
        self.reporters.len()
    }
}

#[async_trait]
impl Reporter for MultiReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> Result<()> {
        let mut first_error = None;
        for reporter in &self.reporters {
            if let Err(e) = reporter.on_benchmark(report).await {
                first_error.get_or_insert(e);
            }
        }
        match first_error {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    async fn on_regression(&self, result: &RegressionResult) -> Result<()> {
        let mut first_error = None;
        for reporter in &self.reporters {
            if let Err(e) = reporter.on_regression(result).await {
                first_error.get_or_insert(e);
            }
        }
        match first_error {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    use super::*;

    #[derive(Default)]
    struct Counter {
        benchmarks: AtomicUsize,
        regressions: AtomicUsize,
    }

    struct Counting(Arc<Counter>);

    #[async_trait]
    impl Reporter for Counting {
        async fn on_benchmark(&self, _report: &BenchmarkReport) -> Result<()> {
            self.0.benchmarks.fetch_add(1, Ordering::Relaxed);
            Ok(())
        }
        async fn on_regression(&self, _result: &RegressionResult) -> Result<()> {
            self.0.regressions.fetch_add(1, Ordering::Relaxed);
            Ok(())
        }
    }

    /// Only implements `on_benchmark`, and fails — exercises both the defaulted
    /// method and the error path.
    struct Failing;

    #[async_trait]
    impl Reporter for Failing {
        async fn on_benchmark(&self, _report: &BenchmarkReport) -> Result<()> {
            Err(Error::Chart("boom".into()))
        }
    }

    fn regression() -> RegressionResult {
        RegressionResult {
            baseline: "main".into(),
            metrics: Vec::new(),
            passed: true,
        }
    }

    #[tokio::test]
    async fn fans_every_event_out_to_every_reporter() {
        let counter = Arc::new(Counter::default());
        let multi = MultiReporter::new(vec![
            Box::new(Counting(counter.clone())),
            Box::new(Counting(counter.clone())),
        ]);

        multi
            .on_benchmark(&BenchmarkReport::default())
            .await
            .unwrap();
        multi.on_regression(&regression()).await.unwrap();

        assert_eq!(counter.benchmarks.load(Ordering::Relaxed), 2);
        assert_eq!(counter.regressions.load(Ordering::Relaxed), 2);
    }

    #[tokio::test]
    async fn one_failing_backend_does_not_starve_the_others() {
        let counter = Arc::new(Counter::default());
        let multi =
            MultiReporter::new(vec![Box::new(Failing), Box::new(Counting(counter.clone()))]);

        let result = multi.on_benchmark(&BenchmarkReport::default()).await;
        assert!(result.is_err(), "the failure is still surfaced");
        assert_eq!(
            counter.benchmarks.load(Ordering::Relaxed),
            1,
            "the healthy reporter still received the event"
        );
    }

    #[tokio::test]
    async fn defaulted_methods_are_no_ops() {
        // `Failing` does not implement `on_regression`; the default must succeed.
        let multi = MultiReporter::new(vec![Box::new(Failing)]);
        assert!(multi.on_regression(&regression()).await.is_ok());
    }

    #[tokio::test]
    async fn an_empty_multi_reporter_is_harmless() {
        let multi = MultiReporter::default();
        assert!(multi.is_empty());
        assert!(multi
            .on_benchmark(&BenchmarkReport::default())
            .await
            .is_ok());
    }
}
