//! `gauntlet-report` — reporter backends and baselines.
//!
//! A [`Reporter`] is a sink for two events: a finished benchmark and a baseline
//! regression check. [`MultiReporter`] fans both out to a set of backends, so
//! output formats compose — the CLI assembles the active set from flags and the
//! rest of the pipeline is unaware of which are enabled. Adding a format means
//! implementing `Reporter` and adding it to that list; nothing else changes.
//!
//! Both trait methods default to no-ops, so a backend implements only the events
//! it has an opinion about. One failing backend never robs the others: every
//! reporter is invoked and the first error is returned once they all have been,
//! because a failed HTML write should not cost you the terminal summary.
//!
//! This crate depends only on `gauntlet-core` and `gauntlet-stats`. Its input is
//! the plain [`BenchmarkReport`] data model, never a live run; adapting the
//! engine's output into a report is CLI glue.
//!
//! # Untrusted strings must be escaped
//!
//! Target names, URLs, and validation messages come from config files and from
//! server responses. None of it is trusted to be markup-safe, and a target named
//! `a&b` is enough to produce XML no parser will accept. Every markup backend
//! **must** route interpolated strings through [`format::xml_escape`],
//! [`format::html_escape`], [`format::prom_label_escape`], or
//! [`format::md_cell_escape`] rather than interpolating directly.
//!
//! The same rule applies to anything that becomes a *filename*: `chart::slug`,
//! `format::prom_name_sanitize`, and the baseline store's name sanitizer all
//! exist so a name like `../../etc/passwd` cannot escape the directory it was
//! told to write into.
//!
//! # The markup backends
//!
//! **HTML** is one self-contained document: charts inline as `<svg>` and the
//! stylesheet as an inline `<style>`, so it renders correctly when opened
//! straight from a file manager over `file://`, where browsers refuse to load
//! subresources. A report that needs a web server to look right is a report
//! nobody looks at. There is no templating crate — one template with no user
//! extensibility does not earn a build-time step — so the renderers are `format!`
//! calls.
//!
//! **JUnit XML** is how a run shows up in a CI pipeline's test tab. Latency
//! numbers have no home in the schema, since there is no "metric" element, so
//! they ride as `<property>` entries on one synthetic `<testcase>` per target.
//! Only things that can genuinely pass or fail — validation, regression — emit
//! `<failure>`.
//!
//! **Markdown**, **terminal**, and the trace-analysis sections are pure `String`
//! producers with colour passed in. That makes them golden-testable without
//! capturing stdout, lets colour switch off for pipes and CI, and lets one layout
//! serve the markdown report, the HTML report, and a CI step summary.
//!
//! # Documents are rewritten, not appended
//!
//! A run that also checks a baseline delivers two events, and the structured
//! formats have exactly one root apiece. So the JUnit and HTML reporters retain
//! what they rendered and rewrite the whole file on the second event, rather than
//! appending a second `<?xml?>` prolog or a second `<!DOCTYPE>` to a finished
//! document — either of which yields a file no parser accepts. Markdown, having
//! no root element, genuinely can append.
//!
//! # Errors, and why reporters are fallible
//!
//! A failed artifact write must not vanish silently: in CI that is
//! indistinguishable from a clean run that produced no report. Every backend
//! returns [`Result`], and `MultiReporter` surfaces the first failure.
//!
//! The async signature is earned by exactly one backend: the Prometheus
//! pushgateway call is real network I/O.
//!
//! # The data model
//!
//! [`Sample`] is deliberately richer than a bare latency. Throughput, error rate,
//! and status mix are all time series *over outcomes*, and collapsing a run to
//! `Vec<f64>` throws away everything except successful latencies.
//!
//! [`Sample::is_success`] is **not** the same predicate as `latency_ms.is_some()`,
//! and the difference is intentional: a 500 is a *response*. It was served, it
//! took a measurable time, and that time belongs in the latency distribution. So
//! a 5xx counts as an error while still feeding the latency charts and
//! statistics. Only a transport failure — nothing came back — has no latency.
//!
//! Targets are held in *run* order. Ranking is a presentation concern, so
//! renderers that want it call [`BenchmarkReport::ranked`].
//!
//! # Charts
//!
//! Charts are drawn in-process with `plotters`, straight from
//! [`TargetReport::samples`]. [`chart::render`] is a pure `String` producer: it
//! hands back the SVG document rather than writing it, so the HTML reporter can
//! embed the same bytes inline instead of linking an external asset that
//! `file://` would refuse to load.
//!
//! [`ChartKind`] is an enum, so an unknown kind is a config error caught at parse
//! time rather than a failure at report time, after the benchmark has already
//! been paid for.
//!
//! `render` returns `Ok(None)` when there is nothing to plot, because an empty
//! pair of axes is worse than no chart. That covers no samples at all; no
//! *successful* samples for the kinds that plot latency (an all-failed target
//! still charts its error rate and status mix, which is the point of keeping
//! failures in the model); and a run with no measurable duration, where "requests
//! per second" is not a number. Non-finite values are dropped first, so one NaN
//! cannot collapse an axis.
//!
//! Individual kinds encode a few deliberate choices:
//!
//! - **Error rate** pins its y-axis to 0–100% rather than scaling to the data. An
//!   axis that silently rescales makes a 0.2% error rate look like an outage.
//! - **Throughput** counts failures. A target that answers fast because it is
//!   refusing everything has a throughput, and hiding it would flatter it.
//! - **The tail chart** plots percentile across and latency up. A CDF filtered to
//!   the top decile spends the whole x-axis on a handful of samples and squashes
//!   the percentile axis into a sliver; transposing puts p90…p100 on an even
//!   footing, which is the comparison anyone opening a tail chart is making.
//! - **Percentiles over time** use a window of a tenth of the run rather than a
//!   wall-clock constant, which would swallow a short run whole.
//! - **Status counts** group by class, not exact code: a run returning 200 and
//!   204 is not saying two different things, and one bar per code turns a noisy
//!   run into an unreadable comb. Colour is semantic there — 4xx and 5xx are red
//!   whichever target it is — because "which target" is already the caption.
//! - The canvas is left **unfilled**, so an embedded SVG inherits the page's
//!   light or dark surface.
//!
//! `ChartReporter` implements no `on_regression`: a regression result is summary
//! statistics with no sample vectors, so there is nothing to plot that the tables
//! do not already say.
//!
//! # Prometheus
//!
//! A benchmark is a one-shot job, not a long-lived process, so there is no
//! registry and no scrape endpoint: the values are formatted into exposition text
//! and either written to a file for a node exporter's textfile collector or
//! pushed to a gateway. That is why the `prometheus` crates do not fit — they
//! model a live process exporting its own metrics.
//!
//! Target names are **labels, not part of metric names**:
//! `gauntlet_latency_milliseconds{target="…",stat="mean"}`, not
//! `gauntlet_<target>_mean_ms`. Baking the name in makes every target a separate
//! metric family, which cannot be graphed or aggregated across and silently
//! creates a new name whenever a target is added.
//!
//! `# TYPE` is emitted exactly once per family — repeating it per sample is a
//! parse error in strict parsers — and every metric is a gauge, including
//! `gauntlet_requests`, because each push replaces the previous run's value
//! rather than adding to it.
//!
//! The push verb follows the same fresh/append distinction as the file sink, and
//! it matters more: **PUT replaces every metric under the grouping key**, so
//! pushing the regression document with PUT would delete the latency families the
//! benchmark push just wrote, leaving a dashboard with a verdict and nothing
//! behind it. POST replaces only the families present in the body.
//!
//! # Baselines
//!
//! A baseline is a named snapshot of one target's statistics in
//! `baselines/<name>.json`. A later run compares metric by metric, and any metric
//! past its threshold fails the run — which is what drives the exit-code contract
//! in [`RunOutcome`].
//!
//! The on-disk shape is [`StatsSnapshot`], a deliberate mirror of `BenchmarkStats`
//! rather than a serde derive on the stats type: `gauntlet-stats` is
//! dependency-light by design, and the file format should be free to evolve
//! separately from the in-memory type. The histogram is dropped, since nothing
//! reads it back and it dominates the file size.
//!
//! A zero baseline cannot produce a ratio, so zero-to-zero is treated as no
//! change and zero-to-anything as a 100% regression rather than infinity.

// Production code must not panic: an unwrap that fires mid-run destroys the
// whole measurement, and a benchmark that dies is worse than one reporting a
// clean error. `cfg(not(test))` scopes this to real code; inside `#[cfg(test)]`
// modules, panicking assertions and exact float comparisons are the point.
#![cfg_attr(
    not(test),
    deny(
        clippy::unwrap_used,
        clippy::panic,
        clippy::unreachable,
        clippy::panic_in_result_fn,
        clippy::float_cmp
    )
)]

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
#[derive(Default)]
pub struct MultiReporter {
    reporters: Vec<Box<dyn Reporter>>,
}

// `Reporter` is not `Debug` — requiring it would constrain every backend — so
// report the fan-out width, which is the runtime-interesting part anyway.
impl std::fmt::Debug for MultiReporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MultiReporter")
            .field("reporters", &self.reporters.len())
            .finish()
    }
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
