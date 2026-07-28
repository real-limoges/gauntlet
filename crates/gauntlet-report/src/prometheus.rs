//! Prometheus exposition rendering, and the reporter that writes or pushes it.
//!
//! See the crate docs for the metric-naming, `# TYPE`, and PUT-versus-POST rules
//! this module implements.

use std::path::PathBuf;

use async_trait::async_trait;
use gauntlet_stats::{BayesianComparison, BenchmarkStats};

use crate::error::{Error, Result};
use crate::format::{prom_label_escape, prom_name_sanitize};
use crate::markdown::{append, write_new};
use crate::model::BenchmarkReport;
use crate::{RegressionResult, Reporter};

/// Content type for the Prometheus text exposition format, version 0.0.4.
const EXPOSITION_CONTENT_TYPE: &str = "text/plain; version=0.0.4; charset=utf-8";

/// The exposition text for a finished benchmark.
pub fn benchmark_metrics(report: &BenchmarkReport) -> String {
    let mut out = String::new();

    if !report.targets.is_empty() {
        out.push_str(&family(
            "gauntlet_latency_milliseconds",
            "Request latency statistics for one target, in milliseconds.",
            report
                .targets
                .iter()
                .flat_map(|t| latency_samples(&t.name, &t.stats))
                .collect(),
        ));
        out.push_str(&family(
            "gauntlet_requests",
            "Requests issued during the run, by outcome.",
            report
                .targets
                .iter()
                .flat_map(|t| request_samples(&t.name, &t.stats))
                .collect(),
        ));
    }

    if !report.comparisons.is_empty() {
        out.push_str(&family(
            "gauntlet_comparison",
            "Pairwise Bayesian comparison of a candidate (b) against a primary (a).",
            report
                .comparisons
                .iter()
                .flat_map(|p| comparison_samples(&p.a, &p.b, &p.comparison))
                .collect(),
        ));
    }

    if report.has_validation() {
        let (total, failed) = report.validation_totals();
        out.push_str(&family(
            "gauntlet_validation",
            "Response validations performed during the run, by outcome.",
            vec![
                sample(&[("outcome", "total")], total as f64),
                sample(&[("outcome", "failed")], failed as f64),
            ],
        ));
    }

    out
}

/// The exposition text for a baseline regression check.
/// `gauntlet_regression_passed` is the series to alert on; the per-metric ones
/// show *which* metric moved.
pub fn regression_metrics(result: &RegressionResult) -> String {
    let mut out = family(
        "gauntlet_regression_passed",
        "1 when every metric stayed within its threshold, 0 otherwise.",
        vec![sample(&[], if result.passed { 1.0 } else { 0.0 })],
    );

    if !result.metrics.is_empty() {
        out.push_str(&family(
            "gauntlet_regression",
            "Per-metric baseline comparison: relative change, threshold, and verdict.",
            result
                .metrics
                .iter()
                .flat_map(|m| {
                    [
                        sample(&[("metric", &m.name), ("kind", "change")], m.change),
                        sample(&[("metric", &m.name), ("kind", "threshold")], m.threshold),
                        sample(
                            &[("metric", &m.name), ("kind", "regressed")],
                            if m.regressed { 1.0 } else { 0.0 },
                        ),
                        sample(&[("metric", &m.name), ("kind", "baseline_ms")], m.baseline),
                        sample(&[("metric", &m.name), ("kind", "current_ms")], m.current),
                    ]
                })
                .collect(),
        ));
    }

    out
}

/// One sample: its label set, already escaped and rendered, and its value.
struct Sample {
    labels: String,
    value: f64,
}

/// Build a sample from label pairs. Label *names* are fixed literals here;
/// label *values* are untrusted (target names, metric names from config) and
/// are escaped.
fn sample(labels: &[(&str, &str)], value: f64) -> Sample {
    let rendered = if labels.is_empty() {
        String::new()
    } else {
        let pairs: Vec<String> = labels
            .iter()
            .map(|(name, value)| format!("{name}=\"{}\"", prom_label_escape(value)))
            .collect();
        format!("{{{}}}", pairs.join(","))
    };
    Sample {
        labels: rendered,
        value,
    }
}

/// Render one metric family: `# HELP`, one `# TYPE`, then its samples. Every
/// family is a gauge — see the crate docs.
fn family(name: &str, help: &str, samples: Vec<Sample>) -> String {
    let mut out = format!(
        "# HELP {name} {}\n# TYPE {name} gauge\n",
        help.replace('\\', "\\\\").replace('\n', "\\n")
    );
    for s in samples {
        out.push_str(&format!("{name}{} {}\n", s.labels, value(s.value)));
    }
    out
}

/// Prometheus spells the non-finite values `NaN`, `+Inf`, `-Inf`; Rust's
/// `Display` spells them `NaN`, `inf`, `-inf`, which parsers reject. An empty
/// target produces `NaN` percentiles, so this is reachable, not theoretical.
fn value(v: f64) -> String {
    if v.is_nan() {
        "NaN".to_string()
    } else if v.is_infinite() {
        if v.is_sign_positive() {
            "+Inf".to_string()
        } else {
            "-Inf".to_string()
        }
    } else {
        format!("{v}")
    }
}

fn latency_samples(target: &str, s: &BenchmarkStats) -> Vec<Sample> {
    [
        ("mean", s.mean_ms),
        ("stddev", s.std_dev_ms),
        ("min", s.min_ms),
        ("max", s.max_ms),
        ("p50", s.p50_ms),
        ("p95", s.p95_ms),
        ("p99", s.p99_ms),
        ("es", s.es_ms),
    ]
    .into_iter()
    .map(|(stat, v)| sample(&[("target", target), ("stat", stat)], v))
    .collect()
}

fn request_samples(target: &str, s: &BenchmarkStats) -> Vec<Sample> {
    vec![
        sample(
            &[("target", target), ("outcome", "success")],
            s.count_success as f64,
        ),
        sample(
            &[("target", target), ("outcome", "failure")],
            s.count_failure as f64,
        ),
    ]
}

fn comparison_samples(a: &str, b: &str, c: &BayesianComparison) -> Vec<Sample> {
    let mut samples: Vec<Sample> = [
        ("prob_b_faster", c.prob_b_faster_than_a),
        ("prob_single_request_faster", c.prob_single_request_faster),
        ("prob_b_less_jittery", c.prob_b_less_jittery),
        ("cohens_d", c.effect_size),
        ("mean_diff_ms", c.mean_difference),
        ("relative_effect_pct", c.relative_effect),
    ]
    .into_iter()
    .map(|(metric, v)| sample(&[("a", a), ("b", b), ("metric", metric)], v))
    .collect();

    if let Some(emd) = c.emd {
        samples.push(sample(&[("a", a), ("b", b), ("metric", "emd_ms")], emd));
    }
    samples
}

/// Where a pushgateway lives and under which job name.
#[derive(Clone, Debug)]
struct PushTarget {
    url: String,
    job: String,
}

impl PushTarget {
    /// `<url>/metrics/job/<job>`. The job name is sanitized rather than
    /// percent-encoded: it is both a URL path segment and a Prometheus label,
    /// and only a name that survives both unchanged is worth accepting.
    fn endpoint(&self) -> String {
        format!(
            "{}/metrics/job/{}",
            self.url.trim_end_matches('/'),
            prom_name_sanitize(&self.job)
        )
    }
}

/// Writes exposition text to a file, pushes it to a pushgateway, or both. A
/// failed push is an `Error::Push`, never a swallowed warning.
#[derive(Debug)]
pub struct PrometheusReporter {
    path: Option<PathBuf>,
    push: Option<PushTarget>,
}

impl PrometheusReporter {
    /// Write exposition text to `path` (e.g. a textfile-collector directory).
    pub fn to_file(path: impl Into<PathBuf>) -> Self {
        Self {
            path: Some(path.into()),
            push: None,
        }
    }

    /// Push exposition text to a pushgateway under `job`.
    pub fn to_pushgateway(url: impl Into<String>, job: impl Into<String>) -> Self {
        Self {
            path: None,
            push: Some(PushTarget {
                url: url.into(),
                job: job.into(),
            }),
        }
    }

    pub fn with_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub fn with_pushgateway(mut self, url: impl Into<String>, job: impl Into<String>) -> Self {
        self.push = Some(PushTarget {
            url: url.into(),
            job: job.into(),
        });
        self
    }

    /// Emit one document. `fresh` replaces the file (a new run's values
    /// supersede the last); a follow-up regression check appends, since its
    /// metric families are disjoint from the benchmark's.
    async fn emit(&self, body: &str, fresh: bool) -> Result<()> {
        if let Some(path) = &self.path {
            if fresh {
                write_new(path, body)?;
            } else {
                append(path, body)?;
            }
        }
        if let Some(target) = &self.push {
            self.push_to(target, body, fresh).await?;
        }
        Ok(())
    }

    /// `fresh` picks the verb: PUT to replace the grouping key, POST to add to
    /// it. See the crate docs for why using PUT for both loses data.
    async fn push_to(&self, target: &PushTarget, body: &str, fresh: bool) -> Result<()> {
        let endpoint = target.endpoint();
        // The client is built per push rather than held in the struct: this
        // fires once or twice per process, and building it lazily keeps the
        // reporter constructible outside a tokio runtime.
        let client = reqwest::Client::new();
        let request = if fresh {
            client.put(&endpoint)
        } else {
            client.post(&endpoint)
        };
        let response = request
            .header("Content-Type", EXPOSITION_CONTENT_TYPE)
            .body(body.to_string())
            .send()
            .await
            .map_err(|source| Error::Push {
                url: endpoint.clone(),
                source,
            })?;

        response.error_for_status().map_err(|source| Error::Push {
            url: endpoint,
            source,
        })?;
        Ok(())
    }
}

#[async_trait]
impl Reporter for PrometheusReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> Result<()> {
        self.emit(&benchmark_metrics(report), true).await
    }

    /// Regressions are rendered: `gauntlet_regression_passed` is the series an
    /// alert rule fires on, which is the whole reason to push at all.
    async fn on_regression(&self, result: &RegressionResult) -> Result<()> {
        self.emit(&regression_metrics(result), false).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::baseline::MetricRegression;
    use crate::model::{PairComparison, TargetReport};

    fn stats(mean: f64) -> BenchmarkStats {
        BenchmarkStats {
            mean_ms: mean,
            p50_ms: mean,
            count_success: 100,
            count_failure: 2,
            ..Default::default()
        }
    }

    fn lines(text: &str) -> Vec<&str> {
        text.lines().collect()
    }

    #[test]
    fn each_family_declares_help_and_type_exactly_once() {
        let report = BenchmarkReport {
            targets: vec![
                TargetReport::new("a", stats(1.0)),
                TargetReport::new("b", stats(2.0)),
            ],
            comparisons: Vec::new(),
        };
        let text = benchmark_metrics(&report);

        assert_eq!(
            text.matches("# HELP gauntlet_latency_milliseconds").count(),
            1
        );
        assert_eq!(
            text.matches("# TYPE gauntlet_latency_milliseconds").count(),
            1
        );
        assert_eq!(text.matches("# TYPE gauntlet_requests ").count(), 1);
        // Two targets share the family, distinguished only by their labels.
        assert_eq!(
            text.matches("gauntlet_latency_milliseconds{target=\"a\"")
                .count(),
            8
        );
    }

    #[test]
    fn target_names_are_labels_not_metric_name_fragments() {
        let report = BenchmarkReport::single(TargetReport::new("api-v2", stats(12.5)));
        let text = benchmark_metrics(&report);

        assert!(text.contains(r#"gauntlet_latency_milliseconds{target="api-v2",stat="mean"} 12.5"#));
        assert!(
            !text.contains("gauntlet_api"),
            "the Haskell name-mangled form must not come back: {text}"
        );
    }

    #[test]
    fn a_quote_in_a_target_name_cannot_break_the_label_set() {
        let report = BenchmarkReport::single(TargetReport::new(r#"a"b\c"#, stats(1.0)));
        let text = benchmark_metrics(&report);

        assert!(text.contains(r#"{target="a\"b\\c",stat="mean"}"#));
    }

    #[test]
    fn request_counts_are_emitted_per_outcome() {
        let report = BenchmarkReport::single(TargetReport::new("api", stats(1.0)));
        let text = benchmark_metrics(&report);

        assert!(text.contains(r#"gauntlet_requests{target="api",outcome="success"} 100"#));
        assert!(text.contains(r#"gauntlet_requests{target="api",outcome="failure"} 2"#));
    }

    #[test]
    fn non_finite_values_use_prometheus_spelling() {
        let report = BenchmarkReport::single(TargetReport::new(
            "empty",
            BenchmarkStats {
                mean_ms: f64::NAN,
                min_ms: f64::INFINITY,
                max_ms: f64::NEG_INFINITY,
                ..Default::default()
            },
        ));
        let text = benchmark_metrics(&report);

        assert!(text.contains(r#"stat="mean"} NaN"#));
        assert!(text.contains(r#"stat="min"} +Inf"#));
        assert!(text.contains(r#"stat="max"} -Inf"#));
        assert!(!text.contains(" inf"), "Rust's spelling never leaks");
    }

    #[test]
    fn a_single_target_report_emits_no_comparison_or_validation_families() {
        let text = benchmark_metrics(&BenchmarkReport::single(TargetReport::new(
            "api",
            stats(1.0),
        )));

        assert!(!text.contains("gauntlet_comparison"));
        assert!(!text.contains("gauntlet_validation"));
    }

    #[test]
    fn comparisons_carry_both_target_names_as_labels() {
        let report = BenchmarkReport {
            targets: vec![
                TargetReport::new("base", stats(20.0)),
                TargetReport::new("candidate", stats(10.0)),
            ],
            comparisons: vec![PairComparison {
                a: "base".into(),
                b: "candidate".into(),
                comparison: BayesianComparison {
                    prob_b_faster_than_a: 0.94,
                    emd: Some(1.5),
                    ..Default::default()
                },
            }],
        };
        let text = benchmark_metrics(&report);

        assert!(text.contains(
            r#"gauntlet_comparison{a="base",b="candidate",metric="prob_b_faster"} 0.94"#
        ));
        assert!(text.contains(r#"metric="emd_ms"} 1.5"#));
    }

    #[test]
    fn the_emd_series_is_absent_when_it_was_not_computed() {
        let samples = comparison_samples("a", "b", &BayesianComparison::default());
        assert!(!samples.iter().any(|s| s.labels.contains("emd_ms")));
    }

    #[test]
    fn validation_totals_are_summed_across_targets() {
        use gauntlet_core::ValidationSummary as V;

        let mut a = TargetReport::new("a", stats(1.0));
        a.validation = vec![V {
            total: 10,
            failed: 1,
            errors: Vec::new(),
        }];
        let mut b = TargetReport::new("b", stats(2.0));
        b.validation = vec![V {
            total: 5,
            failed: 0,
            errors: Vec::new(),
        }];

        let text = benchmark_metrics(&BenchmarkReport {
            targets: vec![a, b],
            comparisons: Vec::new(),
        });

        assert!(text.contains(r#"gauntlet_validation{outcome="total"} 15"#));
        assert!(text.contains(r#"gauntlet_validation{outcome="failed"} 1"#));
    }

    #[test]
    fn an_empty_report_produces_no_output_rather_than_stray_headers() {
        assert_eq!(benchmark_metrics(&BenchmarkReport::default()), "");
    }

    #[test]
    fn every_line_is_a_comment_or_a_name_labels_value_triple() {
        let report = BenchmarkReport::single(TargetReport::new("api", stats(1.0)));
        for line in lines(&benchmark_metrics(&report)) {
            if line.starts_with('#') {
                continue;
            }
            let (_, value) = line.rsplit_once(' ').expect("a sample ends in a value");
            assert!(
                value.parse::<f64>().is_ok() || ["NaN", "+Inf", "-Inf"].contains(&value),
                "unparseable sample value in {line:?}"
            );
        }
    }

    #[test]
    fn a_passing_regression_check_still_reports_its_metrics() {
        let result = RegressionResult {
            baseline: "main".into(),
            metrics: vec![MetricRegression {
                name: "mean".into(),
                baseline: 10.0,
                current: 10.2,
                change: 0.02,
                threshold: 0.10,
                regressed: false,
            }],
            passed: true,
        };
        let text = regression_metrics(&result);

        assert!(text.contains("gauntlet_regression_passed 1"));
        assert!(text.contains(r#"gauntlet_regression{metric="mean",kind="change"} 0.02"#));
        assert!(text.contains(r#"gauntlet_regression{metric="mean",kind="regressed"} 0"#));
    }

    #[test]
    fn a_failing_regression_check_flips_the_alertable_series_to_zero() {
        let result = RegressionResult {
            baseline: "main".into(),
            metrics: Vec::new(),
            passed: false,
        };
        let text = regression_metrics(&result);

        assert!(text.contains("gauntlet_regression_passed 0"));
        assert!(
            !text.contains("# TYPE gauntlet_regression "),
            "no per-metric family when there are no metrics"
        );
    }

    #[test]
    fn the_pushgateway_endpoint_sanitizes_the_job_name() {
        let target = PushTarget {
            url: "http://gateway:9091/".into(),
            job: "load test/v2".into(),
        };
        assert_eq!(
            target.endpoint(),
            "http://gateway:9091/metrics/job/load_test_v2"
        );
    }

    #[tokio::test]
    async fn writing_to_a_file_replaces_the_benchmark_and_appends_the_regression() {
        let dir = std::env::temp_dir().join(format!("gauntlet-prom-{}", std::process::id()));
        let path = dir.join("nested").join("metrics.prom");
        let reporter = PrometheusReporter::to_file(&path);

        reporter
            .on_benchmark(&BenchmarkReport::single(TargetReport::new(
                "api",
                stats(1.0),
            )))
            .await
            .unwrap();
        reporter
            .on_regression(&RegressionResult {
                baseline: "main".into(),
                metrics: Vec::new(),
                passed: true,
            })
            .await
            .unwrap();

        let written = std::fs::read_to_string(&path).unwrap();
        std::fs::remove_dir_all(&dir).ok();

        assert!(written.contains("gauntlet_latency_milliseconds"));
        assert!(written.contains("gauntlet_regression_passed 1"));
        assert_eq!(
            written
                .matches("# TYPE gauntlet_latency_milliseconds")
                .count(),
            1
        );
    }

    #[tokio::test]
    async fn an_unreachable_pushgateway_surfaces_as_a_push_error() {
        // Port 1 refuses connections immediately; no network is required.
        let reporter = PrometheusReporter::to_pushgateway("http://127.0.0.1:1", "gauntlet");
        let err = reporter
            .on_benchmark(&BenchmarkReport::single(TargetReport::new(
                "api",
                stats(1.0),
            )))
            .await
            .expect_err("the failed push is not swallowed");

        match err {
            Error::Push { url, .. } => {
                assert_eq!(url, "http://127.0.0.1:1/metrics/job/gauntlet")
            }
            other => panic!("expected Error::Push, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn a_reporter_with_no_sink_configured_is_a_no_op() {
        let reporter = PrometheusReporter {
            path: None,
            push: None,
        };
        assert!(reporter
            .on_benchmark(&BenchmarkReport::default())
            .await
            .is_ok());
    }
}
