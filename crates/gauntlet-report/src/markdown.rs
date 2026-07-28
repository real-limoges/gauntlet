//! Markdown rendering, and the reporter that writes it to a file. The renderers
//! are pure `String` producers, so the CI reporter can reuse them for a GitHub
//! Step Summary.

use std::path::{Path, PathBuf};

use async_trait::async_trait;
use gauntlet_core::ValidationSummary;
use gauntlet_stats::{BayesianComparison, BenchmarkStats};

use crate::error::{Error, Result};
use crate::format::{md_cell_escape, ms, prob_pct, signed_pct};
use crate::model::BenchmarkReport;
use crate::{RegressionResult, Reporter};

/// The full markdown document for a finished benchmark.
pub fn benchmark_report(report: &BenchmarkReport) -> String {
    let mut out = String::new();

    if report.is_single() {
        match report.targets.first() {
            Some(target) => {
                out.push_str(&format!(
                    "## Benchmark Report: {}\n\n### Statistics\n\n",
                    md_cell_escape(&target.name)
                ));
                out.push_str(&stats_table(&target.stats));
            }
            None => out.push_str("## Benchmark Report\n\nNo targets were benchmarked.\n"),
        }
    } else {
        out.push_str("## Benchmark Report\n\n### Ranking (by mean latency)\n\n");
        out.push_str(&ranking_table(report));

        for pair in &report.comparisons {
            out.push_str("\n---\n\n");
            out.push_str(&pair_section(
                &pair.a,
                &pair.b,
                &report.stats_for(&pair.a),
                &report.stats_for(&pair.b),
                &pair.comparison,
            ));
        }
    }

    out.push_str(&validation_report(report));
    out
}

/// The markdown document for a baseline regression check.
pub fn regression_report(result: &RegressionResult) -> String {
    let status = if result.passed {
        "**Status:** PASSED"
    } else {
        "**Status:** FAILED — regression detected"
    };

    let mut out = format!(
        "## Regression Check vs `{}`\n\n{status}\n\n### Metrics\n\n\
         | Metric | Baseline | Current | Change | Threshold | Status |\n\
         |--------|----------|---------|--------|-----------|--------|\n",
        md_cell_escape(&result.baseline)
    );

    for m in &result.metrics {
        out.push_str(&format!(
            "| {} | {} ms | {} ms | {} | {:.0}% | {} |\n",
            md_cell_escape(&m.name),
            ms(m.baseline),
            ms(m.current),
            signed_pct(m.change),
            m.threshold * 100.0,
            if m.regressed { "FAIL" } else { "PASS" },
        ));
    }

    out.push('\n');
    if result.passed {
        out.push_str("All metrics within acceptable thresholds.\n");
    } else {
        out.push_str(
            "**Action required:** performance regression detected.\n\nRegressed metrics:\n",
        );
        for m in result.regressed() {
            out.push_str(&format!(
                "- **{}**: increased by {} (threshold: {:.0}%)\n",
                md_cell_escape(&m.name),
                signed_pct(m.change),
                m.threshold * 100.0,
            ));
        }
    }
    out
}

/// The validation section, or nothing when no endpoint declared assertions.
pub fn validation_report(report: &BenchmarkReport) -> String {
    if !report.has_validation() {
        return String::new();
    }

    let mut out = String::from(
        "\n## Validation Results\n\n\
         | Total Validated | Passed | Failed |\n\
         |-----------------|--------|--------|\n",
    );

    for summary in report.validations() {
        out.push_str(&format!(
            "| {} | {} | {} |\n",
            summary.total,
            summary.total.saturating_sub(summary.failed),
            summary.failed,
        ));
    }

    let errors: Vec<&ValidationSummary> = report
        .validations()
        .filter(|s| !s.errors.is_empty())
        .collect();
    if !errors.is_empty() {
        out.push_str("\n**Validation errors:**\n");
        for summary in errors {
            for err in &summary.errors {
                out.push_str(&format!(
                    "- `{}`: {}\n",
                    md_cell_escape(&err.field),
                    md_cell_escape(&err.message),
                ));
            }
        }
    }
    out
}

fn ranking_table(report: &BenchmarkReport) -> String {
    let mut out = String::from(
        "| # | Target | Mean | p50 | p95 | p99 |\n|---|--------|------|-----|-----|-----|\n",
    );
    for (i, target) in report.ranked().iter().enumerate() {
        let s = &target.stats;
        out.push_str(&format!(
            "| {} | {} | {} ms | {} ms | {} ms | {} ms |\n",
            i + 1,
            md_cell_escape(&target.name),
            ms(s.mean_ms),
            ms(s.p50_ms),
            ms(s.p95_ms),
            ms(s.p99_ms),
        ));
    }
    out
}

fn pair_section(
    name_a: &str,
    name_b: &str,
    stats_a: &BenchmarkStats,
    stats_b: &BenchmarkStats,
    bayes: &BayesianComparison,
) -> String {
    let (a, b) = (md_cell_escape(name_a), md_cell_escape(name_b));
    let mut out = format!("## Benchmark Report: {b} vs {a}\n\n### Statistics\n\n#### {a}\n\n");
    out.push_str(&stats_table(stats_a));
    out.push_str(&format!("\n#### {b}\n\n"));
    out.push_str(&stats_table(stats_b));
    out.push_str("\n### Bayesian Analysis\n\n");
    out.push_str(&bayes_table(&a, &b, bayes));
    out
}

fn stats_table(s: &BenchmarkStats) -> String {
    let rows = [
        ("Total Requests", s.total_requests.to_string()),
        ("Success", s.count_success.to_string()),
        ("Failure", s.count_failure.to_string()),
        ("Mean", format!("{} ms", ms(s.mean_ms))),
        ("Std Dev", format!("{} ms", ms(s.std_dev_ms))),
        ("Min", format!("{} ms", ms(s.min_ms))),
        ("Max", format!("{} ms", ms(s.max_ms))),
        ("p50", format!("{} ms", ms(s.p50_ms))),
        ("p95", format!("{} ms", ms(s.p95_ms))),
        ("p99", format!("{} ms", ms(s.p99_ms))),
        ("ES(p99)", format!("{} ms", ms(s.es_ms))),
    ];

    let mut out = String::from("| Metric | Value |\n|--------|-------|\n");
    for (label, value) in rows {
        out.push_str(&format!("| {label} | {value} |\n"));
    }
    out
}

fn bayes_table(a: &str, b: &str, bayes: &BayesianComparison) -> String {
    let pct = |p: &gauntlet_stats::PercentileComparison| {
        format!(
            "{:+.2} ms [{:+.2}, {:+.2}]",
            p.pct_difference, p.pct_credible_lower, p.pct_credible_upper
        )
    };

    let mut rows = vec![
        (
            format!("P({b} faster than {a}, means)"),
            prob_pct(bayes.prob_b_faster_than_a),
        ),
        (
            format!("P({b} faster, single request)"),
            prob_pct(bayes.prob_single_request_faster),
        ),
        (
            format!("P({b} less jittery)"),
            prob_pct(bayes.prob_b_less_jittery),
        ),
        (
            format!("Mean difference ({b} - {a})"),
            format!("{:+.2} ms", bayes.mean_difference),
        ),
        (
            "95% credible interval".to_string(),
            format!(
                "[{:+.2} ms, {:+.2} ms]",
                bayes.credible_interval_lower, bayes.credible_interval_upper
            ),
        ),
        (
            "Effect size (Cohen's d)".to_string(),
            format!("{:.3}", bayes.effect_size),
        ),
        (
            "Relative effect".to_string(),
            format!("{:+.1}%", bayes.relative_effect),
        ),
        ("p95 difference".to_string(), pct(&bayes.p95_comparison)),
        ("p99 difference".to_string(), pct(&bayes.p99_comparison)),
    ];

    if let Some(emd) = bayes.emd {
        rows.push(("Earth Mover's Distance".to_string(), format!("{emd:.3} ms")));
    }

    let mut out = String::from("| Metric | Value |\n|--------|-------|\n");
    for (label, value) in rows {
        out.push_str(&format!("| {label} | {value} |\n"));
    }
    out
}

/// Writes markdown reports to a file. The benchmark report replaces the file's
/// contents; a subsequent regression report is appended, so one file holds the
/// whole run.
#[derive(Debug)]
pub struct MarkdownReporter {
    path: PathBuf,
}

impl MarkdownReporter {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into() }
    }
}

#[async_trait]
impl Reporter for MarkdownReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> Result<()> {
        write_new(&self.path, &benchmark_report(report))
    }

    async fn on_regression(&self, result: &RegressionResult) -> Result<()> {
        append(&self.path, &format!("\n{}", regression_report(result)))
    }
}

pub(crate) fn write_new(path: &Path, contents: &str) -> Result<()> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).map_err(|source| Error::write(parent, source))?;
        }
    }
    std::fs::write(path, contents).map_err(|source| Error::write(path, source))
}

pub(crate) fn append(path: &Path, contents: &str) -> Result<()> {
    use std::io::Write;

    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).map_err(|source| Error::write(parent, source))?;
        }
    }
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .map_err(|source| Error::write(path, source))?;
    file.write_all(contents.as_bytes())
        .map_err(|source| Error::write(path, source))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::baseline::MetricRegression;
    use crate::model::TargetReport;

    fn metric(name: &str, baseline: f64, current: f64, threshold: f64) -> MetricRegression {
        let change = (current - baseline) / baseline;
        MetricRegression {
            name: name.into(),
            baseline,
            current,
            change,
            threshold,
            regressed: change > threshold,
        }
    }

    #[test]
    fn single_target_report_has_no_ranking_or_comparison() {
        let report = BenchmarkReport::single(TargetReport::new(
            "api",
            BenchmarkStats {
                mean_ms: 12.5,
                ..Default::default()
            },
        ));
        let md = benchmark_report(&report);

        assert!(md.starts_with("## Benchmark Report: api"));
        assert!(md.contains("| Mean | 12.50 ms |"));
        assert!(!md.contains("Ranking"));
        assert!(!md.contains("Bayesian"));
    }

    #[test]
    fn multi_target_report_ranks_fastest_first() {
        let report = BenchmarkReport {
            targets: vec![
                TargetReport::new(
                    "slow",
                    BenchmarkStats {
                        mean_ms: 30.0,
                        ..Default::default()
                    },
                ),
                TargetReport::new(
                    "fast",
                    BenchmarkStats {
                        mean_ms: 10.0,
                        ..Default::default()
                    },
                ),
            ],
            comparisons: Vec::new(),
        };
        let md = benchmark_report(&report);
        let fast = md.find("| 1 | fast").expect("fast ranks first");
        let slow = md.find("| 2 | slow").expect("slow ranks second");
        assert!(fast < slow);
    }

    #[test]
    fn emd_row_appears_only_when_present() {
        let with_emd = bayes_table(
            "a",
            "b",
            &BayesianComparison {
                emd: Some(1.234),
                ..Default::default()
            },
        );
        assert!(with_emd.contains("| Earth Mover's Distance | 1.234 ms |"));

        let without = bayes_table("a", "b", &BayesianComparison::default());
        assert!(!without.contains("Earth Mover's Distance"));
    }

    #[test]
    fn pipe_in_a_target_name_cannot_break_the_table() {
        let report = BenchmarkReport::single(TargetReport::new("a|b", BenchmarkStats::default()));
        assert!(benchmark_report(&report).contains("a\\|b"));
    }

    #[test]
    fn passing_regression_report_says_so_and_lists_no_actions() {
        let result = RegressionResult {
            baseline: "main".into(),
            metrics: vec![metric("mean", 10.0, 10.2, 0.10)],
            passed: true,
        };
        let md = regression_report(&result);

        assert!(md.contains("**Status:** PASSED"));
        assert!(md.contains("| mean | 10.00 ms | 10.20 ms | +2.0% | 10% | PASS |"));
        assert!(md.contains("All metrics within acceptable thresholds."));
        assert!(!md.contains("Action required"));
    }

    #[test]
    fn failing_regression_report_lists_only_the_regressed_metrics() {
        let result = RegressionResult {
            baseline: "main".into(),
            metrics: vec![
                metric("mean", 10.0, 10.1, 0.10),
                metric("p99", 10.0, 20.0, 0.15),
            ],
            passed: false,
        };
        let md = regression_report(&result);

        assert!(md.contains("FAILED — regression detected"));
        assert!(md.contains("**Action required:**"));
        assert!(md.contains("- **p99**: increased by +100.0% (threshold: 15%)"));
        assert!(
            !md.contains("- **mean**:"),
            "a metric within threshold is not listed as an action"
        );
    }

    #[test]
    fn validation_section_is_omitted_when_nothing_was_validated() {
        let report = BenchmarkReport::single(TargetReport::new("api", BenchmarkStats::default()));
        assert_eq!(validation_report(&report), "");
    }

    #[test]
    fn validation_section_lists_errors() {
        use gauntlet_core::ValidationError;

        let mut target = TargetReport::new("api", BenchmarkStats::default());
        target.validation = vec![ValidationSummary {
            total: 100,
            failed: 2,
            errors: vec![ValidationError {
                field: "$.id".into(),
                message: "expected 1, got 2".into(),
            }],
        }];
        let report = BenchmarkReport::single(target);
        let md = validation_report(&report);

        assert!(md.contains("| 100 | 98 | 2 |"));
        assert!(md.contains("- `$.id`: expected 1, got 2"));
    }
}
