//! The post-run terminal summary — the headless printer, not the live view,
//! which is `gauntlet-tui`'s job.

use std::io::{IsTerminal, Write};

use async_trait::async_trait;
use gauntlet_stats::{BayesianComparison, BenchmarkStats};

use crate::error::{Error, Result};
use crate::format::ms;
use crate::model::BenchmarkReport;
use crate::{RegressionResult, Reporter};

/// ANSI styling, or the identity functions when colour is off.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Style {
    enabled: bool,
}

impl Style {
    pub const PLAIN: Style = Style { enabled: false };
    pub const COLOR: Style = Style { enabled: true };

    fn wrap(&self, code: &str, text: &str) -> String {
        if self.enabled {
            format!("\x1b[{code}m{text}\x1b[0m")
        } else {
            text.to_string()
        }
    }

    fn bold(&self, text: &str) -> String {
        self.wrap("1", text)
    }
    fn header(&self, text: &str) -> String {
        self.wrap("1;36", &format!("── {text} ──"))
    }
    fn green(&self, text: &str) -> String {
        self.wrap("32", text)
    }
    fn yellow(&self, text: &str) -> String {
        self.wrap("33", text)
    }
    fn red(&self, text: &str) -> String {
        self.wrap("1;31", text)
    }

    /// Probability colouring: green at ≥90%, yellow at ≥60%, plain below — a
    /// glanceable "is this conclusive?" signal.
    fn confidence(&self, probability: f64, text: &str) -> String {
        match probability {
            p if p >= 0.90 => self.green(text),
            p if p >= 0.60 => self.yellow(text),
            _ => text.to_string(),
        }
    }
}

/// Render the whole benchmark summary.
pub fn benchmark_summary(report: &BenchmarkReport, style: Style) -> String {
    let mut out = String::new();

    if report.is_single() {
        if let Some(target) = report.targets.first() {
            out.push_str(&format!(
                "\n{}\n",
                style.bold(&format!("({})", target.name))
            ));
            out.push_str(&stats_block(&target.stats));
        }
    } else {
        out.push_str(&format!(
            "\n{}\n\n",
            style.header("Ranking (by mean latency)")
        ));
        out.push_str(&ranking_table(report));

        for pair in &report.comparisons {
            out.push_str(&format!("\n{}\n", style.header("Pairwise Comparison")));
            out.push_str(&comparison_block(
                &pair.a,
                &pair.b,
                &report.stats_for(&pair.a),
                &report.stats_for(&pair.b),
                &pair.comparison,
                style,
            ));
        }
    }

    out.push_str(&validation_block(report, style));
    out
}

fn ranking_table(report: &BenchmarkReport) -> String {
    let mut out = format!(
        "{:<4}  {:<20}  {:>10}  {:>10}  {:>10}  {:>10}\n",
        "#", "Target", "Mean", "p50", "p95", "p99"
    );
    out.push_str(&format!("{}\n", "-".repeat(72)));

    for (i, target) in report.ranked().iter().enumerate() {
        let s = &target.stats;
        out.push_str(&format!(
            "{:<4}  {:<20}  {:>7} ms  {:>7} ms  {:>7} ms  {:>7} ms\n",
            i + 1,
            truncate(&target.name, 20),
            ms(s.mean_ms),
            ms(s.p50_ms),
            ms(s.p95_ms),
            ms(s.p99_ms),
        ));
    }
    out
}

fn comparison_block(
    name_a: &str,
    name_b: &str,
    stats_a: &BenchmarkStats,
    stats_b: &BenchmarkStats,
    bayes: &BayesianComparison,
    style: Style,
) -> String {
    let mut out = format!("\n{}\n", style.bold(&format!("({name_a})")));
    out.push_str(&stats_block(stats_a));
    out.push_str(&format!("\n{}\n", style.bold(&format!("({name_b})"))));
    out.push_str(&stats_block(stats_b));

    out.push_str(&format!("\n{}\n", style.header("Bayesian Analysis")));
    let pct = |p: f64| style.confidence(p, &format!("{:.2}%", p * 100.0));
    out.push_str(&format!(
        "P({name_b} faster than {name_a}, means): {}\n",
        pct(bayes.prob_b_faster_than_a)
    ));
    out.push_str(&format!(
        "P({name_b} faster, single request):     {}\n",
        pct(bayes.prob_single_request_faster)
    ));
    out.push_str(&format!(
        "P({name_b} less jittery):               {}\n",
        pct(bayes.prob_b_less_jittery)
    ));
    out.push_str(&format!(
        "Mean difference ({name_b} - {name_a}): {:+.2} ms\n",
        bayes.mean_difference
    ));
    out.push_str(&format!(
        "95% credible interval: [{:+.2} ms, {:+.2} ms]\n",
        bayes.credible_interval_lower, bayes.credible_interval_upper
    ));
    out.push_str(&format!(
        "Effect size (Cohen's d): {:.3}\n",
        bayes.effect_size
    ));
    out.push_str(&format!(
        "Relative effect: {:+.2}%\n",
        bayes.relative_effect
    ));

    out.push_str(&format!("\n{}\n", style.header("Tail Analysis")));
    for (label, p) in [
        ("P95", &bayes.p95_comparison),
        ("P99", &bayes.p99_comparison),
    ] {
        out.push_str(&format!(
            "{label} difference: {:+.2} ms [{:+.2}, {:+.2}]\n",
            p.pct_difference, p.pct_credible_lower, p.pct_credible_upper
        ));
        out.push_str(&format!(
            "{label} regression probability: {}\n",
            style.confidence(
                p.prob_pct_regression,
                &format!("{:.2}%", p.prob_pct_regression * 100.0)
            )
        ));
    }

    if let Some(emd) = bayes.emd {
        out.push_str(&format!("\n{}\n", style.header("Distribution")));
        out.push_str(&format!("Earth Mover's Distance: {emd:.3} ms\n"));
    }
    out
}

fn stats_block(s: &BenchmarkStats) -> String {
    let mut out = String::new();
    for (label, value) in [
        ("Mean", s.mean_ms),
        ("StdDev", s.std_dev_ms),
        ("p50", s.p50_ms),
        ("p95", s.p95_ms),
        ("p99", s.p99_ms),
        ("ES(p99)", s.es_ms),
        ("Min", s.min_ms),
        ("Max", s.max_ms),
    ] {
        out.push_str(&format!("  {label:<8} {:>8} ms\n", ms(value)));
    }
    out.push_str(&format!(
        "  {:<8} {} / {}\n",
        "Success", s.count_success, s.total_requests
    ));
    if !s.histogram.is_empty() {
        out.push('\n');
        out.push_str(&histogram(&s.histogram));
    }
    out
}

/// A horizontal bar chart of the latency distribution, scaled to the tallest
/// bin. Empty bins are skipped so a long sparse tail stays readable.
fn histogram(bins: &[(f64, usize)]) -> String {
    const MAX_BAR: usize = 40;

    let max_count = bins.iter().map(|(_, c)| *c).max().unwrap_or(0);
    if max_count == 0 {
        return String::new();
    }

    let decimals = label_decimals(bins);
    let mut out = String::from("  Distribution:\n");
    for (lower, count) in bins.iter().filter(|(_, c)| *c > 0) {
        let width = ((*count as f64 / max_count as f64) * MAX_BAR as f64).round() as usize;
        out.push_str(&format!(
            "  {lower:>8.decimals$} ms  {} {count}\n",
            "█".repeat(width)
        ));
    }
    out
}

/// Decimal places that keep adjacent bin labels distinct: a fixed single decimal
/// renders a sub-millisecond run as several identical rows, which reads as a bug.
fn label_decimals(bins: &[(f64, usize)]) -> usize {
    let width = bins
        .windows(2)
        .map(|w| w[1].0 - w[0].0)
        .find(|w| *w > 0.0)
        .unwrap_or(1.0);

    match width {
        w if w >= 1.0 => 1,
        w if w >= 0.1 => 2,
        w if w >= 0.01 => 3,
        _ => 4,
    }
}

fn validation_block(report: &BenchmarkReport, style: Style) -> String {
    if !report.has_validation() {
        return String::new();
    }

    let (total, failed) = report.validation_totals();
    let mut out = format!("\n{}\n", style.header("Response Validation"));
    out.push_str(&format!("Checked:  {total} responses\n"));
    out.push_str(&format!("Passed:   {}\n", total.saturating_sub(failed)));

    if failed > 0 {
        out.push_str(&format!("Failed:   {failed}\n\n"));

        // De-duplicate: a failing assertion usually fires on every response, so
        // the raw list is the same line thousands of times.
        let mut unique: Vec<String> = Vec::new();
        for err in report.validations().flat_map(|v| v.errors.iter()) {
            let line = format!("{}: {}", err.field, err.message);
            if !unique.contains(&line) {
                unique.push(line);
            }
        }

        const MAX_SHOWN: usize = 10;
        for line in unique.iter().take(MAX_SHOWN) {
            out.push_str(&format!("  {line}\n"));
        }
        if unique.len() > MAX_SHOWN {
            out.push_str(&format!(
                "  ... and {} more unique error(s)\n",
                unique.len() - MAX_SHOWN
            ));
        }
    }
    out
}

/// Render a regression check.
pub fn regression_summary(result: &RegressionResult, style: Style) -> String {
    let mut out = format!(
        "\n{}\n\n",
        style.header(&format!("Regression Check vs '{}'", result.baseline))
    );
    out.push_str(&format!(
        "{:<8} {:>12} {:>12} {:>10} {:>10}  {}\n",
        "Metric", "Baseline", "Current", "Change", "Threshold", "Status"
    ));
    out.push_str(&format!("{}\n", "-".repeat(70)));

    for m in &result.metrics {
        let status = if m.regressed {
            style.red("REGRESSED")
        } else {
            style.green("ok")
        };
        out.push_str(&format!(
            "{:<8} {:>9} ms {:>9} ms {:>9.1}% {:>9.0}%  {}\n",
            m.name,
            ms(m.baseline),
            ms(m.current),
            m.change * 100.0,
            m.threshold * 100.0,
            status,
        ));
    }

    out.push('\n');
    out.push_str(&if result.passed {
        style.green("Result: PASSED (no regressions detected)")
    } else {
        style.red("Result: FAILED (regression detected)")
    });
    out.push('\n');
    out
}

fn truncate(text: &str, width: usize) -> String {
    if text.chars().count() <= width {
        text.to_string()
    } else {
        text.chars()
            .take(width.saturating_sub(1))
            .collect::<String>()
            + "…"
    }
}

/// Prints the run summary to stdout.
#[derive(Debug)]
pub struct TerminalReporter {
    style: Style,
}

impl TerminalReporter {
    /// Colour on only when stdout is a terminal and `NO_COLOR` is unset, so
    /// redirected output and CI logs stay free of escape sequences.
    pub fn auto() -> Self {
        let enabled = std::io::stdout().is_terminal() && std::env::var_os("NO_COLOR").is_none();
        Self {
            style: if enabled { Style::COLOR } else { Style::PLAIN },
        }
    }

    pub fn with_style(style: Style) -> Self {
        Self { style }
    }
}

impl Default for TerminalReporter {
    fn default() -> Self {
        Self::auto()
    }
}

#[async_trait]
impl Reporter for TerminalReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> Result<()> {
        print_out(&benchmark_summary(report, self.style))
    }

    async fn on_regression(&self, result: &RegressionResult) -> Result<()> {
        print_out(&regression_summary(result, self.style))
    }
}

fn print_out(text: &str) -> Result<()> {
    let mut stdout = std::io::stdout().lock();
    stdout
        .write_all(text.as_bytes())
        .and_then(|_| stdout.flush())
        .map_err(|source| Error::write("<stdout>", source))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::baseline::MetricRegression;
    use crate::model::TargetReport;

    fn stats() -> BenchmarkStats {
        BenchmarkStats {
            total_requests: 100,
            count_success: 100,
            mean_ms: 12.5,
            p99_ms: 40.0,
            ..Default::default()
        }
    }

    #[test]
    fn plain_style_emits_no_escape_sequences() {
        let report = BenchmarkReport::single(TargetReport::new("api", stats()));
        let text = benchmark_summary(&report, Style::PLAIN);
        assert!(!text.contains('\x1b'), "plain output must be pipe-safe");
        assert!(text.contains("Mean"));
        assert!(text.contains("12.50 ms"));
    }

    #[test]
    fn color_style_emits_escape_sequences() {
        let report = BenchmarkReport::single(TargetReport::new("api", stats()));
        assert!(benchmark_summary(&report, Style::COLOR).contains('\x1b'));
    }

    #[test]
    fn confidence_colouring_thresholds() {
        assert_eq!(Style::PLAIN.confidence(0.99, "x"), "x");
        assert!(Style::COLOR.confidence(0.95, "x").contains("32"));
        assert!(Style::COLOR.confidence(0.70, "x").contains("33"));
        assert_eq!(Style::COLOR.confidence(0.10, "x"), "x");
    }

    #[test]
    fn histogram_scales_to_the_tallest_bin_and_skips_empties() {
        let text = histogram(&[(0.0, 10), (1.0, 0), (2.0, 5)]);
        assert_eq!(text.matches('\n').count(), 3, "header plus two bars");
        assert!(text.contains(&"█".repeat(40)), "tallest bin is full width");
        assert!(text.contains(&format!("{} 5", "█".repeat(20))));
        assert!(!text.contains(" 0\n"), "empty bins are not printed");
    }

    #[test]
    fn sub_millisecond_bins_get_enough_precision_to_stay_distinct() {
        // Three 0.05ms-wide bins would all render as "0.6 ms" at one decimal.
        let text = histogram(&[(0.60, 1), (0.65, 2), (0.70, 3)]);
        assert!(text.contains("0.600"), "got:\n{text}");
        assert!(text.contains("0.650"));
        assert!(text.contains("0.700"));
    }

    #[test]
    fn wide_bins_stay_at_one_decimal() {
        let text = histogram(&[(0.0, 1), (10.0, 2), (20.0, 3)]);
        assert!(text.contains("0.0 ms"));
        assert!(text.contains("10.0 ms"));
        assert!(!text.contains("10.00"));
    }

    #[test]
    fn an_all_zero_histogram_renders_nothing_rather_than_dividing_by_zero() {
        assert_eq!(histogram(&[(0.0, 0), (1.0, 0)]), "");
    }

    #[test]
    fn long_target_names_are_truncated_to_keep_columns_aligned() {
        assert_eq!(truncate("short", 20), "short");
        assert_eq!(truncate(&"x".repeat(30), 20).chars().count(), 20);
    }

    #[test]
    fn duplicate_validation_errors_are_collapsed() {
        use gauntlet_core::{ValidationError, ValidationSummary};

        let err = ValidationError {
            field: "$.id".into(),
            message: "mismatch".into(),
        };
        let mut target = TargetReport::new("api", stats());
        target.validation = vec![ValidationSummary {
            total: 100,
            failed: 3,
            errors: vec![err.clone(), err.clone(), err],
        }];

        let text = validation_block(&BenchmarkReport::single(target), Style::PLAIN);
        assert_eq!(
            text.matches("$.id: mismatch").count(),
            1,
            "the same failure repeated is shown once"
        );
        assert!(text.contains("Failed:   3"));
    }

    #[test]
    fn regression_summary_marks_the_failing_metric() {
        let result = RegressionResult {
            baseline: "main".into(),
            metrics: vec![
                MetricRegression {
                    name: "mean".into(),
                    baseline: 10.0,
                    current: 10.1,
                    change: 0.01,
                    threshold: 0.10,
                    regressed: false,
                },
                MetricRegression {
                    name: "p99".into(),
                    baseline: 10.0,
                    current: 20.0,
                    change: 1.0,
                    threshold: 0.15,
                    regressed: true,
                },
            ],
            passed: false,
        };

        let text = regression_summary(&result, Style::PLAIN);
        assert!(text.contains("Result: FAILED (regression detected)"));
        assert!(text.contains("REGRESSED"));
        assert!(text.contains("ok"));
    }
}
