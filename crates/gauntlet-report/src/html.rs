//! HTML rendering, and the reporter that writes it to a file.
//!
//! One self-contained document: the charts are inlined as `<svg>` elements and
//! the stylesheet as an inline `<style>`, so the file renders correctly when it
//! is opened straight from a file manager over `file://` — browsers refuse to
//! load subresources there, and a report that needs a web server to look right
//! is a report nobody looks at.
//!
//! There is no templating crate. One template with no user extensibility does
//! not earn a build-time template step, so the renderers are `format!` calls
//! (ADR `M4-report` §8) — and every interpolated string goes through
//! [`html_escape`], which the Haskell original only did for target names.

use std::path::PathBuf;
use std::sync::Mutex;

use async_trait::async_trait;
use gauntlet_stats::{BayesianComparison, BenchmarkStats};

use crate::chart::{self, ChartKind};
use crate::error::Result;
use crate::format::{html_escape, ms, prob_pct, signed_pct};
use crate::markdown::write_new;
use crate::model::{BenchmarkReport, TargetReport};
use crate::{RegressionResult, Reporter};

/// Wrap rendered body markup in a complete, self-contained document.
pub fn document(title: &str, body: &str) -> String {
    format!(
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
         <title>{}</title>\n{STYLE}</head>\n<body>\n{body}</body>\n</html>\n",
        html_escape(title)
    )
}

/// The body markup for a finished benchmark, charts included.
///
/// Fallible where the markdown renderer is not: chart rendering can fail, and a
/// half-drawn SVG should surface as [`crate::Error::Chart`] rather than a silent
/// gap in the page.
pub fn benchmark_body(report: &BenchmarkReport, charts: &[ChartKind]) -> Result<String> {
    let mut out = String::from("<h1>Benchmark Report</h1>\n");

    if report.targets.is_empty() {
        out.push_str("<p class=\"empty\">No targets were benchmarked.</p>\n");
        return Ok(out);
    }

    if !report.is_single() {
        out.push_str("<h2>Ranking</h2>\n");
        out.push_str(&ranking_table(report));
    }

    for (index, target) in report.targets.iter().enumerate() {
        out.push_str(&target_section(target, index, charts)?);
    }

    for pair in &report.comparisons {
        out.push_str(&comparison_section(&pair.a, &pair.b, &pair.comparison));
    }

    out.push_str(&validation_section(report));
    Ok(out)
}

/// The body markup for a baseline regression check.
pub fn regression_body(result: &RegressionResult) -> String {
    let verdict = if result.passed {
        "<span class=\"pass\">PASSED</span>"
    } else {
        "<span class=\"fail\">FAILED</span>"
    };

    let mut out = format!(
        "<h2>Regression Check</h2>\n\
         <p>Baseline: <strong>{}</strong> — {verdict}</p>\n\
         <table>\n<thead><tr><th>Metric</th><th>Baseline</th><th>Current</th>\
         <th>Change</th><th>Threshold</th><th>Status</th></tr></thead>\n<tbody>\n",
        html_escape(&result.baseline)
    );

    for m in &result.metrics {
        let status = if m.regressed {
            "<span class=\"fail\">REGRESSED</span>"
        } else {
            "<span class=\"pass\">OK</span>"
        };
        out.push_str(&format!(
            "<tr><th scope=\"row\">{}</th><td>{} ms</td><td>{} ms</td>\
             <td>{}</td><td>{:.0}%</td><td>{status}</td></tr>\n",
            html_escape(&m.name),
            ms(m.baseline),
            ms(m.current),
            signed_pct(m.change),
            m.threshold * 100.0,
        ));
    }
    out.push_str("</tbody>\n</table>\n");
    out
}

fn target_section(target: &TargetReport, index: usize, charts: &[ChartKind]) -> Result<String> {
    let mut out = format!("<h2>{}</h2>\n", html_escape(&target.name));
    if !target.url.is_empty() {
        // Rendered as text, not as a link: the URL comes from config and a
        // report is not a place to hand the reader a one-click navigation.
        out.push_str(&format!(
            "<p class=\"url\">{}</p>\n",
            html_escape(&target.url)
        ));
    }
    out.push_str(&stats_table(&target.stats));

    // Counted rather than assumed: a target can have samples and still produce
    // no chart — an all-failed run has no latency for a CDF to plot, and a
    // chart set of only latency kinds then renders nothing at all.
    let mut drawn = 0usize;
    if chart::has_samples(target) {
        for kind in charts {
            if let Some(svg) = chart::render(*kind, &target.name, &target.samples, index)? {
                // `plotters` escapes the text it emits, so the SVG is inlined
                // verbatim — escaping it again would show the markup instead of
                // drawing it.
                out.push_str(&format!("<figure class=\"chart\">\n{svg}</figure>\n"));
                drawn += 1;
            }
        }
    }
    if drawn == 0 && !charts.is_empty() {
        out.push_str("<p class=\"empty\">No successful responses — nothing to chart.</p>\n");
    }
    Ok(out)
}

fn comparison_section(name_a: &str, name_b: &str, bayes: &BayesianComparison) -> String {
    let (a, b) = (html_escape(name_a), html_escape(name_b));
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
            "Mean difference".to_string(),
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
        (
            "P(p95 regression)".to_string(),
            prob_pct(bayes.p95_comparison.prob_pct_regression),
        ),
        ("p99 difference".to_string(), pct(&bayes.p99_comparison)),
        (
            "P(p99 regression)".to_string(),
            prob_pct(bayes.p99_comparison.prob_pct_regression),
        ),
    ];
    if let Some(emd) = bayes.emd {
        rows.push(("Earth Mover's Distance".to_string(), format!("{emd:.3} ms")));
    }

    // The labels are built from already-escaped names, so they are inserted
    // as-is; only the raw heading needs escaping here.
    let mut out = format!("<h2>{b} vs {a}</h2>\n{}", metric_table_head());
    for (label, value) in rows {
        out.push_str(&format!(
            "<tr><th scope=\"row\">{label}</th><td>{value}</td></tr>\n"
        ));
    }
    out.push_str("</tbody>\n</table>\n");
    out
}

fn validation_section(report: &BenchmarkReport) -> String {
    if !report.has_validation() {
        return String::new();
    }

    let (total, failed) = report.validation_totals();
    let mut out = format!(
        "<h2>Validation</h2>\n<p>{} responses checked, {failed} failed.</p>\n\
         <table>\n<thead><tr><th>#</th><th>Validated</th><th>Passed</th><th>Failed</th>\
         <th>Status</th></tr></thead>\n<tbody>\n",
        total
    );

    for (i, summary) in report.validations().enumerate() {
        let status = if summary.failed > 0 {
            "<span class=\"fail\">FAIL</span>"
        } else {
            "<span class=\"pass\">PASS</span>"
        };
        out.push_str(&format!(
            "<tr><th scope=\"row\">{}</th><td>{}</td><td>{}</td><td>{}</td><td>{status}</td></tr>\n",
            i + 1,
            summary.total,
            summary.total.saturating_sub(summary.failed),
            summary.failed,
        ));
    }
    out.push_str("</tbody>\n</table>\n");

    // Haskell's terminal summary capped the displayed errors at 10 unique; the
    // HTML report keeps the same cap so a fully-failing run does not render a
    // megabyte of identical assertion messages.
    let errors: Vec<String> = report
        .validations()
        .flat_map(|s| s.errors.iter())
        .map(|e| {
            format!(
                "<li><code>{}</code>: {}</li>\n",
                html_escape(&e.field),
                html_escape(&e.message)
            )
        })
        .take(MAX_DISPLAYED_ERRORS)
        .collect();
    if !errors.is_empty() {
        out.push_str("<h3>Validation errors</h3>\n<ul>\n");
        out.extend(errors);
        out.push_str("</ul>\n");
    }
    out
}

/// Matches the Haskell `printValidationSummary` cap.
const MAX_DISPLAYED_ERRORS: usize = 10;

fn ranking_table(report: &BenchmarkReport) -> String {
    let mut out = String::from(
        "<table>\n<thead><tr><th>#</th><th>Target</th><th>Mean</th><th>p50</th>\
         <th>p95</th><th>p99</th></tr></thead>\n<tbody>\n",
    );
    for (i, target) in report.ranked().iter().enumerate() {
        let s = &target.stats;
        out.push_str(&format!(
            "<tr><th scope=\"row\">{}</th><td class=\"name\">{}</td>\
             <td>{} ms</td><td>{} ms</td><td>{} ms</td><td>{} ms</td></tr>\n",
            i + 1,
            html_escape(&target.name),
            ms(s.mean_ms),
            ms(s.p50_ms),
            ms(s.p95_ms),
            ms(s.p99_ms),
        ));
    }
    out.push_str("</tbody>\n</table>\n");
    out
}

fn stats_table(s: &BenchmarkStats) -> String {
    let rows = [
        ("Total requests", s.total_requests.to_string()),
        ("Success", s.count_success.to_string()),
        ("Failure", s.count_failure.to_string()),
        ("Mean", format!("{} ms", ms(s.mean_ms))),
        ("Std dev", format!("{} ms", ms(s.std_dev_ms))),
        ("Min", format!("{} ms", ms(s.min_ms))),
        ("Max", format!("{} ms", ms(s.max_ms))),
        ("p50", format!("{} ms", ms(s.p50_ms))),
        ("p95", format!("{} ms", ms(s.p95_ms))),
        ("p99", format!("{} ms", ms(s.p99_ms))),
        ("ES(p99)", format!("{} ms", ms(s.es_ms))),
    ];

    let mut out = metric_table_head();
    for (label, value) in rows {
        out.push_str(&format!(
            "<tr><th scope=\"row\">{label}</th><td>{value}</td></tr>\n"
        ));
    }
    out.push_str("</tbody>\n</table>\n");
    out
}

fn metric_table_head() -> String {
    String::from("<table>\n<thead><tr><th>Metric</th><th>Value</th></tr></thead>\n<tbody>\n")
}

/// Inline stylesheet. Colours are declared as custom properties and swapped by
/// `prefers-color-scheme`, so one document reads correctly in a light and a dark
/// browser without any script.
const STYLE: &str = r#"<style>
:root {
  --bg: #ffffff; --fg: #1f2328; --muted: #57606a;
  --rule: #d0d7de; --head: #f6f8fa; --zebra: #fafbfc;
  --accent: #2563eb; --pass: #15803d; --failc: #b91c1c;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #14161a; --fg: #e6e6e6; --muted: #9aa4b2;
    --rule: #30363d; --head: #1c2027; --zebra: #191c22;
    --accent: #7aa2f7; --pass: #4ade80; --failc: #f87171;
  }
}
body {
  background: var(--bg); color: var(--fg);
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
  max-width: 60rem; margin: 2rem auto; padding: 0 1rem; line-height: 1.5;
}
h1 { border-bottom: 2px solid var(--accent); padding-bottom: .3rem; }
h2 { color: var(--accent); margin-top: 2rem; }
h3 { margin-top: 1.5rem; }
p.url, p.empty { color: var(--muted); font-size: .9rem; }
table { border-collapse: collapse; width: 100%; margin: 1rem 0; font-variant-numeric: tabular-nums; }
th, td { border: 1px solid var(--rule); padding: .4rem .7rem; text-align: right; }
thead th, tbody th { background: var(--head); text-align: left; font-weight: 600; }
tbody tr:nth-child(even) { background: var(--zebra); }
td.name { text-align: left; }
code { background: var(--head); padding: 0 .25rem; border-radius: 3px; }
.pass { color: var(--pass); font-weight: 600; }
.fail { color: var(--failc); font-weight: 600; }
figure.chart { margin: 1rem 0; overflow-x: auto; }
figure.chart svg { max-width: 100%; height: auto; }
</style>
"#;

/// Writes one self-contained HTML file for the whole run.
///
/// Unlike the markdown reporter, this one does **not** append: concatenating a
/// second `<!DOCTYPE>` onto a finished document — which the Haskell version did
/// — produces a file no browser parses as one page. Instead the rendered body is
/// kept and the file is rewritten with the regression section appended, so the
/// output stays a single valid document however many events arrive.
pub struct HtmlReporter {
    path: PathBuf,
    charts: Vec<ChartKind>,
    body: Mutex<Option<String>>,
}

impl HtmlReporter {
    /// A reporter with the default chart set: the distribution and the CDF,
    /// which between them answer most latency questions without making the page
    /// four charts long per target.
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self::with_charts(path, vec![ChartKind::Histogram, ChartKind::Cdf])
    }

    /// A reporter rendering the given chart kinds; pass an empty vector for a
    /// tables-only report.
    pub fn with_charts(path: impl Into<PathBuf>, charts: Vec<ChartKind>) -> Self {
        Self {
            path: path.into(),
            charts,
            body: Mutex::new(None),
        }
    }
}

#[async_trait]
impl Reporter for HtmlReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> Result<()> {
        let body = benchmark_body(report, &self.charts)?;
        *self.body.lock().expect("html body lock") = Some(body.clone());
        write_new(&self.path, &document("Gauntlet Benchmark Report", &body))
    }

    /// Rendered, and merged into the benchmark document when there is one. A
    /// regression check run on its own still gets a complete page.
    async fn on_regression(&self, result: &RegressionResult) -> Result<()> {
        let benchmark = self.body.lock().expect("html body lock").clone();
        let (title, body) = match benchmark {
            Some(body) => (
                "Gauntlet Benchmark Report",
                format!("{body}{}", regression_body(result)),
            ),
            None => (
                "Gauntlet Regression Report",
                format!("<h1>Regression Report</h1>\n{}", regression_body(result)),
            ),
        };
        write_new(&self.path, &document(title, &body))
    }
}

#[cfg(test)]
mod tests {
    use gauntlet_core::{ValidationError, ValidationSummary};

    use super::*;
    use crate::baseline::MetricRegression;
    use crate::model::{PairComparison, Sample};

    fn stats(mean: f64) -> BenchmarkStats {
        BenchmarkStats {
            mean_ms: mean,
            total_requests: 100,
            count_success: 100,
            p50_ms: mean,
            p95_ms: mean * 1.5,
            p99_ms: mean * 2.0,
            ..Default::default()
        }
    }

    fn target(name: &str, mean: f64) -> TargetReport {
        let mut t = TargetReport::new(name, stats(mean));
        t.url = format!("https://example.test/{name}");
        t.samples = (0..80)
            .map(|i| Sample {
                latency_ms: Some(mean + (i % 11) as f64 * 0.3),
                status: 200,
                offset_s: i as f64 * 0.05,
            })
            .collect();
        t
    }

    fn no_charts() -> Vec<ChartKind> {
        Vec::new()
    }

    #[test]
    fn a_single_target_report_has_no_ranking_or_comparison() {
        let report = BenchmarkReport::single(target("api", 12.5));
        let html = benchmark_body(&report, &no_charts()).unwrap();

        assert!(html.contains("<h2>api</h2>"));
        assert!(html.contains("<th scope=\"row\">Mean</th><td>12.50 ms</td>"));
        assert!(!html.contains("<h2>Ranking</h2>"));
        assert!(!html.contains("vs"));
    }

    #[test]
    fn a_multi_target_report_ranks_fastest_first_and_renders_each_pair() {
        let report = BenchmarkReport {
            targets: vec![target("slow", 30.0), target("fast", 10.0)],
            comparisons: vec![PairComparison {
                a: "slow".into(),
                b: "fast".into(),
                comparison: BayesianComparison {
                    prob_b_faster_than_a: 0.94,
                    ..Default::default()
                },
            }],
        };
        let html = benchmark_body(&report, &no_charts()).unwrap();

        assert!(html.contains("<h2>Ranking</h2>"));
        let fast = html.find(">fast</td>").expect("fast is ranked");
        let slow = html.find(">slow</td>").expect("slow is ranked");
        assert!(fast < slow, "the faster target ranks first");
        assert!(html.contains("<h2>fast vs slow</h2>"));
        assert!(html.contains("<td>94.0%</td>"));
    }

    #[test]
    fn a_target_name_containing_markup_cannot_inject_it() {
        let mut hostile = target("<script>alert(1)</script>", 10.0);
        hostile.url = "https://x.test/?a=<b>&c=\"d\"".into();
        hostile.validation = vec![ValidationSummary {
            total: 1,
            failed: 1,
            errors: vec![ValidationError {
                field: "<img onerror=x>".into(),
                message: "expected </td>".into(),
            }],
        }];

        let html = document(
            "Gauntlet",
            &benchmark_body(&BenchmarkReport::single(hostile), &no_charts()).unwrap(),
        );

        assert!(
            !html.contains("<script>"),
            "a target name must not open a script tag"
        );
        assert!(!html.contains("<img onerror"));
        assert!(html.contains("&lt;script&gt;alert(1)&lt;/script&gt;"));
        assert!(html.contains("&lt;img onerror=x&gt;"));
        assert!(html.contains("expected &lt;/td&gt;"));
        assert!(html.contains("&amp;c=&quot;d&quot;"), "URL is escaped too");
    }

    #[test]
    fn a_target_with_no_samples_says_so_instead_of_charting_nothing() {
        let report = BenchmarkReport::single(TargetReport::new("dead", stats(0.0)));
        let html = benchmark_body(&report, &ChartKind::ALL).unwrap();

        assert!(html.contains("nothing to chart"));
        assert!(!html.contains("<svg"));
    }

    #[test]
    fn a_target_whose_requests_all_failed_still_charts_its_outcomes() {
        let mut dead = TargetReport::new("down", stats(0.0));
        dead.samples = (0..40)
            .map(|i| Sample {
                latency_ms: None,
                status: 503,
                offset_s: i as f64 * 0.1,
            })
            .collect();
        let report = BenchmarkReport::single(dead);

        let html = benchmark_body(&report, &ChartKind::ALL).unwrap();
        assert!(
            html.contains("<svg"),
            "the error and status charts still draw"
        );
        assert!(!html.contains("nothing to chart"));

        // A latency-only chart set has nothing to say about this target.
        let latency_only = benchmark_body(&report, &[ChartKind::Cdf]).unwrap();
        assert!(latency_only.contains("nothing to chart"));
        assert!(!latency_only.contains("<svg"));
    }

    #[test]
    fn the_document_is_self_contained() {
        let report = BenchmarkReport::single(target("api", 12.0));
        let html = document(
            "Gauntlet",
            &benchmark_body(&report, &[ChartKind::Cdf, ChartKind::Histogram]).unwrap(),
        );

        assert!(html.contains("<svg"), "charts are inlined, not linked");
        assert!(html.contains("<style>"), "the stylesheet is inlined");
        // The only remaining absolute URL is the SVG XML namespace, which is an
        // identifier and never fetched. Nothing may be *loaded* from the network.
        assert!(!html.contains("<link"), "no external stylesheet");
        assert!(!html.contains("<script"), "no scripts at all");
        assert!(!html.contains("src=\"http"), "no remote asset");
        assert!(!html.contains("href=\"http"), "no remote asset");
        assert!(!html.contains("@import"), "no imported stylesheet");
    }

    #[test]
    fn the_document_styles_both_colour_schemes() {
        let html = document("Gauntlet", "");
        assert!(html.contains("@media (prefers-color-scheme: dark)"));
    }

    #[test]
    fn a_regression_section_marks_only_the_regressed_metrics() {
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
        let html = regression_body(&result);

        assert!(html.contains("<span class=\"fail\">FAILED</span>"));
        assert_eq!(html.matches("REGRESSED").count(), 1);
        assert!(html.contains("<span class=\"pass\">OK</span>"));
        assert!(html.contains("<td>+100.0%</td>"));
    }

    #[tokio::test]
    async fn a_regression_is_merged_into_the_benchmark_document_not_appended_after_it() {
        let path = std::env::temp_dir().join(format!("gauntlet-html-{}.html", std::process::id()));
        let _ = std::fs::remove_file(&path);

        let reporter = HtmlReporter::with_charts(&path, no_charts());
        let report = BenchmarkReport::single(target("api", 12.0));
        reporter.on_benchmark(&report).await.unwrap();
        reporter
            .on_regression(&RegressionResult {
                baseline: "main".into(),
                metrics: Vec::new(),
                passed: true,
            })
            .await
            .unwrap();

        let html = std::fs::read_to_string(&path).unwrap();
        assert_eq!(
            html.matches("<!DOCTYPE html>").count(),
            1,
            "the file stays one document"
        );
        assert!(
            html.contains("<h2>api</h2>"),
            "the benchmark section survives"
        );
        assert!(html.contains("<h2>Regression Check</h2>"));

        let _ = std::fs::remove_file(&path);
    }

    #[tokio::test]
    async fn a_regression_only_run_still_writes_a_complete_page() {
        let path =
            std::env::temp_dir().join(format!("gauntlet-html-reg-{}.html", std::process::id()));
        let _ = std::fs::remove_file(&path);

        let reporter = HtmlReporter::with_charts(&path, no_charts());
        reporter
            .on_regression(&RegressionResult {
                baseline: "main".into(),
                metrics: Vec::new(),
                passed: true,
            })
            .await
            .unwrap();

        let html = std::fs::read_to_string(&path).unwrap();
        assert!(html.starts_with("<!DOCTYPE html>"));
        assert!(html.contains("<h1>Regression Report</h1>"));
        assert!(html.trim_end().ends_with("</html>"));

        let _ = std::fs::remove_file(&path);
    }
}
