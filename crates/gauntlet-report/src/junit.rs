//! JUnit XML rendering, and the reporter that writes it to a file. See the crate
//! docs for how statistics map onto the schema.

use std::path::PathBuf;
use std::sync::Mutex;

use async_trait::async_trait;
use gauntlet_core::ValidationSummary;
use gauntlet_stats::{BayesianComparison, BenchmarkStats};

use crate::format::{ms, xml_escape};
use crate::markdown::write_new;
use crate::model::BenchmarkReport;
use crate::{RegressionResult, Reporter};

/// The complete JUnit document for a finished benchmark.
pub fn benchmark_xml(report: &BenchmarkReport) -> String {
    document(&benchmark_suites(report))
}

/// The complete JUnit document for a baseline regression check.
pub fn regression_xml(result: &RegressionResult) -> String {
    document(&[regression_suite(result)])
}

/// Wrap rendered suites in the prolog and the single `<testsuites>` root.
fn document(suites: &[String]) -> String {
    let mut out = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<testsuites name=\"gauntlet\">\n",
    );
    for suite in suites {
        out.push_str(suite);
    }
    out.push_str("</testsuites>\n");
    out
}

/// The suites for a benchmark: statistics, pairwise comparisons, validation.
/// Empty sections are omitted rather than rendered as zero-test suites.
fn benchmark_suites(report: &BenchmarkReport) -> Vec<String> {
    let mut suites = Vec::new();

    let stats_cases: Vec<TestCase> = report
        .targets
        .iter()
        .map(|t| stats_case(&t.name, &t.stats))
        .collect();
    if !stats_cases.is_empty() {
        suites.push(testsuite("statistics", &stats_cases));
    }

    let comparison_cases: Vec<TestCase> = report
        .comparisons
        .iter()
        .map(|p| comparison_case(&p.a, &p.b, &p.comparison))
        .collect();
    if !comparison_cases.is_empty() {
        suites.push(testsuite("comparisons", &comparison_cases));
    }

    let validation_cases: Vec<TestCase> = report
        .validations()
        .enumerate()
        .map(|(i, s)| validation_case(i + 1, s))
        .collect();
    if !validation_cases.is_empty() {
        suites.push(testsuite("validation", &validation_cases));
    }

    suites
}

/// One target's statistics as a single case carrying the numbers as properties.
fn stats_case(name: &str, s: &BenchmarkStats) -> TestCase {
    TestCase {
        name: name.to_string(),
        classname: "gauntlet.stats",
        properties: vec![
            ("total_requests".into(), s.total_requests.to_string()),
            ("success_count".into(), s.count_success.to_string()),
            ("failure_count".into(), s.count_failure.to_string()),
            ("mean_ms".into(), ms(s.mean_ms)),
            ("stddev_ms".into(), ms(s.std_dev_ms)),
            ("min_ms".into(), ms(s.min_ms)),
            ("max_ms".into(), ms(s.max_ms)),
            ("p50_ms".into(), ms(s.p50_ms)),
            ("p95_ms".into(), ms(s.p95_ms)),
            ("p99_ms".into(), ms(s.p99_ms)),
            ("es_ms".into(), ms(s.es_ms)),
        ],
        failure: None,
    }
}

/// One pairwise comparison. The suite is named "B versus A", matching the
/// direction the comparison itself reads in and every other renderer.
fn comparison_case(a: &str, b: &str, c: &BayesianComparison) -> TestCase {
    let mut properties = vec![
        (
            "prob_b_faster".into(),
            format!("{:.4}", c.prob_b_faster_than_a),
        ),
        (
            "prob_single_request_faster".into(),
            format!("{:.4}", c.prob_single_request_faster),
        ),
        (
            "prob_b_less_jittery".into(),
            format!("{:.4}", c.prob_b_less_jittery),
        ),
        ("cohens_d".into(), format!("{:.4}", c.effect_size)),
        ("mean_diff_ms".into(), ms(c.mean_difference)),
    ];
    if let Some(emd) = c.emd {
        properties.push(("emd_ms".into(), ms(emd)));
    }

    TestCase {
        name: format!("{b} vs {a}"),
        classname: "gauntlet.comparison",
        properties,
        failure: None,
    }
}

/// A validation summary fails its case when any response failed its assertions.
fn validation_case(index: usize, s: &ValidationSummary) -> TestCase {
    let failure = (s.failed > 0).then(|| {
        let detail = s
            .errors
            .iter()
            .map(|e| format!("{}: {}", e.field, e.message))
            .collect::<Vec<_>>()
            .join("\n");
        format!("{} of {} validations failed\n{}", s.failed, s.total, detail)
    });

    TestCase {
        name: format!("validation_{index}"),
        classname: "gauntlet.validation",
        properties: vec![
            ("total".into(), s.total.to_string()),
            ("failed".into(), s.failed.to_string()),
        ],
        failure,
    }
}

/// The regression suite: one case per compared metric, failing where it
/// exceeded its threshold.
fn regression_suite(result: &RegressionResult) -> String {
    let cases: Vec<TestCase> = result
        .metrics
        .iter()
        .map(|m| TestCase {
            name: m.name.clone(),
            classname: "gauntlet.regression",
            properties: vec![
                ("baseline_ms".into(), ms(m.baseline)),
                ("current_ms".into(), ms(m.current)),
                ("change".into(), format!("{:.4}", m.change)),
                ("threshold".into(), format!("{:.4}", m.threshold)),
            ],
            failure: m.regressed.then(|| {
                format!(
                    "{} regressed by {:.1}% (threshold {:.1}%)",
                    m.name,
                    m.change * 100.0,
                    m.threshold * 100.0
                )
            }),
        })
        .collect();

    testsuite(&format!("regression vs {}", result.baseline), &cases)
}

/// A synthetic test case. `classname` is a fixed namespace, never user input,
/// so it needs no escaping; `name`, the property values, and the failure text
/// all do.
struct TestCase {
    name: String,
    classname: &'static str,
    properties: Vec<(String, String)>,
    failure: Option<String>,
}

fn testsuite(name: &str, cases: &[TestCase]) -> String {
    let failures = cases.iter().filter(|c| c.failure.is_some()).count();
    let mut out = format!(
        "  <testsuite name=\"{}\" tests=\"{}\" failures=\"{}\">\n",
        xml_escape(name),
        cases.len(),
        failures
    );
    for case in cases {
        out.push_str(&render_case(case));
    }
    out.push_str("  </testsuite>\n");
    out
}

fn render_case(case: &TestCase) -> String {
    let open = format!(
        "    <testcase name=\"{}\" classname=\"{}\"",
        xml_escape(&case.name),
        case.classname
    );

    if case.properties.is_empty() && case.failure.is_none() {
        return format!("{open}/>\n");
    }

    let mut out = format!("{open}>\n");
    if !case.properties.is_empty() {
        out.push_str("      <properties>\n");
        for (key, value) in &case.properties {
            out.push_str(&format!(
                "        <property name=\"{}\" value=\"{}\"/>\n",
                xml_escape(key),
                xml_escape(value)
            ));
        }
        out.push_str("      </properties>\n");
    }
    if let Some(message) = &case.failure {
        // The message attribute is the one-line summary shown in a CI UI; the
        // element body carries the full detail.
        let first_line = message.lines().next().unwrap_or_default();
        out.push_str(&format!(
            "      <failure message=\"{}\">{}</failure>\n",
            xml_escape(first_line),
            xml_escape(message)
        ));
    }
    out.push_str("    </testcase>\n");
    out
}

/// Writes a single JUnit XML document describing the whole run. Suites
/// accumulate and the file is rewritten each time, because XML has one root.
#[derive(Debug)]
pub struct JUnitReporter {
    path: PathBuf,
    suites: Mutex<Vec<String>>,
}

impl JUnitReporter {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self {
            path: path.into(),
            suites: Mutex::new(Vec::new()),
        }
    }

    /// Add suites and rewrite the document. The lock is released before the
    /// write so a slow filesystem cannot block a concurrent event.
    fn extend_and_write(&self, new_suites: Vec<String>) -> crate::Result<()> {
        let rendered = {
            let mut suites = self.suites.lock().expect("junit suite lock poisoned");
            suites.extend(new_suites);
            document(&suites)
        };
        write_new(&self.path, &rendered)
    }
}

#[async_trait]
impl Reporter for JUnitReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> crate::Result<()> {
        self.extend_and_write(benchmark_suites(report))
    }

    /// Regressions are rendered: a failing baseline check is exactly the thing a
    /// CI test view should show as a red test.
    async fn on_regression(&self, result: &RegressionResult) -> crate::Result<()> {
        self.extend_and_write(vec![regression_suite(result)])
    }
}

#[cfg(test)]
mod tests {
    use gauntlet_core::{ValidationError, ValidationSummary};

    use super::*;
    use crate::baseline::MetricRegression;
    use crate::model::{PairComparison, TargetReport};

    fn stats(mean: f64) -> BenchmarkStats {
        BenchmarkStats {
            mean_ms: mean,
            count_success: 10,
            ..Default::default()
        }
    }

    fn metric(name: &str, change: f64, threshold: f64) -> MetricRegression {
        MetricRegression {
            name: name.into(),
            baseline: 10.0,
            current: 10.0 * (1.0 + change),
            change,
            threshold,
            regressed: change > threshold,
        }
    }

    /// Counts non-overlapping occurrences — enough to assert structure without
    /// pulling in an XML parser.
    fn count(haystack: &str, needle: &str) -> usize {
        haystack.matches(needle).count()
    }

    #[test]
    fn a_target_name_with_an_ampersand_stays_well_formed() {
        let report = BenchmarkReport::single(TargetReport::new("a&b<c>", stats(1.0)));
        let xml = benchmark_xml(&report);

        assert!(
            xml.contains(r#"<testcase name="a&amp;b&lt;c&gt;""#),
            "the raw name must never reach the document: {xml}"
        );
        assert!(!xml.contains("a&b"), "no unescaped ampersand survives");
    }

    #[test]
    fn a_quote_in_a_validation_message_cannot_break_out_of_the_attribute() {
        let mut target = TargetReport::new("api", stats(1.0));
        target.validation = vec![ValidationSummary {
            total: 2,
            failed: 1,
            errors: vec![ValidationError {
                field: "$.name".into(),
                message: r#"expected "a" & got <b>"#.into(),
            }],
        }];
        let xml = benchmark_xml(&BenchmarkReport::single(target));

        assert!(xml.contains("&quot;a&quot; &amp; got &lt;b&gt;"));
        assert!(!xml.contains(r#"got <b>"#));
    }

    #[test]
    fn the_document_has_exactly_one_prolog_and_one_root() {
        let report = BenchmarkReport::single(TargetReport::new("api", stats(1.0)));
        let xml = benchmark_xml(&report);

        assert_eq!(count(&xml, "<?xml"), 1);
        assert_eq!(count(&xml, "<testsuites"), 1);
        assert_eq!(count(&xml, "</testsuites>"), 1);
    }

    #[test]
    fn statistics_are_emitted_as_values_not_just_names() {
        // The Haskell reporter discarded every metric value; assert they arrive.
        let report = BenchmarkReport::single(TargetReport::new("api", stats(12.5)));
        let xml = benchmark_xml(&report);

        assert!(xml.contains(r#"<property name="mean_ms" value="12.50"/>"#));
        assert!(xml.contains(r#"<property name="success_count" value="10"/>"#));
    }

    #[test]
    fn a_single_target_report_has_no_comparison_suite() {
        let xml = benchmark_xml(&BenchmarkReport::single(TargetReport::new(
            "api",
            stats(1.0),
        )));

        assert!(xml.contains(r#"<testsuite name="statistics" tests="1""#));
        assert!(!xml.contains("comparisons"));
        assert!(!xml.contains("validation"));
    }

    #[test]
    fn a_multi_target_report_names_comparisons_candidate_first() {
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
                    ..Default::default()
                },
            }],
        };
        let xml = benchmark_xml(&report);

        assert!(xml.contains(r#"<testsuite name="statistics" tests="2""#));
        assert!(xml.contains(r#"<testcase name="candidate vs base""#));
        assert!(xml.contains(r#"<property name="prob_b_faster" value="0.9400"/>"#));
    }

    #[test]
    fn the_emd_property_appears_only_when_it_was_computed() {
        let with = comparison_case(
            "a",
            "b",
            &BayesianComparison {
                emd: Some(1.5),
                ..Default::default()
            },
        );
        assert!(with.properties.iter().any(|(k, _)| k == "emd_ms"));

        let without = comparison_case("a", "b", &BayesianComparison::default());
        assert!(!without.properties.iter().any(|(k, _)| k == "emd_ms"));
    }

    #[test]
    fn an_empty_report_still_renders_a_valid_empty_document() {
        let xml = benchmark_xml(&BenchmarkReport::default());
        assert_eq!(
            xml,
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<testsuites name=\"gauntlet\">\n</testsuites>\n"
        );
    }

    #[test]
    fn a_failing_validation_becomes_a_failure_element() {
        let mut target = TargetReport::new("api", stats(1.0));
        target.validation = vec![
            ValidationSummary {
                total: 10,
                failed: 0,
                errors: Vec::new(),
            },
            ValidationSummary {
                total: 10,
                failed: 3,
                errors: Vec::new(),
            },
        ];
        let xml = benchmark_xml(&BenchmarkReport::single(target));

        assert!(xml.contains(r#"<testsuite name="validation" tests="2" failures="1">"#));
        assert!(xml.contains(r#"<failure message="3 of 10 validations failed">"#));
    }

    #[test]
    fn regression_failures_are_counted_per_metric() {
        let result = RegressionResult {
            baseline: "main".into(),
            metrics: vec![metric("mean", 0.02, 0.10), metric("p99", 1.0, 0.15)],
            passed: false,
        };
        let xml = regression_xml(&result);

        assert!(xml.contains(r#"<testsuite name="regression vs main" tests="2" failures="1">"#));
        assert!(xml.contains("p99 regressed by 100.0% (threshold 15.0%)"));
        assert!(
            !xml.contains("mean regressed"),
            "a metric within threshold does not fail"
        );
    }

    #[tokio::test]
    async fn a_benchmark_followed_by_a_regression_yields_one_document() {
        let dir = std::env::temp_dir().join(format!("gauntlet-junit-{}", std::process::id()));
        let path = dir.join("nested").join("report.xml");
        let reporter = JUnitReporter::new(&path);

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
                metrics: vec![metric("mean", 0.5, 0.10)],
                passed: false,
            })
            .await
            .unwrap();

        let written = std::fs::read_to_string(&path).unwrap();
        std::fs::remove_dir_all(&dir).ok();

        assert_eq!(count(&written, "<?xml"), 1, "one prolog, not two");
        assert_eq!(count(&written, "<testsuites"), 1, "one root, not two");
        assert!(written.contains(r#"<testsuite name="statistics""#));
        assert!(written.contains(r#"<testsuite name="regression vs main""#));
    }
}
