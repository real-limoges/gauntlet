//! CI integration: regression output shaped for the detected CI provider, plus
//! the markdown artifact those providers collect.
//!
//! Only regressions are reported here. A CI run's job is to answer "did this
//! change make things slower"; the full benchmark detail belongs in the
//! markdown/HTML artifacts.

use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use async_trait::async_trait;

use crate::error::Result;
use crate::format::{ms, signed_pct};
use crate::markdown;
use crate::{RegressionResult, Reporter};

/// The CI provider we appear to be running under.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CiMode {
    GitLab,
    GitHub,
    None,
}

/// Detect the CI provider from the environment.
///
/// GitLab is checked first: a GitLab job can shell out in ways that leave
/// `GITHUB_ACTIONS` set, but not the reverse.
pub fn detect_ci() -> CiMode {
    let is_true = |var: &str| std::env::var(var).map(|v| v == "true").unwrap_or(false);

    if is_true("GITLAB_CI") {
        CiMode::GitLab
    } else if is_true("GITHUB_ACTIONS") {
        CiMode::GitHub
    } else {
        CiMode::None
    }
}

/// Render the regression check for a CI job log.
///
/// GitLab gets collapsible section markers and colour; GitHub Actions renders
/// its own log formatting and gets plain text.
pub fn ci_log(mode: CiMode, result: &RegressionResult, epoch_secs: u64) -> String {
    /// Wraps a status word in colour, or leaves it alone.
    type Colorize = fn(&str) -> String;

    let (color_bad, color_good): (Colorize, Colorize) = match mode {
        CiMode::GitLab => (
            |s| format!("\x1b[1;31m{s}\x1b[0m"),
            |s| format!("\x1b[1;32m{s}\x1b[0m"),
        ),
        _ => (|s| s.to_string(), |s| s.to_string()),
    };

    let mut out = String::from("\n");
    if mode == CiMode::GitLab {
        out.push_str(&format!(
            "\x1b[0Ksection_start:{epoch_secs}:benchmark_results[collapsed=false]\n"
        ));
        out.push_str(
            "\x1b[0K\x1b[1;34m========== Benchmark Regression Check ==========\x1b[0m\n\n",
        );
    } else {
        out.push_str("========== Benchmark Regression Check ==========\n\n");
    }

    for m in &result.metrics {
        let status = if m.regressed {
            color_bad("REGRESSED")
        } else {
            color_good("ok")
        };
        out.push_str(&format!(
            "  {}: {}ms -> {}ms ({}) [{}]\n",
            m.name,
            ms(m.baseline),
            ms(m.current),
            signed_pct(m.change),
            status,
        ));
    }

    out.push('\n');
    out.push_str(&if result.passed {
        color_good("[PASS] Benchmark passed - no regressions detected")
    } else {
        color_bad("[FAIL] Benchmark FAILED - regression detected")
    });
    out.push('\n');

    if mode == CiMode::GitLab {
        out.push_str(&format!(
            "\x1b[0Ksection_end:{epoch_secs}:benchmark_results\n"
        ));
    }
    out
}

/// Emits CI-shaped regression output, writes a markdown artifact, and on GitHub
/// appends to the job's Step Summary. A no-op outside CI.
pub struct CiReporter {
    mode: CiMode,
    artifact_dir: PathBuf,
}

impl CiReporter {
    /// Build a reporter for the detected provider, writing artifacts to `dir`.
    pub fn detect(artifact_dir: impl Into<PathBuf>) -> Self {
        Self {
            mode: detect_ci(),
            artifact_dir: artifact_dir.into(),
        }
    }

    pub fn with_mode(mode: CiMode, artifact_dir: impl Into<PathBuf>) -> Self {
        Self {
            mode,
            artifact_dir: artifact_dir.into(),
        }
    }

    /// True when there is no CI provider to report to.
    pub fn is_inactive(&self) -> bool {
        self.mode == CiMode::None
    }
}

#[async_trait]
impl Reporter for CiReporter {
    async fn on_regression(&self, result: &RegressionResult) -> Result<()> {
        if self.mode == CiMode::None {
            return Ok(());
        }

        let epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        print!("{}", ci_log(self.mode, result, epoch));

        let report = markdown::regression_report(result);
        let artifact = self
            .artifact_dir
            .join(format!("benchmark-report-{epoch}.md"));
        markdown::write_new(&artifact, &report)?;

        // GitHub renders this file at the top of the job's summary page.
        if self.mode == CiMode::GitHub {
            if let Some(path) = std::env::var_os("GITHUB_STEP_SUMMARY") {
                markdown::append(std::path::Path::new(&path), &report)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::baseline::MetricRegression;

    fn result(passed: bool) -> RegressionResult {
        RegressionResult {
            baseline: "main".into(),
            metrics: vec![MetricRegression {
                name: "mean".into(),
                baseline: 10.0,
                current: 12.0,
                change: 0.2,
                threshold: 0.10,
                regressed: !passed,
            }],
            passed,
        }
    }

    #[test]
    fn gitlab_output_is_wrapped_in_matching_section_markers() {
        let text = ci_log(CiMode::GitLab, &result(false), 1_700_000_000);
        assert!(text.contains("section_start:1700000000:benchmark_results"));
        assert!(text.contains("section_end:1700000000:benchmark_results"));
        assert!(text.contains('\x1b'), "GitLab renders ANSI colour");
    }

    #[test]
    fn github_output_is_plain_and_unsectioned() {
        let text = ci_log(CiMode::GitHub, &result(false), 1_700_000_000);
        assert!(!text.contains("section_start"));
        assert!(!text.contains('\x1b'));
        assert!(text.contains("========== Benchmark Regression Check =========="));
    }

    #[test]
    fn metric_lines_carry_the_full_before_after_delta() {
        let text = ci_log(CiMode::GitHub, &result(false), 0);
        assert!(text.contains("  mean: 10.00ms -> 12.00ms (+20.0%) [REGRESSED]"));
        assert!(text.contains("[FAIL] Benchmark FAILED - regression detected"));
    }

    #[test]
    fn passing_runs_say_pass() {
        let text = ci_log(CiMode::GitHub, &result(true), 0);
        assert!(text.contains("[ok]"));
        assert!(text.contains("[PASS] Benchmark passed - no regressions detected"));
    }

    #[tokio::test]
    async fn outside_ci_the_reporter_writes_nothing() {
        let dir = std::env::temp_dir().join("gauntlet-ci-inactive-test");
        let _ = std::fs::remove_dir_all(&dir);

        let reporter = CiReporter::with_mode(CiMode::None, &dir);
        assert!(reporter.is_inactive());
        reporter.on_regression(&result(false)).await.unwrap();

        assert!(!dir.exists(), "no artifact directory is created");
    }

    #[tokio::test]
    async fn in_ci_a_markdown_artifact_is_written() {
        let dir =
            std::env::temp_dir().join(format!("gauntlet-ci-artifact-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);

        CiReporter::with_mode(CiMode::GitLab, &dir)
            .on_regression(&result(false))
            .await
            .unwrap();

        let written: Vec<_> = std::fs::read_dir(&dir).unwrap().flatten().collect();
        assert_eq!(written.len(), 1);
        let contents = std::fs::read_to_string(written[0].path()).unwrap();
        assert!(contents.contains("## Regression Check vs `main`"));

        let _ = std::fs::remove_dir_all(&dir);
    }
}
