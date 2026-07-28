//! Builds the active reporter set from CLI flags — the "add an output format"
//! seam. See the crate root.

use gauntlet_report::{
    ChartReporter, CiReporter, HtmlReporter, JUnitReporter, MarkdownReporter, MultiReporter,
    PrometheusReporter, Reporter, TerminalReporter,
};

use crate::cli::BenchmarkArgs;

/// Assemble the reporters a benchmark run should fan out to.
pub fn for_benchmark(args: &BenchmarkArgs) -> MultiReporter {
    let mut reporters: Vec<Box<dyn Reporter>> = vec![Box::new(TerminalReporter::auto())];

    if let Some(path) = &args.markdown_report {
        reporters.push(Box::new(MarkdownReporter::new(path)));
    }
    if let Some(path) = &args.junit_report {
        reporters.push(Box::new(JUnitReporter::new(path)));
    }
    if let Some(path) = &args.html_report {
        // `--charts` selects kinds for every renderer that draws, not just the
        // standalone files. Unset, the HTML keeps its histogram + CDF default.
        reporters.push(Box::new(if args.charts.is_empty() {
            HtmlReporter::new(path)
        } else {
            HtmlReporter::with_charts(path, args.charts.clone())
        }));
    }
    if let Some(prometheus) = prometheus_reporter(args) {
        reporters.push(Box::new(prometheus));
    }
    if !args.charts.is_empty() {
        reporters.push(Box::new(ChartReporter::with_kinds(
            &args.charts_dir,
            args.charts.clone(),
        )));
    }

    // Only carries weight inside CI; a no-op locally.
    let ci = CiReporter::detect(&args.results_dir);
    if !ci.is_inactive() {
        reporters.push(Box::new(ci));
    }

    MultiReporter::new(reporters)
}

/// Prometheus can write a file, push to a gateway, or both.
fn prometheus_reporter(args: &BenchmarkArgs) -> Option<PrometheusReporter> {
    match (&args.prometheus_file, &args.prometheus_pushgateway) {
        (None, None) => None,
        (Some(path), None) => Some(PrometheusReporter::to_file(path)),
        (None, Some(url)) => Some(PrometheusReporter::to_pushgateway(
            url,
            &args.prometheus_job,
        )),
        (Some(path), Some(url)) => {
            Some(PrometheusReporter::to_file(path).with_pushgateway(url, &args.prometheus_job))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    use crate::cli::{Cli, Command};

    fn args(extra: &[&str]) -> BenchmarkArgs {
        let mut argv = vec!["gauntlet", "benchmark", "-c", "cfg.json"];
        argv.extend_from_slice(extra);
        match Cli::try_parse_from(argv).expect("arguments parse").command {
            Command::Benchmark(a) => *a,
            _ => unreachable!("benchmark subcommand"),
        }
    }

    /// The CI reporter self-activates from the environment, so a bare run has
    /// either 1 reporter (local) or 2 (inside CI). Counting the *extra* ones
    /// keeps these assertions stable wherever the suite runs.
    fn extra_reporters(extra: &[&str]) -> usize {
        for_benchmark(&args(extra)).len() - for_benchmark(&args(&[])).len()
    }

    #[test]
    fn a_bare_run_still_reports_to_the_terminal() {
        assert!(!for_benchmark(&args(&[])).is_empty());
    }

    #[test]
    fn each_output_flag_adds_exactly_one_reporter() {
        assert_eq!(extra_reporters(&["--markdown-report", "r.md"]), 1);
        assert_eq!(extra_reporters(&["--junit-report", "r.xml"]), 1);
        assert_eq!(extra_reporters(&["--html-report", "r.html"]), 1);
        assert_eq!(extra_reporters(&["--charts", "histogram"]), 1);
    }

    #[test]
    fn flags_compose() {
        assert_eq!(
            extra_reporters(&[
                "--markdown-report",
                "r.md",
                "--junit-report",
                "r.xml",
                "--html-report",
                "r.html",
                "--charts",
                "histogram,cdf",
            ]),
            4
        );
    }

    #[test]
    fn prometheus_file_and_gateway_share_one_reporter() {
        assert_eq!(extra_reporters(&["--prometheus-file", "m.prom"]), 1);
        assert_eq!(
            extra_reporters(&["--prometheus-pushgateway", "http://localhost:9091"]),
            1
        );
        assert_eq!(
            extra_reporters(&[
                "--prometheus-file",
                "m.prom",
                "--prometheus-pushgateway",
                "http://localhost:9091",
            ]),
            1,
            "one backend does both jobs"
        );
    }

    #[test]
    fn charts_are_off_unless_asked_for() {
        assert!(args(&[]).charts.is_empty());
        assert_eq!(extra_reporters(&[]), 0);
    }
}
