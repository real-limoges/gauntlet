//! Argument parsing. The flag surface is documented in `README.md`; see the
//! crate root for the decisions behind `--charts` and the live view.

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use gauntlet_report::ChartKind;

#[derive(Debug, Parser)]
#[command(
    name = "gauntlet",
    about = "Statistically rigorous HTTP performance benchmarking",
    version
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Run a benchmark (single- or multi-target, per the config).
    // Boxed because `BenchmarkArgs` dwarfs the other variants and would pad the
    // enum. Not a doc comment: clap renders those as user-facing help.
    Benchmark(Box<BenchmarkArgs>),

    /// Compare two saved result files offline, with no requests.
    Compare {
        /// Baseline or stats JSON for the primary (A).
        file_a: PathBuf,
        /// Baseline or stats JSON for the candidate (B).
        file_b: PathBuf,
    },

    /// Parse and check a config without sending any requests.
    Validate {
        /// Path to the benchmark configuration file.
        #[arg(short, long, value_name = "FILE")]
        config: PathBuf,

        /// Also issue one HTTP request per target's health-check URL.
        #[arg(long)]
        check_endpoints: bool,
    },

    /// Print the config JSON schema.
    Schema {
        /// Write to a file instead of stdout.
        #[arg(long, value_name = "FILE")]
        out: Option<PathBuf>,
    },
}

#[derive(Debug, Args)]
pub struct BenchmarkArgs {
    /// Path to the benchmark configuration file.
    #[arg(short, long, value_name = "FILE")]
    pub config: PathBuf,

    /// Save this run's stats as a named baseline.
    #[arg(long, value_name = "NAME")]
    pub save_baseline: Option<String>,

    /// Compare against a named baseline; exits 1 on regression.
    #[arg(long, value_name = "NAME")]
    pub compare_baseline: Option<String>,

    /// Directory holding saved baselines.
    #[arg(long, value_name = "DIR", default_value = gauntlet_report::DEFAULT_BASELINE_DIR)]
    pub baseline_dir: PathBuf,

    /// Write a markdown report.
    #[arg(long, value_name = "FILE")]
    pub markdown_report: Option<PathBuf>,

    /// Write a JUnit XML report.
    #[arg(long, value_name = "FILE")]
    pub junit_report: Option<PathBuf>,

    /// Write a self-contained HTML report.
    #[arg(long, value_name = "FILE")]
    pub html_report: Option<PathBuf>,

    /// Write Prometheus exposition text.
    #[arg(long, value_name = "FILE")]
    pub prometheus_file: Option<PathBuf>,

    /// Push metrics to a Prometheus Pushgateway.
    #[arg(long, value_name = "URL")]
    pub prometheus_pushgateway: Option<String>,

    /// Job name used when pushing to the Pushgateway.
    #[arg(long, value_name = "NAME", default_value = "gauntlet")]
    pub prometheus_job: String,

    /// Charts to render, comma-separated: histogram, cdf, tail, timeline,
    /// rolling_pct, boxplot, throughput, error_rate, status.
    //
    // Spelled out because clap needs a 'static help string;
    // `the_charts_help_lists_every_kind` fails if it drifts from ChartKind::ALL.
    #[arg(long, value_name = "KINDS", value_delimiter = ',')]
    pub charts: Vec<ChartKind>,

    /// Directory for rendered charts.
    #[arg(long, value_name = "DIR", default_value = "results/charts")]
    pub charts_dir: PathBuf,

    /// Directory for the latency CSV and CI artifacts.
    #[arg(long, value_name = "DIR", default_value = "results")]
    pub results_dir: PathBuf,

    /// Skip writing the per-request latency CSV.
    #[arg(long)]
    pub no_csv: bool,

    /// Force the headless view even on an interactive terminal.
    #[arg(long)]
    pub no_tui: bool,
}

/// What the baseline flags add up to. The two are independently optional and
/// combine, so one run can save a baseline and compare against another.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BaselineMode {
    None,
    Save(String),
    Compare(String),
    SaveAndCompare { save: String, compare: String },
}

impl BenchmarkArgs {
    pub fn baseline_mode(&self) -> BaselineMode {
        match (&self.save_baseline, &self.compare_baseline) {
            (None, None) => BaselineMode::None,
            (Some(save), None) => BaselineMode::Save(save.clone()),
            (None, Some(compare)) => BaselineMode::Compare(compare.clone()),
            (Some(save), Some(compare)) => BaselineMode::SaveAndCompare {
                save: save.clone(),
                compare: compare.clone(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;

    fn parse(args: &[&str]) -> Cli {
        Cli::try_parse_from(args).expect("arguments parse")
    }

    #[test]
    fn the_command_definition_is_internally_consistent() {
        // Catches duplicate flags, bad defaults, and conflicting short options.
        Cli::command().debug_assert();
    }

    #[test]
    fn benchmark_requires_a_config() {
        assert!(Cli::try_parse_from(["gauntlet", "benchmark"]).is_err());
    }

    #[test]
    fn baseline_flags_combine_into_the_four_modes() {
        let mode = |args: &[&str]| match parse(args).command {
            Command::Benchmark(a) => a.baseline_mode(),
            _ => unreachable!("benchmark subcommand"),
        };
        let base = ["gauntlet", "benchmark", "-c", "cfg.json"];

        assert_eq!(mode(&base), BaselineMode::None);
        assert_eq!(
            mode(&[&base[..], &["--save-baseline", "main"]].concat()),
            BaselineMode::Save("main".into())
        );
        assert_eq!(
            mode(&[&base[..], &["--compare-baseline", "main"]].concat()),
            BaselineMode::Compare("main".into())
        );
        assert_eq!(
            mode(
                &[
                    &base[..],
                    &["--save-baseline", "new", "--compare-baseline", "old"]
                ]
                .concat()
            ),
            BaselineMode::SaveAndCompare {
                save: "new".into(),
                compare: "old".into()
            }
        );
    }

    #[test]
    fn charts_parse_as_a_comma_separated_list_of_kinds() {
        let args = match parse(&[
            "gauntlet",
            "benchmark",
            "-c",
            "cfg.json",
            "--charts",
            "histogram,cdf",
        ])
        .command
        {
            Command::Benchmark(a) => *a,
            _ => unreachable!(),
        };
        assert_eq!(args.charts, [ChartKind::Histogram, ChartKind::Cdf]);
    }

    #[test]
    fn the_charts_help_lists_every_kind() {
        let help = Cli::command()
            .find_subcommand_mut("benchmark")
            .expect("benchmark subcommand")
            .render_long_help()
            .to_string();

        for kind in ChartKind::ALL {
            assert!(
                help.contains(kind.as_str()),
                "--charts help omits {kind}; update the doc comment on `charts`"
            );
        }
    }

    #[test]
    fn no_implementation_notes_leak_into_user_facing_help() {
        // clap renders doc comments as help; implementation notes belong in
        // plain comments. This caught "Boxed because BenchmarkArgs..." shipping
        // in `gauntlet benchmark --help`.
        let help = Cli::command().render_long_help().to_string();
        for leak in ["Boxed because", "clippy", "#[", "TODO"] {
            assert!(!help.contains(leak), "help text contains {leak:?}");
        }
    }

    #[test]
    fn an_unknown_chart_kind_fails_argument_parsing() {
        let err = Cli::try_parse_from([
            "gauntlet",
            "benchmark",
            "-c",
            "cfg.json",
            "--charts",
            "violin",
        ])
        .expect_err("unknown chart kind is rejected up front");
        assert!(err.to_string().contains("violin"));
    }

    #[test]
    fn compare_takes_two_positional_files() {
        match parse(&["gauntlet", "compare", "a.json", "b.json"]).command {
            Command::Compare { file_a, file_b } => {
                assert_eq!(file_a, PathBuf::from("a.json"));
                assert_eq!(file_b, PathBuf::from("b.json"));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn schema_writes_to_stdout_unless_out_is_given() {
        match parse(&["gauntlet", "schema"]).command {
            Command::Schema { out } => assert!(out.is_none()),
            _ => unreachable!(),
        }
        match parse(&["gauntlet", "schema", "--out", "s.json"]).command {
            Command::Schema { out } => assert_eq!(out, Some(PathBuf::from("s.json"))),
            _ => unreachable!(),
        }
    }

    #[test]
    fn validate_defaults_to_not_touching_the_network() {
        match parse(&["gauntlet", "validate", "-c", "cfg.json"]).command {
            Command::Validate {
                check_endpoints, ..
            } => assert!(!check_endpoints),
            _ => unreachable!(),
        }
    }
}
