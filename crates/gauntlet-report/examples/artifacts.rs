//! Renders every artifact for a synthetic two-target run, so the whole reporter
//! set can be exercised and eyeballed before the CLI exists (M6).
//!
//! Run: `cargo run -p gauntlet-report --example artifacts -- <out-dir>`

use gauntlet_report::{
    baseline::{compare_to_baseline, Baseline, RegressionThresholds},
    BenchmarkReport, ChartReporter, HtmlReporter, JUnitReporter, MarkdownReporter, MultiReporter,
    PairComparison, PrometheusReporter, Reporter, Sample, TargetReport,
};
use gauntlet_stats::{all_pair_comparisons, calculate_stats};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let dir = std::env::args().nth(1).unwrap_or_else(|| "out".into());
    let dir = std::path::PathBuf::from(dir);

    // Two synthetic runs: the candidate is faster but noisier, and drops a
    // handful of requests late on so the error-rate and status charts have
    // something to say.
    let synth = |base: f64, spread: f64, period: usize, fail_from: usize| -> Vec<Sample> {
        (0..500)
            .map(|i| {
                let failed = i >= fail_from && i % 7 == 0;
                Sample {
                    latency_ms: (!failed).then(|| base + (i % period) as f64 * spread),
                    status: if failed { 503 } else { 200 },
                    offset_s: i as f64 * 0.06,
                }
            })
            .collect()
    };

    let mut targets = Vec::new();
    for (name, samples) in [
        ("baseline-api", synth(20.0, 0.4, 17, usize::MAX)),
        ("candidate-api", synth(14.0, 0.9, 29, 380)),
    ] {
        let latencies: Vec<f64> = samples.iter().filter_map(|s| s.latency_ms).collect();
        targets.push(TargetReport {
            name: name.into(),
            url: format!("https://example.test/{name}"),
            stats: calculate_stats(samples.len(), &latencies),
            samples,
            validation: Vec::new(),
        });
    }

    let stats: Vec<_> = targets.iter().map(|t| t.stats.clone()).collect();
    let comparisons = all_pair_comparisons(&stats)
        .into_iter()
        .map(|(i, j, comparison)| PairComparison {
            a: targets[i].name.clone(),
            b: targets[j].name.clone(),
            comparison,
        })
        .collect();

    let report = BenchmarkReport {
        targets,
        comparisons,
    };

    let reporters = MultiReporter::new(vec![
        Box::new(MarkdownReporter::new(dir.join("report.md"))),
        Box::new(JUnitReporter::new(dir.join("junit.xml"))),
        Box::new(PrometheusReporter::to_file(dir.join("metrics.prom"))),
        Box::new(HtmlReporter::new(dir.join("report.html"))),
        Box::new(ChartReporter::new(dir.join("charts"))),
    ]);

    reporters.on_benchmark(&report).await?;

    // A failing baseline check, to exercise the regression path and exit code.
    let saved = Baseline::capture("main", "2026-07-21T12:00:00Z", &report.targets[0].stats);
    let regression = compare_to_baseline(
        &RegressionThresholds::default(),
        &saved,
        &report.targets[1].stats,
    );
    reporters.on_regression(&regression).await?;

    println!("wrote artifacts to {}", dir.display());
    println!("regression passed: {}", regression.passed);
    Ok(())
}
