//! Adapts the engine's measurements into the report model.
//!
//! This glue lives in the binary on purpose (ADR M4-report §3): it lets
//! `gauntlet-report` stay free of an engine dependency, so a change to the
//! measurement loop does not recompile every renderer.
//!
//! Two things happen here that the engine and the stats crate cannot each do
//! alone: a target's endpoints are flattened into one latency population, and
//! Earth Mover's Distance is attached to every pairwise comparison — EMD needs
//! the raw duration vectors, which `compare_bayesian` never sees.

use std::time::SystemTime;

use gauntlet_engine::{BenchmarkRun, TargetResult};
use gauntlet_report::{BenchmarkReport, PairComparison, Sample, TargetReport};
use gauntlet_stats::{all_pair_comparisons, calculate_stats, earth_movers_distance};

/// Build the report model from a finished run.
pub fn to_report(run: &BenchmarkRun) -> BenchmarkReport {
    let targets: Vec<TargetReport> = run.targets.iter().map(to_target_report).collect();

    let stats: Vec<_> = targets.iter().map(|t| t.stats.clone()).collect();
    let comparisons = all_pair_comparisons(&stats)
        .into_iter()
        .map(|(i, j, mut comparison)| {
            // EMD compares whole distributions, so it needs the samples rather
            // than the summary statistics the Bayesian comparison works from.
            comparison.emd = Some(earth_movers_distance(
                &targets[i].latencies(),
                &targets[j].latencies(),
            ));
            PairComparison {
                a: targets[i].name.clone(),
                b: targets[j].name.clone(),
                comparison,
            }
        })
        .collect();

    BenchmarkReport {
        targets,
        comparisons,
    }
}

/// One target's endpoints collapse into a single latency population: the
/// per-target statistics and the A/B comparison are about the target as a
/// whole, not about any one payload.
fn to_target_report(target: &TargetResult) -> TargetReport {
    let mut validation = Vec::new();
    let mut outcomes = Vec::new();

    for endpoint in &target.endpoints {
        outcomes.extend(endpoint.outcomes.iter());
        if endpoint.validation.total > 0 {
            validation.push(endpoint.validation.clone());
        }
    }

    // Order by issue time so time-series charts read left to right; endpoints
    // run concurrently, so collection order is arrival order, not clock order.
    outcomes.sort_by_key(|o| o.requested_at);
    let origin = outcomes.first().map(|o| o.requested_at);

    let samples: Vec<Sample> = outcomes
        .iter()
        .map(|o| Sample {
            latency_ms: o.response.latency_ms(),
            status: o.response.status,
            offset_s: seconds_between(origin, o.requested_at),
        })
        .collect();

    let latencies: Vec<f64> = samples.iter().filter_map(|s| s.latency_ms).collect();

    TargetReport {
        name: target.name.clone(),
        url: target
            .endpoints
            .first()
            .map(|e| e.url.clone())
            .unwrap_or_default(),
        stats: calculate_stats(samples.len(), &latencies),
        samples,
        validation,
    }
}

/// Seconds from the run's first request to `at`. A clock that went backwards
/// mid-run yields 0 rather than a negative offset that would plot off-axis.
fn seconds_between(origin: Option<SystemTime>, at: SystemTime) -> f64 {
    origin
        .and_then(|origin| at.duration_since(origin).ok())
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    use gauntlet_core::{HttpMethod, Nanoseconds, TestingResponse, ValidationSummary};
    use gauntlet_engine::exec::RequestOutcome;
    use gauntlet_engine::EndpointResult;

    /// A fixed origin so `offset_s` is deterministic across runs.
    fn origin() -> SystemTime {
        SystemTime::UNIX_EPOCH + Duration::from_secs(1_700_000_000)
    }

    /// One outcome issued `at_s` seconds into the run.
    fn response(ms: f64, failed: bool) -> RequestOutcome {
        outcome_at(ms, failed, 0.0)
    }

    fn outcome_at(ms: f64, failed: bool, at_s: f64) -> RequestOutcome {
        RequestOutcome {
            response: TestingResponse {
                duration: Nanoseconds((ms * 1_000_000.0) as u64),
                status: if failed { 500 } else { 200 },
                error: failed.then(|| "boom".to_string()),
            },
            requested_at: origin() + Duration::from_secs_f64(at_s),
            validation_errors: Vec::new(),
        }
    }

    fn endpoint(name: &str, outcomes: Vec<RequestOutcome>) -> EndpointResult {
        EndpointResult {
            name: name.into(),
            url: format!("https://example.test/{name}"),
            method: HttpMethod::Get,
            stats: Default::default(),
            validation: ValidationSummary::default(),
            outcomes,
        }
    }

    fn target(name: &str, endpoints: Vec<EndpointResult>) -> TargetResult {
        TargetResult {
            name: name.into(),
            endpoints,
        }
    }

    #[test]
    fn a_targets_endpoints_collapse_into_one_latency_population() {
        let run = BenchmarkRun {
            targets: vec![target(
                "api",
                vec![
                    endpoint("small", vec![response(10.0, false), response(20.0, false)]),
                    endpoint("large", vec![response(30.0, false)]),
                ],
            )],
        };

        let report = to_report(&run);
        assert_eq!(report.targets.len(), 1);
        assert_eq!(report.targets[0].latencies(), [10.0, 20.0, 30.0]);
        assert_eq!(report.targets[0].stats.total_requests, 3);
        assert_eq!(report.targets[0].stats.mean_ms, 20.0);
    }

    #[test]
    fn failed_responses_count_as_requests_but_not_as_latency_samples() {
        let run = BenchmarkRun {
            targets: vec![target(
                "api",
                vec![endpoint(
                    "p",
                    vec![
                        response(10.0, false),
                        response(999.0, true),
                        response(20.0, false),
                    ],
                )],
            )],
        };

        let report = to_report(&run);
        assert_eq!(report.targets[0].latencies(), [10.0, 20.0]);
        assert_eq!(
            report.targets[0].samples.len(),
            3,
            "the failure is retained as a sample"
        );
        assert_eq!(report.targets[0].stats.total_requests, 3);
        assert_eq!(report.targets[0].stats.count_success, 2);
        assert_eq!(report.targets[0].stats.count_failure, 1);
        assert_eq!(
            report.targets[0].stats.mean_ms, 15.0,
            "a failed request must not drag the mean toward its timeout"
        );
    }

    #[test]
    fn a_single_target_run_produces_no_comparisons() {
        let run = BenchmarkRun {
            targets: vec![target(
                "api",
                vec![endpoint("p", vec![response(10.0, false)])],
            )],
        };
        assert!(to_report(&run).comparisons.is_empty());
    }

    #[test]
    fn every_pair_gets_an_emd_attached() {
        let run = BenchmarkRun {
            targets: vec![
                target("a", vec![endpoint("p", vec![response(10.0, false)])]),
                target("b", vec![endpoint("p", vec![response(20.0, false)])]),
                target("c", vec![endpoint("p", vec![response(30.0, false)])]),
            ],
        };

        let report = to_report(&run);
        assert_eq!(report.comparisons.len(), 3, "3 targets => 3 pairs");
        assert!(report
            .comparisons
            .iter()
            .all(|p| p.comparison.emd.is_some()));

        let ab = &report.comparisons[0];
        assert_eq!((ab.a.as_str(), ab.b.as_str()), ("a", "b"));
        assert_eq!(ab.comparison.emd, Some(10.0));
    }

    #[test]
    fn endpoints_without_assertions_contribute_no_validation_noise() {
        let run = BenchmarkRun {
            targets: vec![target(
                "api",
                vec![endpoint("p", vec![response(1.0, false)])],
            )],
        };
        assert!(to_report(&run).targets[0].validation.is_empty());
    }

    #[test]
    fn an_empty_run_is_an_empty_report_rather_than_a_panic() {
        let report = to_report(&BenchmarkRun { targets: vec![] });
        assert!(report.targets.is_empty());
        assert!(report.comparisons.is_empty());
    }
}
