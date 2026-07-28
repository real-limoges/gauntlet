//! The data model every reporter renders. A single-target run is just a report
//! with one target and no comparisons. See the crate docs.

use gauntlet_core::ValidationSummary;
use gauntlet_stats::{BayesianComparison, BenchmarkStats};

/// One measured request, as charts need to see it — deliberately richer than a
/// bare latency. See the crate docs.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Sample {
    /// Latency in milliseconds, or `None` when the request failed. Failed
    /// requests are excluded from latency math but are exactly what the error
    /// and status charts are about, so they are kept.
    pub latency_ms: Option<f64>,
    /// HTTP status, or 0 when the request never got a response.
    pub status: u16,
    /// Seconds since the run started. Relative rather than absolute so charts
    /// can bin by time without doing `SystemTime` arithmetic, and so a report
    /// renders identically wherever it is opened.
    pub offset_s: f64,
}

impl Sample {
    /// True when the request completed without a transport error and returned a
    /// non-error status. Deliberately **not** the same predicate as
    /// `latency_ms.is_some()` — see the crate docs.
    pub fn is_success(&self) -> bool {
        self.latency_ms.is_some() && self.status < 400
    }
}

/// One target's contribution to a report: its statistics, the per-request
/// samples charts consume, and the per-endpoint validation summaries.
#[derive(Clone, Debug, Default)]
pub struct TargetReport {
    pub name: String,
    pub url: String,
    pub stats: BenchmarkStats,
    /// Every request's outcome, in completion order. Charts read these
    /// directly; the CSV round-trip existed only for the old Python script.
    pub samples: Vec<Sample>,
    pub validation: Vec<ValidationSummary>,
}

impl TargetReport {
    /// A target with just a name and stats — the common shape in tests and in
    /// single-target runs that skip validation.
    pub fn new(name: impl Into<String>, stats: BenchmarkStats) -> Self {
        Self {
            name: name.into(),
            stats,
            ..Default::default()
        }
    }

    /// Successful-response latencies in milliseconds — the population the
    /// statistics and EMD are computed over.
    pub fn latencies(&self) -> Vec<f64> {
        self.samples.iter().filter_map(|s| s.latency_ms).collect()
    }

    /// Wall-clock span the samples cover, in seconds. Zero when there are fewer
    /// than two samples, which time-series charts treat as "nothing to plot".
    pub fn duration_s(&self) -> f64 {
        match (self.samples.first(), self.samples.last()) {
            (Some(first), Some(last)) => (last.offset_s - first.offset_s).max(0.0),
            _ => 0.0,
        }
    }
}

/// One pairwise Bayesian comparison. `a` is the primary, `b` the candidate;
/// the comparison reads "B versus A".
#[derive(Clone, Debug)]
pub struct PairComparison {
    pub a: String,
    pub b: String,
    pub comparison: BayesianComparison,
}

/// A finished benchmark, ready to render.
#[derive(Clone, Debug, Default)]
pub struct BenchmarkReport {
    pub targets: Vec<TargetReport>,
    pub comparisons: Vec<PairComparison>,
}

impl BenchmarkReport {
    /// A report for one target and no comparisons.
    pub fn single(target: TargetReport) -> Self {
        Self {
            targets: vec![target],
            comparisons: Vec::new(),
        }
    }

    /// True when there is nothing to compare against — renderers drop their
    /// ranking and pairwise sections.
    pub fn is_single(&self) -> bool {
        self.targets.len() <= 1
    }

    /// Targets ordered by mean latency, fastest first.
    pub fn ranked(&self) -> Vec<&TargetReport> {
        let mut ranked: Vec<&TargetReport> = self.targets.iter().collect();
        ranked.sort_by(|x, y| {
            x.stats
                .mean_ms
                .partial_cmp(&y.stats.mean_ms)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        ranked
    }

    /// Look up a target by name.
    pub fn target(&self, name: &str) -> Option<&TargetReport> {
        self.targets.iter().find(|t| t.name == name)
    }

    /// Stats for a named target, or zeroes when the name is unknown, so a
    /// malformed pair list degrades to an empty row rather than panicking.
    pub fn stats_for(&self, name: &str) -> BenchmarkStats {
        self.target(name)
            .map(|t| t.stats.clone())
            .unwrap_or_default()
    }

    /// Every validation summary across every target, in order.
    pub fn validations(&self) -> impl Iterator<Item = &ValidationSummary> {
        self.targets.iter().flat_map(|t| t.validation.iter())
    }

    /// Total responses validated and total failed, across all targets.
    pub fn validation_totals(&self) -> (usize, usize) {
        self.validations().fold((0, 0), |(total, failed), v| {
            (total + v.total, failed + v.failed)
        })
    }

    /// True when no endpoint declared a `validate` block — reporters skip the
    /// validation section entirely rather than printing an empty table.
    pub fn has_validation(&self) -> bool {
        self.validations().next().is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stats(mean: f64) -> BenchmarkStats {
        BenchmarkStats {
            mean_ms: mean,
            ..Default::default()
        }
    }

    #[test]
    fn ranked_orders_by_mean_fastest_first() {
        let report = BenchmarkReport {
            targets: vec![
                TargetReport::new("slow", stats(30.0)),
                TargetReport::new("fast", stats(10.0)),
                TargetReport::new("mid", stats(20.0)),
            ],
            comparisons: Vec::new(),
        };

        let names: Vec<&str> = report.ranked().iter().map(|t| t.name.as_str()).collect();
        assert_eq!(names, ["fast", "mid", "slow"]);
    }

    #[test]
    fn ranked_tolerates_nan_means() {
        let report = BenchmarkReport {
            targets: vec![
                TargetReport::new("nan", stats(f64::NAN)),
                TargetReport::new("real", stats(5.0)),
            ],
            comparisons: Vec::new(),
        };

        assert_eq!(report.ranked().len(), 2);
    }

    #[test]
    fn stats_for_unknown_target_is_zeroed() {
        let report = BenchmarkReport::single(TargetReport::new("a", stats(1.0)));
        assert_eq!(report.stats_for("nope"), BenchmarkStats::default());
    }

    #[test]
    fn single_report_has_no_comparisons() {
        let report = BenchmarkReport::single(TargetReport::new("a", stats(1.0)));
        assert!(report.is_single());
        assert!(!report.has_validation());
    }

    #[test]
    fn validation_totals_sum_across_targets() {
        let summary = |total, failed| ValidationSummary {
            total,
            failed,
            errors: Vec::new(),
        };
        let mut a = TargetReport::new("a", stats(1.0));
        a.validation = vec![summary(10, 1), summary(5, 0)];
        let mut b = TargetReport::new("b", stats(2.0));
        b.validation = vec![summary(10, 3)];

        let report = BenchmarkReport {
            targets: vec![a, b],
            comparisons: Vec::new(),
        };
        assert!(report.has_validation());
        assert_eq!(report.validation_totals(), (25, 4));
    }
}
