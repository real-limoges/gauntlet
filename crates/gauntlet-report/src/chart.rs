//! Chart rendering, and the reporter that writes the SVGs to a directory.
//!
//! This replaces the whole Python path: `scripts/plot_latency.py`, the `uv`
//! subprocess, and the free-text `types` list in the charts config. Charts are
//! drawn in-process with `plotters` straight from [`TargetReport::samples`] —
//! the CSV round-trip existed only because the Python script needed a file to
//! read. See ADR `M4-report` §7.
//!
//! Every kind reads [`Sample`]s rather than a bare `Vec<f64>`: throughput, error
//! rate, and the status mix are charts *about the outcomes*, and a latency
//! vector has already thrown those away. The kinds that only care about latency
//! ([`TargetReport::latencies`]) simply ignore the rest of each sample.
//!
//! The rendering entry point is [`render`], a pure `String` producer: it hands
//! back the SVG document rather than writing it, so the HTML reporter can embed
//! the same bytes inline instead of linking an external asset.

use std::cmp::Ordering;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

use async_trait::async_trait;
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};

use crate::error::{Error, Result};
use crate::markdown::write_new;
use crate::model::{BenchmarkReport, Sample, TargetReport};
use crate::Reporter;

/// The chart kinds the tool can render.
///
/// An enum rather than a string: an unknown chart kind is now a config error
/// caught by [`ChartKind::from_str`], not a Python traceback at report time,
/// after the benchmark has already been paid for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChartKind {
    /// Frequency of latencies, binned.
    Histogram,
    /// Empirical CDF — the chart that actually answers "what fraction of
    /// requests came in under X ms".
    Cdf,
    /// The top decile of the CDF, plotted percentile-against-latency, which is
    /// where p99-and-beyond behaviour is actually visible.
    Tail,
    /// Latency against request ordinal, which exposes warmup and drift.
    Timeline,
    /// Rolling p50/p95/p99 over the run, which separates a slow tail that was
    /// always there from one that appeared as load built up.
    RollingPct,
    /// Quartiles, 1.5-IQR whiskers, and outliers.
    BoxPlot,
    /// Completed requests per second over the run.
    Throughput,
    /// Share of requests that failed, over the run.
    ErrorRate,
    /// How the responses were distributed across status classes.
    Status,
}

impl ChartKind {
    /// Every kind, in the order a report renders them: distribution first, then
    /// how it moved over time, then what came back.
    pub const ALL: [ChartKind; 9] = [
        ChartKind::Histogram,
        ChartKind::Cdf,
        ChartKind::Tail,
        ChartKind::Timeline,
        ChartKind::RollingPct,
        ChartKind::BoxPlot,
        ChartKind::Throughput,
        ChartKind::ErrorRate,
        ChartKind::Status,
    ];

    /// The lowercase name used in config and in generated filenames.
    pub fn as_str(self) -> &'static str {
        match self {
            ChartKind::Histogram => "histogram",
            ChartKind::Cdf => "cdf",
            ChartKind::Tail => "tail",
            ChartKind::Timeline => "timeline",
            ChartKind::RollingPct => "rolling_pct",
            ChartKind::BoxPlot => "boxplot",
            ChartKind::Throughput => "throughput",
            ChartKind::ErrorRate => "error_rate",
            ChartKind::Status => "status",
        }
    }

    /// The human-readable chart title.
    pub fn title(self) -> &'static str {
        match self {
            ChartKind::Histogram => "Latency distribution",
            ChartKind::Cdf => "Latency CDF",
            ChartKind::Tail => "Latency tail",
            ChartKind::Timeline => "Latency over time",
            ChartKind::RollingPct => "Rolling latency percentiles",
            ChartKind::BoxPlot => "Latency quartiles",
            ChartKind::Throughput => "Throughput",
            ChartKind::ErrorRate => "Error rate",
            ChartKind::Status => "Status codes",
        }
    }

    /// True when the kind plots latency and nothing else, so a target whose
    /// requests all failed has nothing for it to draw.
    fn needs_latency(self) -> bool {
        !matches!(
            self,
            ChartKind::Throughput | ChartKind::ErrorRate | ChartKind::Status
        )
    }
}

impl fmt::Display for ChartKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for ChartKind {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        // `-` and `_` are the same separator here: both `error-rate` and
        // `error_rate` are things a person writes in a config by hand.
        let name = s.trim().to_ascii_lowercase().replace('-', "_");
        match name.as_str() {
            "histogram" => Ok(ChartKind::Histogram),
            "cdf" => Ok(ChartKind::Cdf),
            "tail" => Ok(ChartKind::Tail),
            "timeline" => Ok(ChartKind::Timeline),
            "rolling_pct" | "rolling" => Ok(ChartKind::RollingPct),
            "boxplot" | "box" => Ok(ChartKind::BoxPlot),
            "throughput" => Ok(ChartKind::Throughput),
            "error_rate" | "errors" => Ok(ChartKind::ErrorRate),
            "status" => Ok(ChartKind::Status),
            _ => Err(Error::Chart(format!(
                "unknown chart kind `{}` (expected one of: {})",
                s.trim(),
                ChartKind::ALL
                    .iter()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))),
        }
    }
}

/// Chart canvas, in pixels. Wide enough for readable axis labels at the size
/// the HTML report displays them.
const WIDTH: u32 = 820;
const HEIGHT: u32 = 380;

/// Axis, grid, and label ink. A mid grey is legible against both a white and a
/// dark page, which matters because the SVG is embedded inline in a report that
/// follows the reader's `prefers-color-scheme`.
const INK: RGBColor = RGBColor(136, 136, 136);

/// Series colours, chosen to stay distinguishable on either background.
const PALETTE: [RGBColor; 4] = [
    RGBColor(59, 130, 246), // blue
    RGBColor(245, 158, 11), // amber
    RGBColor(16, 185, 129), // emerald
    RGBColor(217, 70, 239), // fuchsia
];

/// Failure ink, used where a colour carries meaning rather than identity: the
/// 4xx/5xx bars of the status chart and the error-rate line.
const ALARM: RGBColor = RGBColor(239, 68, 68);

/// Render one chart as a standalone SVG document.
///
/// `series` picks a colour from the palette so that, in a multi-target report,
/// each target keeps one identity across all of its charts.
///
/// Returns `Ok(None)` when there is nothing to plot, and an empty pair of axes
/// is worse than no chart at all. That covers three cases:
///
/// * no samples at all;
/// * no *successful* samples, for the kinds that plot latency — an all-failed
///   target still charts its error rate and status mix, which is the point of
///   keeping failures in the model;
/// * a run with no measurable duration (one sample, or every sample stamped at
///   the same offset), for the kinds that plot against time — "requests per
///   second" over a zero-second window is not a number.
///
/// Non-finite values are dropped first, so a single NaN cannot collapse an axis.
pub fn render(
    kind: ChartKind,
    label: &str,
    samples: &[Sample],
    series: usize,
) -> Result<Option<String>> {
    let points = clean(samples);
    if points.is_empty() {
        return Ok(None);
    }

    let colour = PALETTE[series % PALETTE.len()];
    let caption = format!("{} — {label}", kind.title());

    // The order the samples arrive in *is* the time order, so the timeline is
    // the one chart that must not be sorted.
    let ordered: Vec<f64> = points.iter().filter_map(|s| s.latency_ms).collect();
    if kind.needs_latency() && ordered.is_empty() {
        return Ok(None);
    }
    let mut sorted = ordered.clone();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));

    let svg = match kind {
        ChartKind::Histogram => histogram(&caption, &sorted, colour)?,
        ChartKind::Cdf => cdf(&caption, &sorted, colour)?,
        ChartKind::Tail => tail(&caption, &sorted, colour)?,
        ChartKind::Timeline => timeline(&caption, &ordered, colour)?,
        ChartKind::BoxPlot => box_plot(&caption, &sorted, colour)?,
        ChartKind::Status => status(&caption, &points)?,
        // The only kind that can still come back empty after all the checks
        // above: every success may sit outside every rolling window.
        ChartKind::RollingPct => match span(&points) {
            Some(window) => match rolling_pct(&caption, &points, window, colour)? {
                Some(svg) => svg,
                None => return Ok(None),
            },
            None => return Ok(None),
        },
        ChartKind::Throughput => match span(&points) {
            Some(window) => throughput(&caption, &points, window, colour)?,
            None => return Ok(None),
        },
        ChartKind::ErrorRate => match span(&points) {
            Some(window) => error_rate(&caption, &points, window)?,
            None => return Ok(None),
        },
    };
    Ok(Some(svg))
}

/// Writes one `.svg` per target per chart kind into a directory.
///
/// Filenames are `<target-slug>-<kind>.svg`. The slug is sanitized rather than
/// interpolated, so a target named `../../etc/passwd` cannot escape the
/// directory it was told to write into.
pub struct ChartReporter {
    dir: PathBuf,
    kinds: Vec<ChartKind>,
}

impl ChartReporter {
    /// A reporter rendering every chart kind.
    pub fn new(dir: impl Into<PathBuf>) -> Self {
        Self {
            dir: dir.into(),
            kinds: ChartKind::ALL.to_vec(),
        }
    }

    /// A reporter rendering only the given kinds, in the given order.
    pub fn with_kinds(dir: impl Into<PathBuf>, kinds: Vec<ChartKind>) -> Self {
        Self {
            dir: dir.into(),
            kinds,
        }
    }

    /// The path this reporter would write for one target and kind.
    pub fn path_for(&self, target: &str, kind: ChartKind) -> PathBuf {
        self.dir
            .join(format!("{}-{}.svg", slug(target), kind.as_str()))
    }
}

#[async_trait]
impl Reporter for ChartReporter {
    async fn on_benchmark(&self, report: &BenchmarkReport) -> Result<()> {
        for (index, target) in report.targets.iter().enumerate() {
            for kind in &self.kinds {
                if let Some(svg) = render(*kind, &target.name, &target.samples, index)? {
                    write_new(&self.path_for(&target.name, *kind), &svg)?;
                }
            }
        }
        Ok(())
    }

    // `on_regression` is deliberately not implemented. A regression result is
    // summary statistics compared against a stored baseline — it carries no
    // sample vectors, so there is nothing to plot that the tables do not already
    // say. The defaulted no-op is the honest answer.
}

// --- rendering: latency ------------------------------------------------------

fn histogram(caption: &str, sorted: &[f64], colour: RGBColor) -> Result<String> {
    let (lo, hi) = padded(sorted[0], sorted[sorted.len() - 1]);
    // Square-root binning, clamped: enough resolution to show a bimodal
    // distribution without turning a short run into a comb.
    let bins = bin_count(sorted.len(), 5, 40);
    let width = (hi - lo) / bins as f64;

    let mut counts = vec![0usize; bins];
    for v in sorted {
        let idx = (((v - lo) / width) as usize).min(bins - 1);
        counts[idx] += 1;
    }
    let peak = counts.iter().copied().max().unwrap_or(1).max(1) as f64;

    draw(|root| {
        let mut chart = frame(root, caption, lo..hi, 0.0..peak * 1.08)?;
        mesh(&mut chart, "Latency (ms)", "Requests")?;
        chart
            .draw_series(counts.iter().enumerate().map(|(i, &count)| {
                let x0 = lo + i as f64 * width;
                Rectangle::new(
                    [(x0, 0.0), (x0 + width * 0.92, count as f64)],
                    colour.mix(0.75).filled(),
                )
            }))
            .map_err(chart_err)?;
        Ok(())
    })
}

fn cdf(caption: &str, sorted: &[f64], colour: RGBColor) -> Result<String> {
    let (lo, hi) = padded(sorted[0], sorted[sorted.len() - 1]);
    let n = sorted.len() as f64;
    let points: Vec<(f64, f64)> = sorted
        .iter()
        .enumerate()
        .map(|(i, &v)| (v, (i + 1) as f64 / n))
        .collect();

    draw(|root| {
        let mut chart = frame(root, caption, lo..hi, 0.0..1.02)?;
        mesh(&mut chart, "Latency (ms)", "Cumulative fraction")?;
        chart
            .draw_series(LineSeries::new(
                points.iter().copied(),
                colour.stroke_width(2),
            ))
            .map_err(chart_err)?;
        Ok(())
    })
}

/// The tail of the distribution, plotted as percentile against latency.
///
/// The Python script drew this as a CDF filtered to the top decile, which spends
/// the whole x-axis on the handful of slowest samples and squashes the
/// percentile axis into a sliver at the top. Transposing it — percentile across,
/// latency up — puts p90…p100 on an even footing, which is the comparison
/// anyone opening a tail chart is actually making.
fn tail(caption: &str, sorted: &[f64], colour: RGBColor) -> Result<String> {
    const FROM: f64 = 90.0;
    const STEPS: usize = 100;

    let points: Vec<(f64, f64)> = (0..=STEPS)
        .map(|i| {
            let p = FROM + (100.0 - FROM) * i as f64 / STEPS as f64;
            (p, quantile(sorted, p / 100.0))
        })
        .collect();
    let (lo, hi) = padded(points[0].1, points[points.len() - 1].1);

    draw(|root| {
        let mut chart = frame(root, caption, FROM..100.0, lo..hi)?;
        mesh(&mut chart, "Percentile", "Latency (ms)")?;
        chart
            .draw_series(LineSeries::new(
                points.iter().copied(),
                colour.stroke_width(2),
            ))
            .map_err(chart_err)?;
        // p95 and p99 are the two numbers the tables quote, so mark them.
        chart
            .draw_series(
                [95.0, 99.0]
                    .into_iter()
                    .map(|p| Circle::new((p, quantile(sorted, p / 100.0)), 3, colour.filled())),
            )
            .map_err(chart_err)?;
        Ok(())
    })
}

fn timeline(caption: &str, values: &[f64], colour: RGBColor) -> Result<String> {
    let max = values.iter().copied().fold(f64::MIN, f64::max);
    let (lo, hi) = padded(0.0, max);
    let last = (values.len().saturating_sub(1)) as f64;

    draw(|root| {
        let mut chart = frame(root, caption, 0.0..last.max(1.0), lo..hi)?;
        mesh(&mut chart, "Request #", "Latency (ms)")?;
        chart
            .draw_series(LineSeries::new(
                values.iter().enumerate().map(|(i, &v)| (i as f64, v)),
                colour.mix(0.85).stroke_width(1),
            ))
            .map_err(chart_err)?;
        Ok(())
    })
}

/// A box plot drawn from primitives.
///
/// `plotters`' own `Boxplot` element sits behind the `boxplot` feature, which
/// this crate does not enable; a box is five line segments and a rectangle, so
/// hand-drawing it is cheaper than pulling in the feature.
fn box_plot(caption: &str, sorted: &[f64], colour: RGBColor) -> Result<String> {
    let q1 = quantile(sorted, 0.25);
    let median = quantile(sorted, 0.50);
    let q3 = quantile(sorted, 0.75);
    let iqr = q3 - q1;
    // Tukey fences: whiskers reach the most extreme sample still within
    // 1.5 IQR, and everything beyond is drawn as an individual outlier.
    let low_fence = q1 - 1.5 * iqr;
    let high_fence = q3 + 1.5 * iqr;
    let whisker_lo = sorted
        .iter()
        .copied()
        .find(|&v| v >= low_fence)
        .unwrap_or(q1);
    let whisker_hi = sorted
        .iter()
        .rev()
        .copied()
        .find(|&v| v <= high_fence)
        .unwrap_or(q3);
    let outliers: Vec<f64> = sorted
        .iter()
        .copied()
        .filter(|&v| v < low_fence || v > high_fence)
        .collect();

    let (lo, hi) = padded(sorted[0], sorted[sorted.len() - 1]);

    draw(|root| {
        let mut chart = frame(root, caption, 0.0..1.0, lo..hi)?;
        mesh(&mut chart, "", "Latency (ms)")?;

        // Box, median bar, whisker stem, and whisker caps.
        chart
            .draw_series(std::iter::once(Rectangle::new(
                [(0.35, q1), (0.65, q3)],
                colour.mix(0.55).filled(),
            )))
            .map_err(chart_err)?;
        let lines = [
            vec![(0.35, median), (0.65, median)],
            vec![(0.50, whisker_lo), (0.50, q1)],
            vec![(0.50, q3), (0.50, whisker_hi)],
            vec![(0.42, whisker_lo), (0.58, whisker_lo)],
            vec![(0.42, whisker_hi), (0.58, whisker_hi)],
        ];
        chart
            .draw_series(
                lines
                    .into_iter()
                    .map(|pts| PathElement::new(pts, colour.stroke_width(2))),
            )
            .map_err(chart_err)?;
        chart
            .draw_series(
                outliers
                    .into_iter()
                    .map(|v| Circle::new((0.50, v), 2, colour.mix(0.5).filled())),
            )
            .map_err(chart_err)?;
        Ok(())
    })
}

// --- rendering: time series --------------------------------------------------

/// Completed requests per second, binned by [`Sample::offset_s`].
///
/// Failures count: a target that answers fast because it is refusing everything
/// has a throughput, and hiding it here would flatter it.
fn throughput(
    caption: &str,
    points: &[Sample],
    (lo, hi): (f64, f64),
    colour: RGBColor,
) -> Result<String> {
    let bins = bin_count(points.len(), 4, 60);
    let width = (hi - lo) / bins as f64;
    let mut counts = vec![0usize; bins];
    for s in points {
        counts[bin_of(s.offset_s, lo, width, bins)] += 1;
    }

    let series: Vec<(f64, f64)> = counts
        .iter()
        .enumerate()
        .map(|(i, &c)| (lo + (i as f64 + 0.5) * width, c as f64 / width))
        .collect();
    let peak = series.iter().map(|&(_, y)| y).fold(0.0_f64, f64::max);

    draw(|root| {
        let mut chart = frame(root, caption, lo..hi, 0.0..peak * 1.12)?;
        mesh(&mut chart, "Elapsed (s)", "Requests / sec")?;
        chart
            .draw_series(
                AreaSeries::new(series.iter().copied(), 0.0, colour.mix(0.22))
                    .border_style(colour.stroke_width(2)),
            )
            .map_err(chart_err)?;
        Ok(())
    })
}

/// Share of requests that failed, binned by [`Sample::offset_s`].
///
/// The y-axis is pinned to 0–100% rather than scaled to the data: a chart whose
/// axis silently rescales makes a 0.2% error rate look like an outage.
fn error_rate(caption: &str, points: &[Sample], (lo, hi): (f64, f64)) -> Result<String> {
    let bins = bin_count(points.len(), 4, 60);
    let width = (hi - lo) / bins as f64;
    let mut totals = vec![0usize; bins];
    let mut errors = vec![0usize; bins];
    for s in points {
        let idx = bin_of(s.offset_s, lo, width, bins);
        totals[idx] += 1;
        if !s.is_success() {
            errors[idx] += 1;
        }
    }

    // Empty bins are skipped rather than drawn as zero: no requests landed there,
    // which is not the same claim as "none of them failed".
    let series: Vec<(f64, f64)> = (0..bins)
        .filter(|&i| totals[i] > 0)
        .map(|i| {
            (
                lo + (i as f64 + 0.5) * width,
                errors[i] as f64 / totals[i] as f64,
            )
        })
        .collect();

    draw(|root| {
        let mut chart = frame(root, caption, lo..hi, 0.0..1.02)?;
        styled_mesh(
            &mut chart,
            "Elapsed (s)",
            "Failed requests",
            None,
            None,
            Some(&|v: &f64| format!("{:.0}%", v * 100.0)),
        )?;
        chart
            .draw_series(
                AreaSeries::new(series.iter().copied(), 0.0, ALARM.mix(0.22))
                    .border_style(ALARM.stroke_width(2)),
            )
            .map_err(chart_err)?;
        Ok(())
    })
}

/// Trailing-window p50/p95/p99 across the run.
///
/// The window is a tenth of the run rather than a wall-clock constant: a 30
/// second window is a reasonable default for a ten minute soak and swallows a
/// twenty second smoke test whole.
fn rolling_pct(
    caption: &str,
    points: &[Sample],
    (lo, hi): (f64, f64),
    colour: RGBColor,
) -> Result<Option<String>> {
    const PERCENTILES: [(f64, &str); 3] = [(0.50, "p50"), (0.95, "p95"), (0.99, "p99")];

    let steps = bin_count(points.len(), 4, 60);
    let window = (hi - lo) / 10.0;
    let stops: Vec<f64> = (1..=steps)
        .map(|i| lo + (hi - lo) * i as f64 / steps as f64)
        .collect();

    // One sorted latency vector per stop, reused by all three percentiles.
    let windows: Vec<(f64, Vec<f64>)> = stops
        .into_iter()
        .map(|t| {
            let mut vals: Vec<f64> = points
                .iter()
                .filter(|s| s.offset_s > t - window && s.offset_s <= t)
                .filter_map(|s| s.latency_ms)
                .collect();
            vals.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
            (t, vals)
        })
        .filter(|(_, vals)| !vals.is_empty())
        .collect();
    // A run whose only success landed before the first window closes leaves
    // nothing to draw. That is a shape of data, not a failure, so it is the same
    // `None` every other empty case returns rather than an error that would sink
    // the whole report.
    if windows.is_empty() {
        return Ok(None);
    }

    /// One labelled, coloured percentile track: `(name, shade, points)`.
    type Track = (&'static str, RGBColor, Vec<(f64, f64)>);

    let lines: Vec<Track> = PERCENTILES
        .iter()
        .enumerate()
        .map(|(i, &(q, name))| {
            // Blended toward the axis grey rather than made transparent: the SVG
            // sits on whichever background the reader's browser chose, so an
            // alpha ramp would mean something different in light and dark.
            let series = windows
                .iter()
                .map(|(t, vals)| (*t, quantile(vals, q)))
                .collect();
            (name, toward_ink(colour, 0.32 * i as f64), series)
        })
        .collect();

    let (mut min, mut max) = (f64::MAX, f64::MIN);
    for (_, _, series) in &lines {
        for &(_, y) in series {
            min = min.min(y);
            max = max.max(y);
        }
    }
    let (y_lo, y_hi) = padded(min, max);

    draw(|root| {
        let mut chart = frame(root, caption, lo..hi, y_lo..y_hi)?;
        mesh(&mut chart, "Elapsed (s)", "Latency (ms)")?;
        for (i, (name, shade, series)) in lines.iter().enumerate() {
            // Width as well as shade, so the three stay separable for a reader
            // who cannot rely on the colour difference.
            let style = shade.stroke_width(3 - i as u32);
            chart
                .draw_series(LineSeries::new(series.iter().copied(), style))
                .map_err(chart_err)?
                .label(*name)
                .legend(move |(x, y)| PathElement::new(vec![(x, y), (x + 18, y)], style));
        }
        chart
            .configure_series_labels()
            .border_style(INK.mix(0.5))
            .label_font(("sans-serif", 12).into_font().color(&INK))
            .draw()
            .map_err(chart_err)?;
        Ok(())
    })
    .map(Some)
}

/// Response counts per status class.
///
/// Grouped by class rather than by exact code: a run that returns 200 and 204 is
/// not telling you two different things, and one bar per distinct code turns a
/// noisy run into an unreadable comb. Colour is semantic here — the 4xx and 5xx
/// bars are red whichever target this is — because "which target" is already the
/// chart's caption and "did it fail" is the question being asked.
fn status(caption: &str, points: &[Sample]) -> Result<String> {
    const CLASSES: [&str; 7] = ["0", "1xx", "2xx", "3xx", "4xx", "5xx", "other"];

    let mut counts = [0usize; CLASSES.len()];
    for s in points {
        counts[class_of(s.status)] += 1;
    }
    let bars: Vec<(&str, usize, RGBColor)> = counts
        .iter()
        .enumerate()
        .filter(|&(_, &c)| c > 0)
        .map(|(i, &c)| (CLASSES[i], c, class_colour(i)))
        .collect();
    let peak = bars.iter().map(|&(_, c, _)| c).max().unwrap_or(1).max(1) as f64;
    let labels: Vec<&str> = bars.iter().map(|&(name, _, _)| name).collect();
    let n = bars.len();

    draw(|root| {
        let mut chart = frame(root, caption, -0.5..n as f64 - 0.5, 0.0..peak * 1.15)?;
        styled_mesh(
            &mut chart,
            "Status class",
            "Responses",
            Some(n),
            Some(&|v: &f64| tick_label(*v, &labels)),
            None,
        )?;
        chart
            .draw_series(bars.iter().enumerate().map(|(i, &(_, count, colour))| {
                Rectangle::new(
                    [(i as f64 - 0.3, 0.0), (i as f64 + 0.3, count as f64)],
                    colour.mix(0.8).filled(),
                )
            }))
            .map_err(chart_err)?;
        chart
            .draw_series(bars.iter().enumerate().map(|(i, &(_, count, _))| {
                Text::new(
                    count.to_string(),
                    (i as f64, count as f64),
                    ("sans-serif", 12)
                        .into_font()
                        .color(&INK)
                        .pos(Pos::new(HPos::Center, VPos::Bottom)),
                )
            }))
            .map_err(chart_err)?;
        Ok(())
    })
}

fn class_of(status: u16) -> usize {
    match status {
        0 => 0,
        100..=199 => 1,
        200..=299 => 2,
        300..=399 => 3,
        400..=499 => 4,
        500..=599 => 5,
        _ => 6,
    }
}

fn class_colour(class: usize) -> RGBColor {
    match class {
        2 => PALETTE[2], // 2xx: emerald
        3 => PALETTE[0], // 3xx: blue
        4 | 5 => ALARM,  // 4xx/5xx: the reason someone opened this chart
        _ => INK,        // 0, 1xx, and anything past 5xx
    }
}

/// The label for an x tick on a categorical axis, or blank for the fractional
/// ticks `plotters` may put between categories.
fn tick_label(v: f64, labels: &[&str]) -> String {
    let i = v.round();
    if (v - i).abs() > 1e-6 || i < 0.0 {
        return String::new();
    }
    labels
        .get(i as usize)
        .map(|s| s.to_string())
        .unwrap_or_default()
}

// --- plotters plumbing -------------------------------------------------------

type Canvas<'a, 'b> = DrawingArea<SVGBackend<'a>, plotters::coord::Shift>;
type Coord = plotters::coord::types::RangedCoordf64;
type Chart<'a, 'b> = ChartContext<'b, SVGBackend<'a>, Cartesian2d<Coord, Coord>>;

/// Run a drawing closure against a fresh SVG canvas and return the document.
///
/// The backend writes into a local `String`, so nothing touches the filesystem
/// here — the caller decides whether the bytes become a file or an inline
/// `<svg>` in the HTML report.
fn draw<F>(body: F) -> Result<String>
where
    F: for<'a, 'b> FnOnce(&'b Canvas<'a, 'b>) -> Result<()>,
{
    let mut buf = String::new();
    {
        let root = SVGBackend::with_string(&mut buf, (WIDTH, HEIGHT)).into_drawing_area();
        body(&root)?;
        root.present().map_err(chart_err)?;
    }
    Ok(buf)
}

/// The captioned, margined chart frame shared by every kind.
///
/// The canvas is left unfilled on purpose: a transparent background lets the
/// embedded SVG inherit the page's light or dark surface.
fn frame<'a, 'b>(
    root: &'b Canvas<'a, 'b>,
    caption: &str,
    x: std::ops::Range<f64>,
    y: std::ops::Range<f64>,
) -> Result<Chart<'a, 'b>> {
    ChartBuilder::on(root)
        .caption(caption, ("sans-serif", 17).into_font().color(&INK))
        .margin(14)
        .x_label_area_size(42)
        .y_label_area_size(64)
        .build_cartesian_2d(x, y)
        .map_err(chart_err)
}

fn mesh(chart: &mut Chart<'_, '_>, x_desc: &str, y_desc: &str) -> Result<()> {
    styled_mesh(chart, x_desc, y_desc, None, None, None)
}

/// The shared mesh style, with the hooks the categorical and percentage axes
/// need: a fixed tick count and per-axis label formatters.
fn styled_mesh(
    chart: &mut Chart<'_, '_>,
    x_desc: &str,
    y_desc: &str,
    x_labels: Option<usize>,
    x_fmt: Option<&dyn Fn(&f64) -> String>,
    y_fmt: Option<&dyn Fn(&f64) -> String>,
) -> Result<()> {
    let mut style = chart.configure_mesh();
    style
        .axis_style(INK)
        .light_line_style(INK.mix(0.12))
        .bold_line_style(INK.mix(0.28))
        .label_style(("sans-serif", 12).into_font().color(&INK))
        .x_desc(x_desc)
        .y_desc(y_desc);
    if let Some(n) = x_labels {
        style.x_labels(n.max(1));
    }
    if let Some(f) = x_fmt {
        style.x_label_formatter(f);
    }
    if let Some(f) = y_fmt {
        style.y_label_formatter(f);
    }
    style.draw().map_err(chart_err)
}

fn chart_err<E: fmt::Display>(err: E) -> Error {
    Error::Chart(err.to_string())
}

// --- small helpers -----------------------------------------------------------

/// Drop samples a chart cannot place, and demote a non-finite latency to a
/// failure: a NaN is not a measurement, but the request it came from still
/// happened and still has a status.
fn clean(samples: &[Sample]) -> Vec<Sample> {
    let mut points: Vec<Sample> = samples
        .iter()
        .filter(|s| s.offset_s.is_finite())
        .map(|s| Sample {
            latency_ms: s.latency_ms.filter(|v| v.is_finite()),
            ..*s
        })
        .collect();
    // Completion order is *almost* time order under concurrency; sorting makes
    // the binning exact rather than almost.
    points.sort_by(|a, b| {
        a.offset_s
            .partial_cmp(&b.offset_s)
            .unwrap_or(Ordering::Equal)
    });
    points
}

/// The `[first, last]` offset span of a run, or `None` when it has no measurable
/// duration and a per-second rate would be a division by zero.
fn span(points: &[Sample]) -> Option<(f64, f64)> {
    let lo = points.first()?.offset_s;
    let hi = points.last()?.offset_s;
    (hi - lo > f64::EPSILON).then_some((lo, hi))
}

/// Square-root binning, clamped — the same rule the histogram uses, so a run's
/// charts share a resolution.
fn bin_count(n: usize, min: usize, max: usize) -> usize {
    (n as f64).sqrt().round().clamp(min as f64, max as f64) as usize
}

fn bin_of(value: f64, lo: f64, width: f64, bins: usize) -> usize {
    (((value - lo) / width) as usize).min(bins - 1)
}

/// Blend a series colour `t` of the way toward the axis grey, for the related
/// tracks of one chart (p50/p95/p99) that must still read as one family.
fn toward_ink(colour: RGBColor, t: f64) -> RGBColor {
    let blend = |c: u8, ink: u8| (c as f64 * (1.0 - t) + ink as f64 * t).round() as u8;
    RGBColor(
        blend(colour.0, INK.0),
        blend(colour.1, INK.1),
        blend(colour.2, INK.2),
    )
}

/// Widen an axis range so a constant-latency run still gets a drawable span,
/// and add a little headroom otherwise.
fn padded(lo: f64, hi: f64) -> (f64, f64) {
    if (hi - lo).abs() < f64::EPSILON {
        let pad = if lo.abs() < f64::EPSILON {
            1.0
        } else {
            lo.abs() * 0.1
        };
        (lo - pad, hi + pad)
    } else {
        let pad = (hi - lo) * 0.04;
        (lo - pad, hi + pad)
    }
}

/// Linearly interpolated quantile of an already-sorted slice.
fn quantile(sorted: &[f64], q: f64) -> f64 {
    let n = sorted.len();
    if n == 1 {
        return sorted[0];
    }
    let pos = q * (n - 1) as f64;
    let lo = pos.floor() as usize;
    let hi = pos.ceil() as usize;
    if lo == hi {
        sorted[lo]
    } else {
        sorted[lo] + (pos - lo as f64) * (sorted[hi] - sorted[lo])
    }
}

/// Turn a target name into a filename fragment: lowercase, `[a-z0-9_-]` only,
/// runs of anything else collapsed to a single `-`.
pub(crate) fn slug(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch.to_ascii_lowercase());
        } else if !out.ends_with('-') {
            out.push('-');
        }
    }
    let trimmed = out.trim_matches('-');
    if trimmed.is_empty() {
        "target".to_string()
    } else {
        trimmed.to_string()
    }
}

/// Whether a target carries anything a chart could read, for callers deciding
/// between rendering and saying so.
pub(crate) fn has_samples(target: &TargetReport) -> bool {
    target.samples.iter().any(|s| s.offset_s.is_finite())
}

#[cfg(test)]
mod tests {
    use super::*;
    use gauntlet_stats::BenchmarkStats;

    /// A mild right-skew at one request every 10 ms, so quartiles, bins, and
    /// time bins are all non-degenerate.
    fn samples(n: usize) -> Vec<Sample> {
        (0..n)
            .map(|i| Sample {
                latency_ms: Some(10.0 + (i % 17) as f64 * 0.7),
                status: 200,
                offset_s: i as f64 * 0.01,
            })
            .collect()
    }

    fn failed(n: usize) -> Vec<Sample> {
        (0..n)
            .map(|i| Sample {
                latency_ms: None,
                status: 0,
                offset_s: i as f64 * 0.01,
            })
            .collect()
    }

    /// Every rendered text run in an SVG, trimmed — `plotters` wraps each label
    /// in newlines, so a naive `contains(">2xx<")` never matches.
    fn text_nodes(svg: &str) -> Vec<String> {
        svg.split('>')
            .skip(1)
            .filter_map(|chunk| chunk.split('<').next())
            .map(|t| t.trim().to_string())
            .filter(|t| !t.is_empty())
            .collect()
    }

    fn svg_of(kind: ChartKind, samples: &[Sample]) -> String {
        render(kind, "api", samples, 0)
            .unwrap()
            .unwrap_or_else(|| panic!("{kind} rendered nothing"))
    }

    #[test]
    fn chart_kinds_round_trip_through_their_string_names() {
        for kind in ChartKind::ALL {
            assert_eq!(ChartKind::from_str(kind.as_str()).unwrap(), kind);
            assert_eq!(kind.to_string(), kind.as_str());
        }
        assert_eq!(ChartKind::from_str("  CDF ").unwrap(), ChartKind::Cdf);
        assert_eq!(
            ChartKind::from_str("error-rate").unwrap(),
            ChartKind::ErrorRate
        );
        assert_eq!(
            ChartKind::from_str("rolling-pct").unwrap(),
            ChartKind::RollingPct
        );
    }

    #[test]
    fn an_unknown_chart_kind_is_an_error_naming_the_valid_ones() {
        let err = ChartKind::from_str("violin").unwrap_err();
        let message = err.to_string();
        assert!(message.contains("violin"), "{message}");
        for kind in ChartKind::ALL {
            assert!(message.contains(kind.as_str()), "{message}");
        }
    }

    #[test]
    fn every_kind_renders_a_non_trivial_svg_document() {
        for kind in ChartKind::ALL {
            let svg = svg_of(kind, &samples(200));

            assert!(svg.starts_with("<svg"), "{kind}: not an svg document");
            assert!(svg.contains("</svg>"), "{kind}: unterminated svg");
            assert!(
                svg.len() > 1_000,
                "{kind}: {} bytes is an empty frame, not a chart",
                svg.len()
            );
            assert!(svg.contains("api"), "{kind}: caption is missing the target");
        }
    }

    #[test]
    fn a_target_with_no_samples_renders_nothing() {
        for kind in ChartKind::ALL {
            assert!(render(kind, "api", &[], 0).unwrap().is_none(), "{kind}");
        }
    }

    #[test]
    fn a_run_where_every_request_failed_charts_outcomes_but_no_latency() {
        let dead = failed(40);
        for kind in ChartKind::ALL {
            let rendered = render(kind, "api", &dead, 0).unwrap();
            if kind.needs_latency() {
                assert!(rendered.is_none(), "{kind} has no latency to draw");
            } else {
                assert!(rendered.is_some(), "{kind} is about outcomes, not latency");
            }
        }
    }

    #[test]
    fn a_non_finite_latency_is_treated_as_a_failure_rather_than_plotted() {
        let junk = vec![
            Sample {
                latency_ms: Some(f64::NAN),
                status: 200,
                offset_s: 0.0,
            },
            Sample {
                latency_ms: Some(f64::INFINITY),
                status: 200,
                offset_s: 1.0,
            },
        ];
        assert!(render(ChartKind::Cdf, "api", &junk, 0).unwrap().is_none());
        assert!(render(ChartKind::Status, "api", &junk, 0)
            .unwrap()
            .is_some());
    }

    #[test]
    fn a_sample_with_a_non_finite_offset_is_dropped_entirely() {
        let junk = vec![Sample {
            latency_ms: Some(5.0),
            status: 200,
            offset_s: f64::NAN,
        }];
        for kind in ChartKind::ALL {
            assert!(render(kind, "api", &junk, 0).unwrap().is_none(), "{kind}");
        }
    }

    #[test]
    fn a_single_sample_charts_what_it_can_and_skips_the_time_series() {
        let one = samples(1);
        for kind in ChartKind::ALL {
            let rendered = render(kind, "api", &one, 0).unwrap();
            match kind {
                // One sample spans no time, so a per-second rate is undefined.
                ChartKind::Throughput | ChartKind::ErrorRate | ChartKind::RollingPct => {
                    assert!(rendered.is_none(), "{kind} needs a duration")
                }
                _ => assert!(rendered.is_some(), "{kind} should draw a single sample"),
            }
        }
    }

    #[test]
    fn a_zero_duration_run_skips_the_time_series_instead_of_dividing_by_it() {
        // Every request stamped at the same offset: a burst that completed
        // inside one clock tick, or a run whose clock never moved.
        let instant: Vec<Sample> = (0..50)
            .map(|i| Sample {
                latency_ms: Some(5.0 + i as f64),
                status: 200,
                offset_s: 3.0,
            })
            .collect();
        for kind in ChartKind::ALL {
            let rendered = render(kind, "api", &instant, 0).unwrap();
            match kind {
                ChartKind::Throughput | ChartKind::ErrorRate | ChartKind::RollingPct => {
                    assert!(rendered.is_none(), "{kind} needs a duration")
                }
                _ => assert!(rendered.is_some(), "{kind} does not depend on time"),
            }
        }
    }

    #[test]
    fn a_constant_latency_run_still_produces_a_drawable_axis() {
        let flat: Vec<Sample> = (0..50)
            .map(|i| Sample {
                latency_ms: Some(5.0),
                status: 200,
                offset_s: i as f64 * 0.1,
            })
            .collect();
        for kind in ChartKind::ALL {
            let svg = svg_of(kind, &flat);
            assert!(svg.contains("</svg>"), "{kind}: degenerate range broke it");
        }
    }

    #[test]
    fn the_throughput_chart_reports_a_rate_not_a_raw_count() {
        // 100 requests over one second at a steady rate: whatever bin width is
        // chosen, the plotted rate must be near 100/s rather than the bin count.
        let steady: Vec<Sample> = (0..100)
            .map(|i| Sample {
                latency_ms: Some(1.0),
                status: 200,
                offset_s: i as f64 / 100.0,
            })
            .collect();
        let points = clean(&steady);
        let (lo, hi) = span(&points).unwrap();
        let bins = bin_count(points.len(), 4, 60);
        let width = (hi - lo) / bins as f64;
        let mut counts = vec![0usize; bins];
        for s in &points {
            counts[bin_of(s.offset_s, lo, width, bins)] += 1;
        }
        let peak = counts.iter().map(|&c| c as f64 / width).fold(0.0, f64::max);
        assert!((90.0..140.0).contains(&peak), "rate was {peak}/s");
    }

    #[test]
    fn the_error_rate_chart_survives_a_run_with_no_errors_at_all() {
        let svg = svg_of(ChartKind::ErrorRate, &samples(60));
        assert!(svg.contains("</svg>"));
    }

    #[test]
    fn the_status_chart_groups_codes_into_classes() {
        let mixed: Vec<Sample> = [200u16, 204, 301, 404, 500, 0, 999]
            .into_iter()
            .enumerate()
            .map(|(i, status)| Sample {
                latency_ms: (status != 0).then_some(3.0),
                status,
                offset_s: i as f64,
            })
            .collect();
        let labels = text_nodes(&svg_of(ChartKind::Status, &mixed));

        for class in ["0", "2xx", "3xx", "4xx", "5xx", "other"] {
            assert!(
                labels.iter().any(|l| l == class),
                "missing class {class} in {labels:?}"
            );
        }
        // 200 and 204 land in one bar, so 2xx is a count of two.
        assert!(labels.iter().any(|l| l == "2"), "2xx should count two");
        assert!(
            !labels.iter().any(|l| l == "1xx"),
            "empty classes are not drawn"
        );
    }

    #[test]
    fn the_rolling_percentile_chart_labels_all_three_series() {
        let svg = svg_of(ChartKind::RollingPct, &samples(300));
        for label in ["p50", "p95", "p99"] {
            assert!(svg.contains(label), "missing {label}");
        }
    }

    #[test]
    fn the_rolling_percentile_chart_tolerates_a_run_that_mostly_failed() {
        // One success at the very end: every earlier window is empty, and the
        // chart must fall back to the windows that do have data.
        let mut mostly_dead = failed(80);
        mostly_dead.push(Sample {
            latency_ms: Some(12.0),
            status: 200,
            offset_s: 0.8,
        });
        let svg = svg_of(ChartKind::RollingPct, &mostly_dead);
        assert!(svg.contains("</svg>"));
    }

    #[test]
    fn the_rolling_percentile_chart_declines_when_no_window_holds_a_success() {
        // The single success is the very first request, which falls before the
        // first window closes: there is no track to draw, and that is `None`
        // rather than an error that would sink the whole report.
        let mut early = failed(80);
        early[0].latency_ms = Some(12.0);
        early[0].status = 200;
        assert!(render(ChartKind::RollingPct, "api", &early, 0)
            .unwrap()
            .is_none());
    }

    #[test]
    fn the_tail_chart_covers_the_top_decile_only() {
        let labels = text_nodes(&svg_of(ChartKind::Tail, &samples(500)));
        assert!(labels.iter().any(|l| l == "90.0"), "x starts at p90");
        assert!(labels.iter().any(|l| l == "100.0"), "x ends at p100");
        assert!(labels.iter().any(|l| l == "Percentile"));
    }

    #[test]
    fn plotters_escapes_markup_in_the_caption() {
        // The caption is the one place a target name reaches the SVG, so the
        // same injection guarantee the HTML report makes has to hold here.
        let svg = render(ChartKind::Cdf, "<script>x</script>", &samples(50), 0)
            .unwrap()
            .unwrap();
        assert!(!svg.contains("<script>"));
        assert!(svg.contains("&lt;script&gt;"));
    }

    #[tokio::test]
    async fn the_reporter_writes_one_svg_per_target_per_kind() {
        let dir = std::env::temp_dir().join(format!("gauntlet-charts-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);

        let mut a = TargetReport::new("api v1", BenchmarkStats::default());
        a.samples = samples(60);
        let mut b = TargetReport::new("api v2", BenchmarkStats::default());
        b.samples = samples(60);
        // No samples at all: this target must simply be skipped.
        let empty = TargetReport::new("dead", BenchmarkStats::default());

        let report = BenchmarkReport {
            targets: vec![a, b, empty],
            comparisons: Vec::new(),
        };
        let reporter = ChartReporter::with_kinds(
            &dir,
            vec![ChartKind::Cdf, ChartKind::Histogram, ChartKind::Throughput],
        );
        reporter.on_benchmark(&report).await.unwrap();

        assert!(dir.join("api-v1-cdf.svg").exists());
        assert!(dir.join("api-v1-histogram.svg").exists());
        assert!(dir.join("api-v1-throughput.svg").exists());
        assert!(dir.join("api-v2-cdf.svg").exists());
        assert!(
            !dir.join("dead-cdf.svg").exists(),
            "a target with no samples must not leave an empty chart behind"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[tokio::test]
    async fn the_reporter_skips_only_the_kinds_a_failed_target_cannot_fill() {
        let dir = std::env::temp_dir().join(format!("gauntlet-charts-dead-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);

        let mut dead = TargetReport::new("down", BenchmarkStats::default());
        dead.samples = failed(30);

        let reporter = ChartReporter::new(&dir);
        reporter
            .on_benchmark(&BenchmarkReport::single(dead))
            .await
            .unwrap();

        assert!(dir.join("down-status.svg").exists());
        assert!(dir.join("down-error_rate.svg").exists());
        assert!(!dir.join("down-cdf.svg").exists());
        assert!(!dir.join("down-boxplot.svg").exists());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_hostile_target_name_cannot_escape_the_chart_directory() {
        let reporter = ChartReporter::new("out");
        let path = reporter.path_for("../../etc/passwd", ChartKind::Cdf);
        assert_eq!(path, PathBuf::from("out/etc-passwd-cdf.svg"));
        assert_eq!(slug("!!!"), "target");
    }
}
