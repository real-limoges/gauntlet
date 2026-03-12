# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "matplotlib>=3.7",
#     "pandas>=2.0",
#     "seaborn>=0.13",
#     "scipy>=1.10",
# ]
# ///
"""Visualize gauntlet benchmark latency CSV files.

CSV schema (from gauntlet Output.hs):
    target_name, payload_id, url, method, status_code, latency_ms, timestamp_iso

Chart types:
    kde           Overlaid KDE per group
    cdf           Empirical CDF per group
    tail          CDF zoomed above --tail-from percentile (default p90)
    timeline      Scatter: time vs latency
    throughput    Requests/sec in 1-second bins
    heatmap       2D density: elapsed time vs latency, one subplot per group
    status        Stacked bar: status code breakdown per group
    violin        Violin plot per group (full shape + quartiles)
    box           Box plot per group (quartiles + outliers)
    percentile_bar Grouped bar chart: p50/p95/p99/p99.9 per group
    rolling_pct   Rolling p50/p95/p99 over time (window: --rolling-window)
    error_rate    Error rate (status >= 400) over time in 5-second bins
    qq            Quantile-quantile plot between first two groups

Usage:
    uv run scripts/plot_latency.py results/latencies-*.csv
    uv run scripts/plot_latency.py results/latest.csv --chart cdf
    uv run scripts/plot_latency.py results/latest.csv --chart all
    uv run scripts/plot_latency.py results/latest.csv -o plot.png --percentiles
    uv run scripts/plot_latency.py results/latest.csv --group-by url
    uv run scripts/plot_latency.py results/latest.csv --successes-only --log-latency
    uv run scripts/plot_latency.py results/latest.csv --chart rolling_pct --rolling-window 60s
    uv run scripts/plot_latency.py results/latest.csv --chart tail --tail-from 95
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import pandas as pd
import seaborn as sns
from scipy import stats as scipy_stats

CHART_TYPES = (
    "kde", "cdf", "tail", "timeline", "throughput", "heatmap",
    "status", "violin", "box", "percentile_bar", "rolling_pct", "error_rate", "qq",
)
GROUP_BY_CHOICES = ("target_name", "url", "payload_id")
PERCENTILES = [50, 95, 99, 99.9]


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_data(paths: list[Path]) -> pd.DataFrame:
    frames = [pd.read_csv(p) for p in paths]
    df = pd.concat(frames, ignore_index=True)
    df["timestamp"] = pd.to_datetime(df["timestamp_iso"], utc=True)
    return df


def add_group_label(df: pd.DataFrame, group_by: str) -> pd.DataFrame:
    df = df.copy()
    if group_by == "target_name":
        df["group_label"] = df["target_name"]
    elif group_by == "url":
        df["group_label"] = df["target_name"] + " / " + df["url"]
    elif group_by == "payload_id":
        df["group_label"] = df["target_name"] + " / payload " + df["payload_id"].astype(str)
    return df


# ---------------------------------------------------------------------------
# Chart functions
# ---------------------------------------------------------------------------

def plot_kde(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in df.groupby("group_label"):
        sns.kdeplot(grp["latency_ms"], ax=ax, label=name, fill=True, alpha=0.3)
        if opts.show_mean:
            ax.axvline(grp["latency_ms"].mean(), linestyle="--", alpha=0.7,
                       color=ax.lines[-1].get_color() if ax.lines else None)
    if opts.percentiles:
        _add_percentile_lines(df, ax)
    if opts.log_latency:
        ax.set_xscale("log")
    ax.set_xlabel("Latency (ms)")
    ax.set_ylabel("Density")
    ax.set_title("Latency Distribution (KDE)")
    ax.legend()


def plot_cdf(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in df.groupby("group_label"):
        sorted_vals = np.sort(grp["latency_ms"].values)
        cdf = np.arange(1, len(sorted_vals) + 1) / len(sorted_vals)
        ax.plot(sorted_vals, cdf, label=name)
    if opts.percentiles:
        _add_percentile_lines(df, ax)
    if opts.log_latency:
        ax.set_xscale("log")
    ax.set_xlabel("Latency (ms)")
    ax.set_ylabel("Cumulative Probability")
    ax.set_title("Latency CDF")
    ax.yaxis.set_major_formatter(ticker.PercentFormatter(1.0))
    ax.legend()


def plot_tail(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """CDF zoomed in above --tail-from percentile to highlight tail behaviour."""
    cutoff = np.percentile(df["latency_ms"], opts.tail_from)
    tail_df = df[df["latency_ms"] >= cutoff]
    for name, grp in tail_df.groupby("group_label"):
        sorted_vals = np.sort(grp["latency_ms"].values)
        # Recompute CDF relative to full group size so y-axis shows true percentile
        full_n = len(df[df["group_label"] == name])
        cdf = (full_n - len(sorted_vals) + np.arange(1, len(sorted_vals) + 1)) / full_n
        ax.plot(sorted_vals, cdf, label=name)
    if opts.log_latency:
        ax.set_xscale("log")
    ax.yaxis.set_major_formatter(ticker.PercentFormatter(1.0))
    ax.set_xlabel("Latency (ms)")
    ax.set_ylabel("Cumulative Probability")
    ax.set_title(f"Tail Latency CDF (above p{opts.tail_from})")
    ax.legend()


def plot_timeline(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in df.groupby("group_label"):
        ax.scatter(grp["timestamp"], grp["latency_ms"], label=name, alpha=0.4, s=8)
    if opts.log_latency:
        ax.set_yscale("log")  # latency is on y for timeline
    ax.set_xlabel("Time")
    ax.set_ylabel("Latency (ms)")
    ax.set_title("Latency Over Time")
    ax.legend()
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_throughput(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in df.groupby("group_label"):
        grp = grp.set_index("timestamp").sort_index()
        counts = grp.resample("1s").size()
        ax.plot(counts.index, counts.values, label=name)
    ax.set_xlabel("Time")
    ax.set_ylabel("Requests / sec")
    ax.set_title("Throughput Over Time")
    ax.legend()
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_heatmap(df: pd.DataFrame, fig: plt.Figure, opts: argparse.Namespace) -> None:
    """2D density heatmap per group. One subplot per group."""
    groups = df["group_label"].unique()
    n = len(groups)
    axes = fig.subplots(1, n, squeeze=False)[0]
    for ax, name in zip(axes, groups):
        grp = df[df["group_label"] == name]
        t0 = grp["timestamp"].min()
        elapsed = (grp["timestamp"] - t0).dt.total_seconds()
        ax.hist2d(
            elapsed,
            grp["latency_ms"],
            bins=[min(50, max(10, len(grp) // 20)), 50],
            cmap="YlOrRd",
            cmin=1,
        )
        ax.set_xlabel("Elapsed (s)")
        ax.set_ylabel("Latency (ms)")
        ax.set_title(name)
    fig.suptitle("Latency Heatmap")


def plot_status(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    pivot = df.groupby(["group_label", "status_code"]).size().unstack(fill_value=0)
    pivot.plot(kind="bar", stacked=True, ax=ax, colormap="Set2")
    ax.set_xlabel("Group")
    ax.set_ylabel("Request Count")
    ax.set_title("Status Code Breakdown")
    ax.legend(title="Status Code")
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_violin(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    sns.violinplot(data=df, x="group_label", y="latency_ms", ax=ax,
                   inner="box", cut=0, density_norm="width")
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Group")
    ax.set_ylabel("Latency (ms)")
    ax.set_title("Latency Distribution (Violin)")
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_box(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    sns.boxplot(data=df, x="group_label", y="latency_ms", ax=ax,
                flierprops={"marker": ".", "alpha": 0.3, "markersize": 3})
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Group")
    ax.set_ylabel("Latency (ms)")
    ax.set_title("Latency Distribution (Box)")
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_percentile_bar(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Grouped bar chart showing p50/p95/p99/p99.9 per group."""
    groups = sorted(df["group_label"].unique())
    pct_labels = [f"p{p:g}" for p in PERCENTILES]
    x = np.arange(len(PERCENTILES))
    width = 0.8 / max(len(groups), 1)

    for i, name in enumerate(groups):
        grp = df[df["group_label"] == name]["latency_ms"]
        vals = [np.percentile(grp, p) for p in PERCENTILES]
        offset = (i - len(groups) / 2 + 0.5) * width
        bars = ax.bar(x + offset, vals, width, label=name)
        for bar, val in zip(bars, vals):
            ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height(),
                    f"{val:.1f}", ha="center", va="bottom", fontsize=7)

    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xticks(x)
    ax.set_xticklabels(pct_labels)
    ax.set_ylabel("Latency (ms)")
    ax.set_title("Latency Percentiles")
    ax.legend()


def plot_rolling_pct(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Rolling p50/p95/p99 over time to reveal warmup effects or degradation."""
    styles = {50: "-", 95: "--", 99: ":"}
    colors = sns.color_palette()
    for i, (name, grp) in enumerate(df.groupby("group_label")):
        color = colors[i % len(colors)]
        grp = grp.set_index("timestamp").sort_index()["latency_ms"]
        rolled = grp.rolling(opts.rolling_window, min_periods=1)
        for pct, style in styles.items():
            series = rolled.quantile(pct / 100)
            ax.plot(series.index, series.values, linestyle=style, color=color,
                    alpha=0.85, label=f"{name} p{pct}")
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Time")
    ax.set_ylabel("Latency (ms)")
    ax.set_title(f"Rolling Latency Percentiles (window={opts.rolling_window})")
    ax.legend(fontsize="x-small")
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_error_rate(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Error rate (status >= 400) over time in 5-second bins."""
    for name, grp in df.groupby("group_label"):
        grp = grp.set_index("timestamp").sort_index()
        total = grp.resample("5s").size()
        errors = (
            grp[grp["status_code"] >= 400]
            .resample("5s")
            .size()
            .reindex(total.index, fill_value=0)
        )
        rate = (errors / total.replace(0, np.nan)).fillna(0)
        ax.plot(rate.index, rate.values, label=name)
    ax.yaxis.set_major_formatter(ticker.PercentFormatter(1.0))
    ax.set_xlabel("Time")
    ax.set_ylabel("Error Rate")
    ax.set_title("Error Rate Over Time (5-second bins)")
    ax.legend()
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_qq(df: pd.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Q-Q plot between the first two groups. Shows where distributions diverge."""
    groups = sorted(df["group_label"].unique())
    if len(groups) < 2:
        print("qq: need at least two groups, skipping.", file=sys.stderr)
        return
    a_vals = np.sort(df[df["group_label"] == groups[0]]["latency_ms"].values)
    b_vals = np.sort(df[df["group_label"] == groups[1]]["latency_ms"].values)

    # Interpolate to common quantile grid
    quantiles = np.linspace(0, 1, min(len(a_vals), len(b_vals), 1000))
    a_q = np.quantile(a_vals, quantiles)
    b_q = np.quantile(b_vals, quantiles)

    ax.scatter(a_q, b_q, s=4, alpha=0.6, label="Quantile pairs")
    ref_min = min(a_q.min(), b_q.min())
    ref_max = max(a_q.max(), b_q.max())
    ax.plot([ref_min, ref_max], [ref_min, ref_max], "r--", linewidth=1, label="y=x (identical)")

    if opts.log_latency:
        ax.set_xscale("log")
        ax.set_yscale("log")
    ax.set_xlabel(f"Latency (ms) — {groups[0]}")
    ax.set_ylabel(f"Latency (ms) — {groups[1]}")
    ax.set_title(f"Q-Q Plot: {groups[0]} vs {groups[1]}")
    ax.legend()


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _add_percentile_lines(df: pd.DataFrame, ax: plt.Axes) -> None:
    colors = sns.color_palette()
    styles = {50: ":", 95: "--", 99: "-.", 99.9: (0, (3, 1, 1, 1, 1, 1))}
    for i, (name, grp) in enumerate(df.groupby("group_label")):
        color = colors[i % len(colors)]
        vals = grp["latency_ms"]
        for pct, style in styles.items():
            v = np.percentile(vals, pct)
            ax.axvline(v, color=color, linestyle=style, alpha=0.6,
                       label=f"{name} p{pct:g}={v:.1f}")
    ax.legend(fontsize="x-small")


CHART_DISPATCH = {
    "kde": plot_kde,
    "cdf": plot_cdf,
    "tail": plot_tail,
    "timeline": plot_timeline,
    "throughput": plot_throughput,
    "status": plot_status,
    "violin": plot_violin,
    "box": plot_box,
    "percentile_bar": plot_percentile_bar,
    "rolling_pct": plot_rolling_pct,
    "error_rate": plot_error_rate,
    "qq": plot_qq,
}

CHART_NEEDS_FIG = {"heatmap"}


def _output_path(first_csv: Path, chart_type: str, explicit: Path | None) -> Path:
    if explicit:
        return explicit
    stem = first_csv.stem
    return first_csv.parent / f"{stem}.{chart_type}.png"


def render_chart(
    chart_type: str,
    df: pd.DataFrame,
    opts: argparse.Namespace,
    first_csv: Path,
    output_override: Path | None,
) -> Path:
    out = _output_path(first_csv, chart_type, output_override)
    sns.set_theme(style="whitegrid")

    if chart_type in CHART_NEEDS_FIG:
        n = df["group_label"].nunique()
        fig = plt.figure(figsize=(max(14, 6 * n), 6))
        plot_heatmap(df, fig, opts)
    else:
        fig, ax = plt.subplots(figsize=(12, 6))
        CHART_DISPATCH[chart_type](df, ax, opts)

    fig.tight_layout()

    if opts.show:
        plt.show()
    else:
        fig.savefig(out, dpi=150)
        print(f"Saved {out}")

    plt.close(fig)
    return out


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Plot gauntlet benchmark latency CSVs.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    p.add_argument("csvs", nargs="+", type=Path, help="One or more CSV files")
    p.add_argument(
        "--chart", choices=[*CHART_TYPES, "all"], default="kde",
        help="Chart type (default: kde)",
    )
    p.add_argument("-o", "--output", type=Path, default=None,
                   help="Output PNG path (single chart only; ignored with --chart all)")
    p.add_argument("--show", action="store_true", help="Show interactive window instead of saving")
    p.add_argument("--percentiles", action="store_true", help="Add p50/p95/p99/p99.9 lines (kde/cdf)")
    p.add_argument("--show-mean", action="store_true", dest="show_mean",
                   help="Add mean line per group (kde only)")
    p.add_argument("--log-latency", action="store_true", dest="log_latency",
                   help="Log scale on the latency axis")
    p.add_argument(
        "--group-by", choices=GROUP_BY_CHOICES, default="target_name", dest="group_by",
        help="Series grouping key (default: target_name)",
    )
    p.add_argument("--successes-only", action="store_true", dest="successes_only",
                   help="Exclude requests with status code >= 400")
    p.add_argument("--rolling-window", default="30s", dest="rolling_window",
                   help="Window size for rolling_pct chart (default: 30s, e.g. 60s, 2min)")
    p.add_argument("--tail-from", type=float, default=90.0, dest="tail_from",
                   help="Percentile above which to show the tail CDF (default: 90)")
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> None:
    opts = parse_args(argv)

    df = load_data(opts.csvs)

    if opts.successes_only:
        before = len(df)
        df = df[df["status_code"] < 400]
        dropped = before - len(df)
        if dropped:
            print(f"Dropped {dropped} rows with status >= 400 (--successes-only).", file=sys.stderr)

    df = add_group_label(df, opts.group_by)

    first_csv = opts.csvs[0]
    charts = list(CHART_TYPES) if opts.chart == "all" else [opts.chart]

    if len(charts) > 1 and opts.output is not None:
        print("Warning: -o ignored when --chart all; using auto-derived filenames.", file=sys.stderr)
        output_override = None
    else:
        output_override = opts.output

    for ct in charts:
        render_chart(ct, df, opts, first_csv, output_override)


if __name__ == "__main__":
    main()