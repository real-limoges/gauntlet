# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "matplotlib>=3.7",
#     "polars>=1.0",
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
import polars as pl
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

def load_data(paths: list[Path]) -> pl.DataFrame:
    frames = [pl.read_csv(p) for p in paths]
    df = pl.concat(frames)
    if "target_name" not in df.columns:
        df = df.with_columns(pl.lit("default").alias("target_name"))
    df = df.with_columns(
        pl.col("timestamp_iso").str.to_datetime("%Y-%m-%dT%H:%M:%S%.f%Z").alias("timestamp")
    )
    return df


def add_group_label(df: pl.DataFrame, group_by: str) -> pl.DataFrame:
    if group_by == "target_name":
        return df.with_columns(pl.col("target_name").alias("group_label"))
    elif group_by == "url":
        return df.with_columns(
            (pl.col("target_name") + " / " + pl.col("url")).alias("group_label")
        )
    elif group_by == "payload_id":
        return df.with_columns(
            (pl.col("target_name") + " / payload " + pl.col("payload_id").cast(pl.Utf8)).alias("group_label")
        )
    return df


def iter_groups(df: pl.DataFrame) -> list[tuple[str, pl.DataFrame]]:
    """Iterate over (group_name, sub_df) pairs, sorted by group_label."""
    labels = sorted(df["group_label"].unique().to_list())
    return [(name, df.filter(pl.col("group_label") == name)) for name in labels]


# ---------------------------------------------------------------------------
# Chart functions
# ---------------------------------------------------------------------------

def plot_kde(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in iter_groups(df):
        vals = grp["latency_ms"].to_numpy()
        sns.kdeplot(vals, ax=ax, label=name, fill=True, alpha=0.3)
        if opts.show_mean:
            ax.axvline(vals.mean(), linestyle="--", alpha=0.7,
                       color=ax.lines[-1].get_color() if ax.lines else None)
    if opts.percentiles:
        add_percentile_lines(df, ax)
    if opts.log_latency:
        ax.set_xscale("log")
    ax.set_xlabel("Latency (ms)")
    ax.set_ylabel("Density")
    ax.setget_title(get_title(opts, "Latency Distribution (KDE)"))
    ax.legend()


def plot_cdf(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in iter_groups(df):
        sorted_vals = np.sort(grp["latency_ms"].to_numpy())
        cdf = np.arange(1, len(sorted_vals) + 1) / len(sorted_vals)
        ax.plot(sorted_vals, cdf, label=name)
    if opts.percentiles:
        add_percentile_lines(df, ax)
    if opts.log_latency:
        ax.set_xscale("log")
    ax.set_xlabel("Latency (ms)")
    ax.set_ylabel("Cumulative Probability")
    ax.setget_title(get_title(opts, "Latency CDF"))
    ax.yaxis.set_major_formatter(ticker.PercentFormatter(1.0))
    ax.legend()


def plot_tail(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """CDF zoomed in above --tail-from percentile to highlight tail behaviour."""
    cutoff = np.percentile(df["latency_ms"].to_numpy(), opts.tail_from)
    tail_df = df.filter(pl.col("latency_ms") >= cutoff)
    for name, grp in iter_groups(tail_df):
        sorted_vals = np.sort(grp["latency_ms"].to_numpy())
        full_n = df.filter(pl.col("group_label") == name).height
        cdf = (full_n - len(sorted_vals) + np.arange(1, len(sorted_vals) + 1)) / full_n
        ax.plot(sorted_vals, cdf, label=name)
    if opts.log_latency:
        ax.set_xscale("log")
    ax.yaxis.set_major_formatter(ticker.PercentFormatter(1.0))
    ax.set_xlabel("Latency (ms)")
    ax.set_ylabel("Cumulative Probability")
    ax.setget_title(get_title(opts, f"Tail Latency CDF (above p{opts.tail_from})"))
    ax.legend()


def plot_timeline(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in iter_groups(df):
        ts = grp["timestamp"].to_numpy()
        lat = grp["latency_ms"].to_numpy()
        ax.scatter(ts, lat, label=name, alpha=0.4, s=8)
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Time")
    ax.set_ylabel("Latency (ms)")
    ax.setget_title(get_title(opts, "Latency Over Time"))
    ax.legend()
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_throughput(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    for name, grp in iter_groups(df):
        grp_sorted = grp.sort("timestamp")
        counts = (
            grp_sorted
            .group_by_dynamic("timestamp", every="1s")
            .agg(pl.len().alias("count"))
        )
        ax.plot(counts["timestamp"].to_numpy(), counts["count"].to_numpy(), label=name)
    ax.set_xlabel("Time")
    ax.set_ylabel("Requests / sec")
    ax.setget_title(get_title(opts, "Throughput Over Time"))
    ax.legend()
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_heatmap(df: pl.DataFrame, fig: plt.Figure, opts: argparse.Namespace) -> None:
    """2D density heatmap per group. One subplot per group."""
    groups = sorted(df["group_label"].unique().to_list())
    n = len(groups)
    axes = fig.subplots(1, n, squeeze=False)[0]
    for ax, name in zip(axes, groups):
        grp = df.filter(pl.col("group_label") == name)
        t0 = grp["timestamp"].min()
        elapsed = ((grp["timestamp"] - t0).dt.total_seconds()).to_numpy()
        lat = grp["latency_ms"].to_numpy()
        ax.hist2d(
            elapsed,
            lat,
            bins=[min(50, max(10, grp.height // 20)), 50],
            cmap="YlOrRd",
            cmin=1,
        )
        ax.set_xlabel("Elapsed (s)")
        ax.set_ylabel("Latency (ms)")
        ax.setget_title(name)
    fig.suptitle(get_title(opts, "Latency Heatmap"))


def plot_status(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    # Pivot via polars, then plot with pandas (matplotlib bar needs it)
    pivot = (
        df.group_by(["group_label", "status_code"])
        .agg(pl.len().alias("count"))
        .pivot(on="status_code", index="group_label", values="count")
        .fill_null(0)
        .sort("group_label")
    )
    pivot.to_pandas().set_index("group_label").plot(kind="bar", stacked=True, ax=ax, colormap="Set2")
    ax.set_xlabel("Group")
    ax.set_ylabel("Request Count")
    ax.setget_title(get_title(opts, "Status Code Breakdown"))
    ax.legend(title="Status Code")
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_violin(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    pdf = df.select("group_label", "latency_ms").to_pandas()
    sns.violinplot(data=pdf, x="group_label", y="latency_ms", ax=ax,
                   inner="box", cut=0, density_norm="width")
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Group")
    ax.set_ylabel("Latency (ms)")
    ax.setget_title(get_title(opts, "Latency Distribution (Violin)"))
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_box(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    pdf = df.select("group_label", "latency_ms").to_pandas()
    sns.boxplot(data=pdf, x="group_label", y="latency_ms", ax=ax,
                flierprops={"marker": ".", "alpha": 0.3, "markersize": 3})
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Group")
    ax.set_ylabel("Latency (ms)")
    ax.setget_title(get_title(opts, "Latency Distribution (Box)"))
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_percentile_bar(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Grouped bar chart showing p50/p95/p99/p99.9 per group."""
    groups = sorted(df["group_label"].unique().to_list())
    pct_labels = [f"p{p:g}" for p in PERCENTILES]
    x = np.arange(len(PERCENTILES))
    width = 0.8 / max(len(groups), 1)

    for i, name in enumerate(groups):
        vals_arr = df.filter(pl.col("group_label") == name)["latency_ms"].to_numpy()
        vals = [np.percentile(vals_arr, p) for p in PERCENTILES]
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
    ax.setget_title(get_title(opts, "Latency Percentiles"))
    ax.legend()


def parse_rolling_window(window_str: str) -> int:
    """Parse a rolling window string like '30s', '2min' into seconds."""
    s = window_str.strip().lower()
    if s.endswith("min"):
        return int(s[:-3]) * 60
    elif s.endswith("s"):
        return int(s[:-1])
    else:
        return int(s)


def plot_rolling_pct(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Rolling p50/p95/p99 over time to reveal warmup effects or degradation."""
    pcts = {50: "-", 95: "--", 99: ":"}
    colors = sns.color_palette()
    window_secs = parse_rolling_window(opts.rolling_window)
    window_dur = f"{window_secs}s"

    for i, (name, grp) in enumerate(iter_groups(df)):
        color = colors[i % len(colors)]
        grp_sorted = grp.sort("timestamp")
        for pct, style in pcts.items():
            rolled = (
                grp_sorted
                .rolling("timestamp", period=window_dur)
                .agg(pl.col("latency_ms").quantile(pct / 100).alias("val"))
            )
            ax.plot(rolled["timestamp"].to_numpy(), rolled["val"].to_numpy(),
                    linestyle=style, color=color, alpha=0.85, label=f"{name} p{pct}")
    if opts.log_latency:
        ax.set_yscale("log")
    ax.set_xlabel("Time")
    ax.set_ylabel("Latency (ms)")
    ax.setget_title(get_title(opts, f"Rolling Latency Percentiles (window={opts.rolling_window})"))
    ax.legend(fontsize="x-small")
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_error_rate(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Error rate (status >= 400) over time in 5-second bins."""
    for name, grp in iter_groups(df):
        grp_sorted = grp.sort("timestamp")
        total = (
            grp_sorted
            .group_by_dynamic("timestamp", every="5s")
            .agg(pl.len().alias("total"))
        )
        errors = (
            grp_sorted
            .filter(pl.col("status_code") >= 400)
            .group_by_dynamic("timestamp", every="5s")
            .agg(pl.len().alias("errors"))
        )
        merged = total.join(errors, on="timestamp", how="left").fill_null(0)
        rate = (merged["errors"] / merged["total"]).fill_nan(0.0).to_numpy()
        ax.plot(merged["timestamp"].to_numpy(), rate, label=name)
    ax.yaxis.set_major_formatter(ticker.PercentFormatter(1.0))
    ax.set_xlabel("Time")
    ax.set_ylabel("Error Rate")
    ax.setget_title(get_title(opts, "Error Rate Over Time (5-second bins)"))
    ax.legend()
    plt.setp(ax.get_xticklabels(), rotation=30, ha="right")


def plot_qq(df: pl.DataFrame, ax: plt.Axes, opts: argparse.Namespace) -> None:
    """Q-Q plot between the first two groups. Shows where distributions diverge."""
    groups = sorted(df["group_label"].unique().to_list())
    if len(groups) < 2:
        print("qq: need at least two groups, skipping.", file=sys.stderr)
        return
    a_vals = np.sort(df.filter(pl.col("group_label") == groups[0])["latency_ms"].to_numpy())
    b_vals = np.sort(df.filter(pl.col("group_label") == groups[1])["latency_ms"].to_numpy())

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
    ax.setget_title(get_title(opts, f"Q-Q Plot: {groups[0]} vs {groups[1]}"))
    ax.legend()


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def add_percentile_lines(df: pl.DataFrame, ax: plt.Axes) -> None:
    colors = sns.color_palette()
    styles = {50: ":", 95: "--", 99: "-.", 99.9: (0, (3, 1, 1, 1, 1, 1))}
    for i, (name, grp) in enumerate(iter_groups(df)):
        color = colors[i % len(colors)]
        vals = grp["latency_ms"].to_numpy()
        for pct, style in styles.items():
            v = np.percentile(vals, pct)
            ax.axvline(v, color=color, linestyle=style, alpha=0.6,
                       label=f"{name} p{pct:g}={v:.1f}")
    ax.legend(fontsize="x-small")


def get_title(opts: argparse.Namespace, default: str) -> str:
    """Return the user-supplied --title or the given default."""
    return opts.title if opts.title else default


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


def output_path(first_csv: Path, chart_type: str, explicit: Path | None, output_dir: str | None = None) -> Path:
    if explicit:
        return explicit
    stem = first_csv.stem
    parent = Path(output_dir) if output_dir else first_csv.parent
    return parent / f"{stem}.{chart_type}.png"


def render_chart(
    chart_type: str,
    df: pl.DataFrame,
    opts: argparse.Namespace,
    first_csv: Path,
    output_override: Path | None,
) -> Path:
    out = output_path(first_csv, chart_type, output_override, getattr(opts, "output_dir", None))
    sns.set_theme(style="whitegrid")

    if chart_type in CHART_NEEDS_FIG:
        n = df["group_label"].n_unique()
        fig = plt.figure(figsize=(max(14, 6 * n), 6))
        plot_heatmap(df, fig, opts)
    else:
        fig, ax = plt.subplots(figsize=(12, 6))
        CHART_DISPATCH[chart_type](df, ax, opts)

    fig.tight_layout()

    if opts.show:
        plt.show()
    else:
        out.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(out, dpi=150)
        if not getattr(opts, "quiet", False):
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
        "--chart", default="kde",
        help="Chart type or comma-separated list (e.g. kde,cdf,violin) or 'all' (default: kde)",
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
    p.add_argument("--title", default=None,
                   help="Override chart title")
    p.add_argument("--quiet", action="store_true",
                   help="Suppress 'Saved ...' messages (useful for programmatic invocation)")
    p.add_argument("--output-dir", default=None, dest="output_dir",
                   help="Output directory for charts (overrides default of CSV parent dir)")
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> None:
    opts = parse_args(argv)

    df = load_data(opts.csvs)

    if opts.successes_only:
        before = df.height
        df = df.filter(pl.col("status_code") < 400)
        dropped = before - df.height
        if dropped:
            print(f"Dropped {dropped} rows with status >= 400 (--successes-only).", file=sys.stderr)

    if df.height == 0:
        print("Error: no data remaining after filtering.", file=sys.stderr)
        sys.exit(1)

    df = add_group_label(df, opts.group_by)

    first_csv = opts.csvs[0]
    # Support comma-separated chart types (e.g. "kde,cdf,violin")
    if opts.chart == "all":
        charts = list(CHART_TYPES)
    elif "," in opts.chart:
        charts = [c.strip() for c in opts.chart.split(",")]
    else:
        charts = [opts.chart]

    if len(charts) > 1 and opts.output is not None:
        print("Warning: -o ignored when --chart all; using auto-derived filenames.", file=sys.stderr)
        output_override = None
    else:
        output_override = opts.output

    for ct in charts:
        render_chart(ct, df, opts, first_csv, output_override)


if __name__ == "__main__":
    main()
