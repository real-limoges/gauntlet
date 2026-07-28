//! Rendering the trace analysis, and dumping the raw traces. The tables are
//! capped; the interesting rows are the slow ones, which sort to the top.

use std::path::{Path, PathBuf};

use crate::analysis::{SpanAggregation, TraceAnalysis};
use crate::error::{Error, Result};

/// Rows shown in either rendering, slowest first.
const MAX_ROWS: usize = 20;

/// Column width for the service name in the fixed-width terminal table.
const SERVICE_COL: usize = 24;
/// Column width for the span name in the fixed-width terminal table.
const SPAN_COL: usize = 40;

/// The terminal section: a header block and a fixed-width span table.
pub fn render_terminal(analysis: &TraceAnalysis) -> String {
    let mut out = String::from("\n#----- Trace Analysis -----#\n");
    out.push_str(&format!("Service:         {}\n", analysis.service_name));
    out.push_str(&format!("Traces Analyzed: {}\n", analysis.trace_count));
    out.push_str(&format!("Total Spans:     {}\n", analysis.span_count));
    if analysis.error_span_count > 0 {
        out.push_str(&format!("Errored Spans:   {}\n", analysis.error_span_count));
    }
    out.push('\n');

    if analysis.is_empty() {
        out.push_str("No spans were found for the benchmark window.\n");
        return out;
    }

    out.push_str(&format!(
        "{:<SERVICE_COL$} {:<SPAN_COL$} {:>6} {:>10} {:>10} {:>10} {:>10}\n",
        "Service", "Span", "Count", "Mean", "P50", "P95", "P99"
    ));
    out.push_str(&"-".repeat(SERVICE_COL + SPAN_COL + 4 * 11 + 7));
    out.push('\n');

    for agg in analysis.spans.iter().take(MAX_ROWS) {
        out.push_str(&terminal_row(agg));
        out.push('\n');
    }
    if let Some(hidden) = analysis
        .spans
        .len()
        .checked_sub(MAX_ROWS)
        .filter(|n| *n > 0)
    {
        out.push_str(&format!("... and {hidden} more span(s), omitted.\n"));
    }
    out
}

/// One fixed-width table row. Public so the layout can be tested directly, the
/// same way `gauntlet-report` exposes its row formatters.
pub fn terminal_row(agg: &SpanAggregation) -> String {
    format!(
        "{:<SERVICE_COL$} {:<SPAN_COL$} {:>6} {:>8.2}ms {:>8.2}ms {:>8.2}ms {:>8.2}ms",
        truncate(&agg.service_name, SERVICE_COL),
        truncate(&agg.span_name, SPAN_COL),
        agg.count,
        agg.mean_ms,
        agg.p50_ms,
        agg.p95_ms,
        agg.p99_ms,
    )
}

/// The markdown section, for the markdown and HTML reports.
pub fn render_markdown(analysis: &TraceAnalysis) -> String {
    let mut out = String::from("## Trace Analysis\n\n");
    out.push_str(&format!(
        "Service `{}` — **{}** trace(s), **{}** span(s)",
        md_escape(&analysis.service_name),
        analysis.trace_count,
        analysis.span_count
    ));
    if analysis.error_span_count > 0 {
        out.push_str(&format!(", **{}** errored", analysis.error_span_count));
    }
    out.push_str(".\n\n");

    if analysis.is_empty() {
        out.push_str("No spans were found for the benchmark window.\n");
        return out;
    }

    out.push_str(
        "| Service | Span | Count | Errors | Mean (ms) | P50 (ms) | P95 (ms) | P99 (ms) |\n\
         |---------|------|------:|-------:|----------:|---------:|---------:|---------:|\n",
    );
    for agg in analysis.spans.iter().take(MAX_ROWS) {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {:.2} | {:.2} | {:.2} | {:.2} |\n",
            md_escape(&agg.service_name),
            md_escape(&agg.span_name),
            agg.count,
            agg.error_count,
            agg.mean_ms,
            agg.p50_ms,
            agg.p95_ms,
            agg.p99_ms,
        ));
    }
    if let Some(hidden) = analysis
        .spans
        .len()
        .checked_sub(MAX_ROWS)
        .filter(|n| *n > 0)
    {
        out.push_str(&format!(
            "\n_{hidden} slower-sorted span group(s) omitted._\n"
        ));
    }
    out
}

/// Write the fetched traces to a JSON file for later inspection.
pub fn write_raw_traces(path: impl AsRef<Path>, analysis: &TraceAnalysis) -> Result<()> {
    let path = path.as_ref();
    let json = serde_json::to_vec_pretty(&analysis.traces).map_err(|source| Error::Decode {
        url: path.display().to_string(),
        source,
    })?;
    if let Some(parent) = path.parent().filter(|p| !p.as_os_str().is_empty()) {
        std::fs::create_dir_all(parent).map_err(|source| Error::Write {
            path: PathBuf::from(parent),
            source,
        })?;
    }
    std::fs::write(path, json).map_err(|source| Error::Write {
        path: path.to_path_buf(),
        source,
    })
}

/// Truncate to `width` *characters* (never bytes — a span name can contain any
/// UTF-8), marking the cut with an ellipsis so it is visibly abbreviated.
fn truncate(value: &str, width: usize) -> String {
    if value.chars().count() <= width {
        return value.to_string();
    }
    let kept: String = value.chars().take(width.saturating_sub(1)).collect();
    format!("{kept}…")
}

/// Escape the characters that would break a markdown *table cell*. Span names
/// come from an instrumented service, so a literal `|` is entirely possible and
/// would silently shift every following column.
fn md_escape(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('|', "\\|")
        .replace(['\n', '\r'], " ")
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::analysis::build_analysis;
    use crate::types::{Span, SpanKind, SpanStatus, Trace, TraceWindow};

    fn window() -> TraceWindow {
        TraceWindow::from_unix_nanos(1_750_000_000_000_000_000, 1_750_000_010_000_000_000)
    }

    fn span(service: &str, name: &str, duration_ms: f64, status: SpanStatus) -> Span {
        let duration_ns = (duration_ms * 1_000_000.0) as u64;
        Span {
            span_id: format!("{service}/{name}/{duration_ms}"),
            parent_span_id: None,
            name: name.to_string(),
            service_name: service.to_string(),
            kind: SpanKind::Server,
            start_time_ns: 0,
            end_time_ns: duration_ns,
            duration_ns,
            status,
            attributes: BTreeMap::new(),
        }
    }

    fn analysis(spans: Vec<Span>) -> TraceAnalysis {
        build_analysis("api", window(), vec![Trace::from_spans("t1", spans)])
    }

    fn sample() -> TraceAnalysis {
        analysis(vec![
            span("api", "GET /cart", 12.0, SpanStatus::Ok),
            span("api", "GET /cart", 18.0, SpanStatus::Ok),
            span("db", "SELECT items", 90.0, SpanStatus::Error),
        ])
    }

    #[test]
    fn the_terminal_section_reports_trace_and_span_counts() {
        let out = render_terminal(&sample());
        assert!(out.contains("Traces Analyzed: 1"), "{out}");
        assert!(out.contains("Total Spans:     3"), "{out}");
        assert!(out.contains("Errored Spans:   1"), "{out}");
    }

    #[test]
    fn the_terminal_table_is_ordered_slowest_p95_first() {
        let out = render_terminal(&sample());
        let db = out.find("SELECT items").expect("db row");
        let api = out.find("GET /cart").expect("api row");
        assert!(db < api, "the 90ms span must precede the 15ms one:\n{out}");
    }

    #[test]
    fn terminal_rows_are_fixed_width_and_carry_millisecond_units() {
        let agg = &sample().spans[0];
        let row = terminal_row(agg);
        assert!(row.starts_with("db "), "{row}");
        assert!(row.contains("90.00ms"), "{row}");
        // Service and span columns are padded to their declared widths.
        assert!(row.len() >= SERVICE_COL + SPAN_COL);
    }

    #[test]
    fn an_empty_analysis_renders_an_explanation_instead_of_an_empty_table() {
        let out = render_terminal(&build_analysis("api", window(), vec![]));
        assert!(out.contains("No spans were found"), "{out}");
        assert!(!out.contains("P95"), "{out}");

        let md = render_markdown(&build_analysis("api", window(), vec![]));
        assert!(md.contains("No spans were found"), "{md}");
        assert!(!md.contains("|---"), "{md}");
    }

    #[test]
    fn the_error_line_is_omitted_when_no_span_failed() {
        let clean = analysis(vec![span("api", "GET /cart", 5.0, SpanStatus::Ok)]);
        assert!(!render_terminal(&clean).contains("Errored"));
        assert!(!render_markdown(&clean).contains("errored"));
    }

    #[test]
    fn the_markdown_section_has_a_heading_and_one_row_per_group() {
        let md = render_markdown(&sample());
        assert!(md.starts_with("## Trace Analysis"), "{md}");
        let data_rows = md
            .lines()
            .filter(|l| l.starts_with("| ") && !l.contains("Count"))
            .count();
        assert_eq!(data_rows, 2, "{md}");
        assert!(md.contains("| db | SELECT items | 1 | 1 | 90.00 |"), "{md}");
    }

    #[test]
    fn a_pipe_in_a_span_name_is_escaped_so_the_markdown_table_survives() {
        let md = render_markdown(&analysis(vec![span(
            "api",
            "GET /a|b",
            1.0,
            SpanStatus::Ok,
        )]));
        assert!(md.contains(r"GET /a\|b"), "{md}");
    }

    #[test]
    fn both_renderers_cap_the_table_and_say_how_many_rows_were_dropped() {
        let spans: Vec<Span> = (0..MAX_ROWS + 5)
            .map(|i| span("api", &format!("op-{i}"), i as f64, SpanStatus::Ok))
            .collect();
        let a = analysis(spans);
        assert_eq!(a.spans.len(), MAX_ROWS + 5);

        let terminal = render_terminal(&a);
        assert_eq!(terminal.matches("op-").count(), MAX_ROWS);
        assert!(terminal.contains("and 5 more span(s)"), "{terminal}");

        let md = render_markdown(&a);
        assert_eq!(md.matches("op-").count(), MAX_ROWS);
        assert!(md.contains("5 slower-sorted span group(s) omitted"), "{md}");
    }

    #[test]
    fn an_overlong_span_name_is_truncated_on_a_character_boundary() {
        let name = "ü".repeat(SPAN_COL + 10);
        let row = terminal_row(&analysis(vec![span("api", &name, 1.0, SpanStatus::Ok)]).spans[0]);
        assert!(row.contains('…'), "{row}");
        // Truncation must not have panicked or produced replacement bytes.
        assert!(row.contains(&"ü".repeat(SPAN_COL - 1)), "{row}");
    }

    #[test]
    fn raw_traces_round_trip_through_the_dump_file() {
        let dir = std::env::temp_dir().join(format!("gauntlet-tracing-{}", std::process::id()));
        let path = dir.join("traces.json");
        let a = sample();
        write_raw_traces(&path, &a).expect("writes");

        let text = std::fs::read_to_string(&path).expect("reads back");
        let traces: Vec<Trace> = serde_json::from_str(&text).expect("valid JSON");
        assert_eq!(traces, a.traces);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
