//! Rendering for the live view.
//!
//! Every function here is a pure read of [`State`] (ADR M5-tui §2): no clock,
//! no mutation, no I/O. That keeps the frame budget honest and makes the whole
//! module testable against a `TestBackend` buffer.
//!
//! The layout is a stack of horizontal bands assembled at draw time, because
//! two of them — load control and recent errors — only exist for some runs. A
//! band that has nothing to say is omitted rather than drawn empty, so a short
//! terminal spends its rows on the sections that carry information.
//!
//! **Nothing in here may panic.** A panic mid-render happens in raw mode on the
//! alternate screen, and the operator gets their terminal back only because of
//! the hook in `lib.rs` — which is a safety net, not a licence. So: no
//! indexing, no unchecked slicing, no `Gauge::ratio` with a value that has not
//! been forced into `0.0..=1.0` (it asserts), and no assumption that any band
//! received the height it asked for.

use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Cell, Gauge, Paragraph, Row, Sparkline, Table};
use ratatui::Frame;

use crate::state::State;

/// Glyphs for one request on the timeline strip. Distinct shapes, not just
/// distinct colours: the strip has to stay readable when it is piped, screen-
/// shotted in monochrome, or read by someone who cannot separate red from
/// green.
const MARK_OK: &str = "▪";
const MARK_ERR: &str = "▫";

/// The bands of the vertical stack, in draw order.
///
/// Modelled as data so the conditional bands can be filtered out before the
/// layout is solved — the alternative, nested layouts with zero-height
/// placeholders, still consumes rows on a 10-row terminal.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Band {
    Header,
    Error,
    Gauge,
    Stats,
    Metrics,
    Load,
    Sparkline,
    Timeline,
    Errors,
    Footer,
}

impl Band {
    /// Rows this band wants. `Min` marks the one band that absorbs slack.
    fn constraint(self, state: &State) -> Constraint {
        match self {
            // Title line plus the status line beneath it.
            Band::Header => Constraint::Length(2),
            Band::Error => Constraint::Length(1),
            Band::Gauge => Constraint::Length(1),
            // Column headings plus the single row of values (or the
            // placeholder, which is deliberately given the same height so the
            // stack does not jump when the first request lands).
            Band::Stats => Constraint::Length(2),
            Band::Metrics => Constraint::Length(1),
            Band::Load => Constraint::Length(1),
            // The only elastic band: the latency trend grows into whatever is
            // left and shrinks to nothing on a cramped terminal.
            Band::Sparkline => Constraint::Min(0),
            Band::Timeline => Constraint::Length(1),
            // One heading line plus one line per retained error.
            Band::Errors => Constraint::Length(1 + state.recent_errors.len() as u16),
            Band::Footer => Constraint::Length(1),
        }
    }
}

/// Draw one frame.
pub fn draw(frame: &mut Frame, state: &State) {
    let bands: Vec<Band> = [
        Band::Header,
        Band::Error,
        Band::Gauge,
        Band::Stats,
        Band::Metrics,
        Band::Load,
        Band::Sparkline,
        Band::Timeline,
        Band::Errors,
        Band::Footer,
    ]
    .into_iter()
    .filter(|band| match band {
        Band::Error => state.error.is_some(),
        Band::Load => has_load_control(state),
        Band::Errors => !state.recent_errors.is_empty(),
        _ => true,
    })
    .collect();

    let constraints: Vec<Constraint> = bands.iter().map(|b| b.constraint(state)).collect();
    let areas = Layout::vertical(constraints).split(frame.area());

    // `split` returns exactly one area per constraint, but zipping rather than
    // indexing means a future mismatch degrades into a missing band instead of
    // a panic.
    for (band, area) in bands.iter().zip(areas.iter()) {
        if area.height == 0 || area.width == 0 {
            continue;
        }
        match band {
            Band::Header => header(frame, state, *area),
            Band::Error => error_banner(frame, state, *area),
            Band::Gauge => progress(frame, state, *area),
            Band::Stats => stats(frame, state, *area),
            Band::Metrics => metrics(frame, state, *area),
            Band::Load => load_control(frame, state, *area),
            Band::Sparkline => latency_trend(frame, state, *area),
            Band::Timeline => timeline(frame, state, *area),
            Band::Errors => errors(frame, state, *area),
            Band::Footer => footer(frame, *area),
        }
    }
}

/// Target, endpoint, elapsed time, and the status line.
fn header(frame: &mut Frame, state: &State, area: Rect) {
    let target = if state.target.is_empty() {
        "gauntlet"
    } else {
        &state.target
    };

    let mut title = vec![Span::styled(
        target.to_string(),
        Style::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD),
    )];
    if !state.endpoint.is_empty() {
        // The index is 1-based from the engine; show it only when there is
        // more than one endpoint, where it actually tells the operator
        // something about how far through the target they are.
        let position = if state.total_endpoints > 1 {
            format!(" ({}/{})", state.endpoint_index, state.total_endpoints)
        } else {
            String::new()
        };
        title.push(Span::raw(format!("  {}{}", state.endpoint, position)));
    }
    title.push(Span::styled(
        format!("  [{}]", format_elapsed(state.elapsed_s)),
        Style::default().fg(Color::DarkGray),
    ));

    // "finished" outranks whatever status the engine last set: once the run is
    // over, a stale "warming up..." would be actively misleading.
    let status = if state.error.is_some() {
        Span::styled("failed", Style::default().fg(Color::Red))
    } else if state.finished {
        Span::styled("finished", Style::default().fg(Color::Green))
    } else {
        Span::styled(state.status.clone(), Style::default().fg(Color::Gray))
    };

    frame.render_widget(
        Paragraph::new(vec![Line::from(title), Line::from(status)]),
        area,
    );
}

/// The fatal error, given its own line at the top where it cannot be missed.
fn error_banner(frame: &mut Frame, state: &State, area: Rect) {
    let Some(message) = state.error.as_deref() else {
        return;
    };
    frame.render_widget(
        Paragraph::new(Line::from(Span::styled(
            truncate(&format!("error: {message}"), area.width as usize),
            Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
        ))),
        area,
    );
}

/// Completion gauge. Duration-based modes report a 0 ratio (see
/// `State::progress`), which draws an empty bar rather than a fake one.
fn progress(frame: &mut Frame, state: &State, area: Rect) {
    // `Gauge::ratio` asserts on anything outside 0..=1, NaN included.
    let ratio = state.progress();
    let ratio = if ratio.is_finite() {
        ratio.clamp(0.0, 1.0)
    } else {
        0.0
    };

    let label = if state.total_requests == 0 {
        format!("{} requests", state.completed)
    } else {
        format!(
            "{}/{} ({:.0}%)",
            state.completed,
            state.total_requests,
            ratio * 100.0
        )
    };

    frame.render_widget(
        Gauge::default()
            .ratio(ratio)
            .label(label)
            .gauge_style(Style::default().fg(Color::Green))
            .use_unicode(true),
        area,
    );
}

/// Rolling latency percentiles, or a placeholder before the first response.
fn stats(frame: &mut Frame, state: &State, area: Rect) {
    let Some(rolling) = state.rolling else {
        // A table of zeroes reads as a measurement; this reads as "not yet".
        frame.render_widget(
            Paragraph::new(Line::from(Span::styled(
                "waiting for first request",
                Style::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            ))),
            area,
        );
        return;
    };

    let labels = ["mean", "p50", "p95", "p99", "min", "max"];
    let values = [
        rolling.mean_ms,
        rolling.p50_ms,
        rolling.p95_ms,
        rolling.p99_ms,
        rolling.min_ms,
        rolling.max_ms,
    ];

    let heading = Row::new(labels.iter().map(|l| Cell::from(*l)).collect::<Vec<_>>())
        .style(Style::default().fg(Color::DarkGray));
    let row = Row::new(
        values
            .iter()
            .map(|v| Cell::from(format_ms(*v)))
            .collect::<Vec<_>>(),
    )
    .style(Style::default().fg(Color::White));

    // Equal ratios rather than fixed widths: the row has to stay readable on a
    // 40-column terminal, where fixed columns would overflow off the edge.
    let widths = [Constraint::Ratio(1, labels.len() as u32); 6];
    frame.render_widget(Table::new(vec![row], widths).header(heading), area);
}

/// Throughput and error rate — the two numbers that are not percentiles.
fn metrics(frame: &mut Frame, state: &State, area: Rect) {
    let error_rate = state.error_rate();
    // Red only once something has actually failed; a 0% rate in red would
    // train the operator to ignore the colour.
    let error_style = if state.error_count > 0 {
        Style::default().fg(Color::Red)
    } else {
        Style::default().fg(Color::Green)
    };

    let line = Line::from(vec![
        Span::styled("throughput ", Style::default().fg(Color::DarkGray)),
        Span::raw(format!("{:.1} req/s", state.throughput())),
        Span::styled("   ok ", Style::default().fg(Color::DarkGray)),
        Span::raw(format!("{}", state.success_count)),
        Span::styled("   errors ", Style::default().fg(Color::DarkGray)),
        Span::styled(
            format!("{} ({:.1}%)", state.error_count, error_rate * 100.0),
            error_style,
        ),
    ]);
    frame.render_widget(Paragraph::new(line), area);
}

/// True when the run is throttled and the load figures mean something.
fn has_load_control(state: &State) -> bool {
    state.current_rpm.is_some() || state.target_rpm.is_some() || state.current_step.is_some()
}

/// Rate-limited runs only: where the load generator currently is.
fn load_control(frame: &mut Frame, state: &State, area: Rect) {
    let mut parts = Vec::new();
    if let Some(step) = state.current_step {
        parts.push(format!("step {step}"));
    }
    if let Some(rpm) = state.current_rpm {
        parts.push(format!("current {}", format_rpm(rpm)));
    }
    if let Some(rpm) = state.target_rpm {
        parts.push(format!("target {}", format_rpm(rpm)));
    }

    frame.render_widget(
        Paragraph::new(Line::from(vec![
            Span::styled("load  ", Style::default().fg(Color::DarkGray)),
            Span::styled(parts.join("   "), Style::default().fg(Color::Yellow)),
        ])),
        area,
    );
}

/// Sparkline over the rolling latency window.
fn latency_trend(frame: &mut Frame, state: &State, area: Rect) {
    let block = Block::default()
        .borders(Borders::ALL)
        .title(" latency trend ");
    // With a border there may be no interior left at all; render the frame and
    // stop rather than computing a negative inner width.
    let inner = block.inner(area);
    frame.render_widget(block, area);
    if inner.width == 0 || inner.height == 0 {
        return;
    }

    // The sparkline draws its data left-to-right and would otherwise show the
    // *oldest* samples when the window is wider than the pane, so keep the
    // tail ourselves. Non-finite samples become 0 — a NaN would otherwise
    // poison the max used for bar scaling.
    let width = inner.width as usize;
    let skip = state.recent_latencies.len().saturating_sub(width);
    let data: Vec<u64> = state
        .recent_latencies
        .iter()
        .skip(skip)
        .map(|ms| {
            if ms.is_finite() && *ms > 0.0 {
                // Sub-millisecond responses would all floor to 0 and flatten
                // the chart, so scale to microseconds before rounding.
                (ms * 1000.0).round().min(u64::MAX as f64) as u64
            } else {
                0
            }
        })
        .collect();

    frame.render_widget(
        Sparkline::default()
            .data(&data)
            .style(Style::default().fg(Color::Cyan)),
        inner,
    );
}

/// The per-request success/failure strip — the one widget ratatui has no
/// equivalent for (ADR M5-tui §6).
///
/// The deque is oldest-first and holds more marks than a narrow terminal can
/// show, so this keeps the tail: what just happened is what the operator is
/// watching for.
fn timeline(frame: &mut Frame, state: &State, area: Rect) {
    let width = area.width as usize;
    let skip = state.timeline.len().saturating_sub(width);
    let marks: Vec<Span> = state
        .timeline
        .iter()
        .skip(skip)
        .map(|ok| {
            if *ok {
                Span::styled(MARK_OK, Style::default().fg(Color::Green))
            } else {
                Span::styled(MARK_ERR, Style::default().fg(Color::Red))
            }
        })
        .collect();

    frame.render_widget(Paragraph::new(Line::from(marks)), area);
}

/// The last few failure messages, newest last.
fn errors(frame: &mut Frame, state: &State, area: Rect) {
    let width = area.width as usize;
    let mut lines = vec![Line::from(Span::styled(
        "recent errors",
        Style::default().fg(Color::DarkGray),
    ))];
    lines.extend(state.recent_errors.iter().map(|message| {
        Line::from(Span::styled(
            truncate(&format!("• {message}"), width),
            Style::default().fg(Color::Red),
        ))
    }));

    frame.render_widget(Paragraph::new(lines), area);
}

fn footer(frame: &mut Frame, area: Rect) {
    frame.render_widget(
        Paragraph::new(Line::from(Span::styled(
            "q / Esc / Ctrl-C to cancel",
            Style::default().fg(Color::DarkGray),
        ))),
        area,
    );
}

/// Cut to a character count, not a byte count — an error message may well be
/// UTF-8, and byte slicing it would panic mid-codepoint.
fn truncate(text: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    if text.chars().count() <= width {
        return text.to_string();
    }
    // Reserve the last column for the ellipsis, unless there is only one.
    let keep = width.saturating_sub(1);
    text.chars()
        .take(keep)
        .chain(std::iter::once('…'))
        .collect()
}

/// Milliseconds at a scale a human reads without counting zeroes, matching the
/// Haskell `formatDuration`.
fn format_ms(ms: f64) -> String {
    if !ms.is_finite() {
        "-".to_string()
    } else if ms < 1.0 {
        format!("{:.0}µs", ms * 1000.0)
    } else if ms < 1000.0 {
        format!("{ms:.1}ms")
    } else {
        format!("{:.2}s", ms / 1000.0)
    }
}

/// Requests per minute, matching the Haskell `formatRPM`.
fn format_rpm(rpm: f64) -> String {
    if !rpm.is_finite() {
        "-".to_string()
    } else if rpm < 1.0 {
        "<1 rpm".to_string()
    } else if rpm < 10.0 {
        format!("{rpm:.1} rpm")
    } else {
        format!("{rpm:.0} rpm")
    }
}

/// Elapsed seconds as `MM:SS`, or `HH:MM:SS` past an hour — the Haskell
/// `formatElapsed`.
fn format_elapsed(seconds: f64) -> String {
    if !seconds.is_finite() || seconds < 0.0 {
        return "00:00".to_string();
    }
    let total = seconds.round().min(u64::MAX as f64) as u64;
    let (hours, minutes, secs) = (total / 3600, (total % 3600) / 60, total % 60);
    if hours > 0 {
        format!("{hours:02}:{minutes:02}:{secs:02}")
    } else {
        format!("{minutes:02}:{secs:02}")
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use ratatui::backend::TestBackend;
    use ratatui::Terminal;

    use super::*;
    use crate::state::RollingStats;

    /// Render headlessly and flatten the buffer to one string per row, so
    /// assertions can just look for text.
    fn render(state: &State, width: u16, height: u16) -> String {
        let mut terminal = Terminal::new(TestBackend::new(width, height)).expect("test backend");
        terminal.draw(|f| draw(f, state)).expect("draw");

        let buffer = terminal.backend().buffer().clone();
        (0..buffer.area.height)
            .map(|y| {
                (0..buffer.area.width)
                    .map(|x| buffer[(x, y)].symbol().to_string())
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn running_state() -> State {
        State {
            target: "staging-api".into(),
            endpoint: "checkout".into(),
            endpoint_index: 2,
            total_endpoints: 3,
            completed: 40,
            total_requests: 100,
            success_count: 38,
            error_count: 2,
            elapsed_s: 12.0,
            status: "benchmarking".into(),
            recent_latencies: VecDeque::from(vec![10.0, 12.0, 9.0, 40.0, 11.0]),
            rolling: Some(RollingStats {
                mean_ms: 16.4,
                p50_ms: 11.0,
                p95_ms: 40.0,
                p99_ms: 40.0,
                min_ms: 9.0,
                max_ms: 40.0,
            }),
            timeline: VecDeque::from(vec![true, true, false, true]),
            ..State::default()
        }
    }

    #[test]
    fn a_running_benchmark_renders_its_target_and_progress_at_a_normal_size() {
        let output = render(&running_state(), 80, 24);

        assert!(output.contains("staging-api"), "{output}");
        assert!(output.contains("checkout"), "the endpoint is named");
        assert!(output.contains("(2/3)"), "with its position in the target");
        assert!(output.contains("40/100"), "and the completion count");
        assert!(
            output.contains("00:12"),
            "elapsed time is formatted as MM:SS"
        );
        assert!(output.contains("q / Esc / Ctrl-C to cancel"));
    }

    #[test]
    fn rendering_survives_a_terminal_far_too_small_for_the_layout() {
        // Every band cannot fit; the requirement is only that nothing panics
        // and the terminal is left in a drawable state.
        for (width, height) in [(40, 10), (20, 5), (10, 3), (1, 1)] {
            let _ = render(&running_state(), width, height);
        }
    }

    #[test]
    fn a_state_with_errors_and_load_control_also_renders_when_tiny() {
        // The conditional bands push the stack past what a small terminal has,
        // which is exactly where a naive layout would over-constrain.
        let mut state = running_state();
        state.current_rpm = Some(600.0);
        state.target_rpm = Some(1200.0);
        state.current_step = Some(2);
        state.recent_errors = VecDeque::from(vec!["connection refused".to_string(); 5]);
        state.error = Some("target unreachable".into());

        for (width, height) in [(40, 10), (20, 5), (8, 2)] {
            let _ = render(&state, width, height);
        }
    }

    #[test]
    fn statistics_are_replaced_by_a_placeholder_until_the_first_response() {
        let mut state = running_state();
        state.rolling = None;

        let waiting = render(&state, 80, 24);
        assert!(waiting.contains("waiting for first request"), "{waiting}");
        assert!(!waiting.contains("p95"), "no column headings without data");

        let measured = render(&running_state(), 80, 24);
        assert!(measured.contains("p95"), "{measured}");
        assert!(measured.contains("40.0ms"), "the p95 value is shown");
        assert!(measured.contains("9.0ms"), "and the minimum");
    }

    #[test]
    fn the_load_control_line_appears_only_for_a_throttled_run() {
        let unthrottled = render(&running_state(), 80, 24);
        assert!(!unthrottled.contains("load "), "{unthrottled}");

        let mut state = running_state();
        state.current_rpm = Some(612.0);
        state.target_rpm = Some(1200.0);
        state.current_step = Some(3);

        let throttled = render(&state, 80, 24);
        assert!(throttled.contains("step 3"), "{throttled}");
        assert!(throttled.contains("612 rpm"), "the achieved rate");
        assert!(
            throttled.contains("1200 rpm"),
            "and the rate being asked for"
        );
    }

    #[test]
    fn a_failed_run_shows_the_error_text_prominently() {
        let mut state = running_state();
        state.finished = true;
        state.error = Some("target unreachable".into());

        let output = render(&state, 80, 24);
        assert!(output.contains("target unreachable"), "{output}");
        assert!(output.contains("failed"), "and the status says so");
    }

    #[test]
    fn recent_errors_are_listed_and_clipped_to_the_terminal_width() {
        let mut state = running_state();
        state.recent_errors =
            VecDeque::from(vec!["connection refused".to_string(), "a".repeat(500)]);

        let output = render(&state, 40, 24);
        assert!(output.contains("recent errors"), "{output}");
        assert!(output.contains("connection refused"));
        for line in output.lines() {
            assert!(line.chars().count() <= 40, "no line overflows: {line:?}");
        }
    }

    #[test]
    fn the_timeline_keeps_the_most_recent_marks_when_wider_than_the_terminal() {
        let mut state = running_state();
        // Far more marks than columns: the oldest must be dropped, not drawn.
        state.timeline = (0..500).map(|i| i % 7 != 0).collect();

        let output = render(&state, 20, 24);
        for line in output.lines() {
            assert!(line.chars().count() <= 20, "{line:?}");
        }
        assert!(
            output.contains(MARK_OK),
            "the strip is still drawn: {output}"
        );
    }

    #[test]
    fn a_default_state_renders_before_any_event_has_arrived() {
        let output = render(&State::default(), 80, 24);
        assert!(output.contains("gauntlet"), "a placeholder title: {output}");
        assert!(output.contains("waiting for first request"));
        assert!(
            output.contains("0 requests"),
            "no total is known yet, so no ratio is implied: {output}"
        );
    }

    #[test]
    fn durations_are_formatted_at_a_readable_scale() {
        assert_eq!(format_ms(0.25), "250µs");
        assert_eq!(format_ms(12.34), "12.3ms");
        assert_eq!(format_ms(4500.0), "4.50s");
        assert_eq!(format_ms(f64::NAN), "-");
    }

    #[test]
    fn elapsed_time_gains_an_hours_field_only_past_an_hour() {
        assert_eq!(format_elapsed(0.0), "00:00");
        assert_eq!(format_elapsed(-5.0), "00:00");
        assert_eq!(format_elapsed(65.0), "01:05");
        assert_eq!(format_elapsed(3725.0), "01:02:05");
    }

    #[test]
    fn truncation_is_by_character_and_never_splits_a_codepoint() {
        assert_eq!(truncate("hello", 10), "hello");
        assert_eq!(truncate("hello", 0), "");
        assert_eq!(truncate("hello", 3), "he…");
        assert_eq!(truncate("héllo wörld", 5), "héll…");
    }
}
