//! `gauntlet-tui` — the live terminal view over the engine's event stream.
//!
//! The benchmark runs as a task and streams [`BenchmarkEvent`]s; this crate
//! renders whatever arrives. The measurement never waits on the UI, and the UI
//! going away never disturbs the measurement.
//!
//! # The split
//!
//! [`state::State`] is pure data with a pure reducer — it takes `now` rather than
//! reading the clock, so it is deterministic under test. Everything worth testing
//! (rolling windows, counters, percentile recomputation) lives there and is
//! tested with no terminal involved. [`ui`] only ever reads a `&State`.
//!
//! The layout is a stack of horizontal bands assembled at draw time, because two
//! of them — load control and recent errors — only exist for some runs. A band
//! with nothing to say is omitted rather than drawn empty, so a short terminal
//! spends its rows on sections that carry information. The bands are modelled as
//! data so the conditional ones can be filtered out *before* the layout is
//! solved; nested layouts with zero-height placeholders still consume rows on a
//! 10-row terminal.
//!
//! # Nothing in the render path may panic
//!
//! [`run`] owns the terminal — raw mode and the alternate screen — and installs a
//! panic hook that gives it back, because a panic while in raw mode otherwise
//! leaves the operator with no echo and no prompt. That hook is a safety net, not
//! a licence. So in `ui`: no indexing, no unchecked slicing, no `Gauge::ratio`
//! with a value that has not been forced into `0.0..=1.0` (it asserts), and no
//! assumption that a band received the height it asked for.
//!
//! # Rendering choices
//!
//! Timeline glyphs are distinct *shapes*, not just distinct colours, so the strip
//! stays readable piped, screenshotted in monochrome, or read by someone who
//! cannot separate red from green. Widgets that show a window of samples keep the
//! **tail** rather than the head: what just happened is what the operator is
//! watching for.

// Production code must not panic: an unwrap that fires mid-run destroys the
// whole measurement, and a benchmark that dies is worse than one reporting a
// clean error. `cfg(not(test))` scopes this to real code; inside `#[cfg(test)]`
// modules, panicking assertions and exact float comparisons are the point.
#![cfg_attr(
    not(test),
    deny(
        clippy::unwrap_used,
        clippy::panic,
        clippy::unreachable,
        clippy::panic_in_result_fn,
        clippy::float_cmp
    )
)]

pub mod state;
pub mod ui;

use std::io::{self, Stdout};
use std::time::{Duration, Instant};

use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::{execute, ExecutableCommand};
use gauntlet_engine::BenchmarkEvent;
use ratatui::backend::CrosstermBackend;
use ratatui::Terminal;
use tokio::sync::mpsc::UnboundedReceiver;

pub use state::State;

/// How the live view ended.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Exit {
    /// The stream reported the benchmark finished.
    Completed,
    /// The operator pressed q / Esc / Ctrl-C.
    Cancelled,
}

/// Frame budget. Redrawing per event would spend the run's CPU on rendering at
/// high request rates; 20fps is smooth and cheap.
const FRAME: Duration = Duration::from_millis(50);

/// Drive the live view until the benchmark finishes or the operator quits.
/// [`Exit::Cancelled`] must be treated as a failed run by the caller: an
/// interrupted measurement is not a result.
pub async fn run(mut events: UnboundedReceiver<BenchmarkEvent>) -> io::Result<Exit> {
    let mut terminal = enter()?;
    let outcome = event_loop(&mut terminal, &mut events).await;
    leave(&mut terminal)?;
    outcome
}

async fn event_loop(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    events: &mut UnboundedReceiver<BenchmarkEvent>,
) -> io::Result<Exit> {
    let mut state = State::default();
    // `None` until the first frame lands, forcing an immediate first draw. The
    // obvious `Instant::now() - FRAME` panics where the monotonic clock's origin
    // is less than one frame in the past.
    let mut last_draw: Option<Instant> = None;

    loop {
        // Drain everything queued before drawing: at high request rates many
        // events land between frames, and folding them all in keeps the
        // displayed counters honest rather than lagging by a frame.
        while let Ok(event) = events.try_recv() {
            let finished = matches!(
                event,
                BenchmarkEvent::Finished | BenchmarkEvent::Failed { .. }
            );
            state.reduce(Instant::now(), event);
            if finished {
                terminal.draw(|f| ui::draw(f, &state))?;
                return Ok(Exit::Completed);
            }
        }

        if quit_requested()? {
            return Ok(Exit::Cancelled);
        }

        if last_draw.is_none_or(|t| t.elapsed() >= FRAME) {
            terminal.draw(|f| ui::draw(f, &state))?;
            last_draw = Some(Instant::now());
        }

        // Yield to the runtime so the benchmark tasks make progress; this loop
        // is otherwise a spin.
        tokio::time::sleep(FRAME / 5).await;

        if events.is_closed() && events.is_empty() {
            // The engine dropped its sender without a terminal event — treat a
            // vanished stream as completion rather than hanging forever.
            terminal.draw(|f| ui::draw(f, &state))?;
            return Ok(Exit::Completed);
        }
    }
}

/// Non-blocking check for a quit key.
fn quit_requested() -> io::Result<bool> {
    if !crossterm::event::poll(Duration::ZERO)? {
        return Ok(false);
    }
    Ok(match crossterm::event::read()? {
        Event::Key(KeyEvent {
            code, modifiers, ..
        }) => matches!(
            (code, modifiers),
            (KeyCode::Char('q'), _)
                | (KeyCode::Esc, _)
                | (KeyCode::Char('c'), KeyModifiers::CONTROL)
        ),
        _ => false,
    })
}

/// Take over the terminal, installing a panic hook that gives it back. See the
/// crate docs.
fn enter() -> io::Result<Terminal<CrosstermBackend<Stdout>>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    stdout.execute(EnterAlternateScreen)?;

    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = restore();
        previous(info);
    }));

    Terminal::new(CrosstermBackend::new(io::stdout()))
}

fn leave(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> io::Result<()> {
    restore()?;
    terminal.show_cursor()
}

fn restore() -> io::Result<()> {
    let _ = disable_raw_mode();
    execute!(io::stdout(), LeaveAlternateScreen)
}
