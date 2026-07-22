//! `gauntlet-tui` — the live terminal view over the engine's event stream.
//!
//! The benchmark runs as a task and streams [`BenchmarkEvent`]s; this crate
//! renders whatever arrives. The measurement never waits on the UI (ADR M5-tui
//! §1), and the UI going away never disturbs the measurement.
//!
//! [`run`] owns the terminal: raw mode, alternate screen, and — importantly —
//! restoring both even if a render panics (§3).

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
///
/// Returns [`Exit::Cancelled`] if the operator interrupted, which the caller
/// must treat as a failed run — interrupted measurements are not results.
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
    let mut last_draw = Instant::now() - FRAME;

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

        if last_draw.elapsed() >= FRAME {
            terminal.draw(|f| ui::draw(f, &state))?;
            last_draw = Instant::now();
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

/// Take over the terminal, installing a panic hook that gives it back.
///
/// Without the hook, a panic while in raw mode on the alternate screen leaves
/// the operator with a terminal that has no echo and no prompt.
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
