//! The shared per-target bundle threaded through every endpoint task.

use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use gauntlet_core::Settings;

use crate::csv_out::CsvSink;
use crate::rate_limiter::RateLimiter;

/// Shared, cheaply-cloneable run state. The `reqwest::Client` is internally
/// reference-counted; the CSV sink sits behind a mutex so concurrent endpoint
/// tasks can append rows.
#[derive(Debug)]
pub struct RunContext {
    pub client: reqwest::Client,
    /// Bearer token, applied when an endpoint sets no `Authorization` header.
    pub token: Option<String>,
    pub settings: Settings,
    pub csv: Option<Arc<Mutex<CsvSink>>>,
    /// Live-UI event sink; `None` in headless runs.
    pub events: crate::event::EventSink,
    /// Dispatch pacing, shared by the target's endpoints. `None` for
    /// `Unthrottled`. See the crate docs for why it is per target.
    pub limiter: Option<Arc<RateLimiter>>,
    /// When the target's measurement window opened, so duration-based modes
    /// share one deadline.
    pub started_at: Instant,
    /// Requests completed so far, feeding the live view's achieved-rate readout.
    /// `Relaxed`: it drives a display, never a measurement.
    pub completed: Arc<AtomicUsize>,
}
