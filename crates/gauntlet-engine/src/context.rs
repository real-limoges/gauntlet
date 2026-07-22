//! The shared per-run bundle threaded through every endpoint task: the pooled
//! HTTP client, an optional bearer token, the settings, and the optional CSV sink.

use std::sync::{Arc, Mutex};

use gauntlet_core::Settings;

use crate::csv_out::CsvSink;

/// Shared, cheaply-cloneable run state. The `reqwest::Client` is internally
/// reference-counted; the CSV sink is shared behind a mutex so concurrent
/// endpoint tasks can append rows.
pub struct RunContext {
    pub client: reqwest::Client,
    /// Bearer token applied when an endpoint has no explicit `Authorization`
    /// header. Auth via per-payload headers is the primary path in M3.
    pub token: Option<String>,
    pub settings: Settings,
    pub csv: Option<Arc<Mutex<CsvSink>>>,
    /// Live-UI event sink; `None` in headless runs.
    pub events: crate::event::EventSink,
}
