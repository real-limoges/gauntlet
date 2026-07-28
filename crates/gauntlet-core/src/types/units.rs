//! Time-unit newtypes. `Nanoseconds` wraps the raw clock delta, `Milliseconds`
//! is what the stats core consumes; both serialize as bare numbers.

use serde::{Deserialize, Serialize};

/// A duration in nanoseconds (raw monotonic-clock delta).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Nanoseconds(pub u64);

/// A duration in milliseconds (what `calculate_stats` consumes).
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Milliseconds(pub f64);

/// Convert nanoseconds to milliseconds: an exact `/ 1e6`, with no rounding.
pub fn ns_to_ms(ns: Nanoseconds) -> Milliseconds {
    Milliseconds(ns.0 as f64 / 1_000_000.0)
}
