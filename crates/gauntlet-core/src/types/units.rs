//! Type-safe time-unit newtypes, ported from `Benchmark.Types.Units`.
//!
//! `Nanoseconds` wraps the raw clock delta; `Milliseconds` is what the stats
//! core consumes. Both are serde-transparent so they (de)serialize as bare
//! numbers, matching the Haskell `deriving newtype (FromJSON, ToJSON)`.

use serde::{Deserialize, Serialize};

/// A duration in nanoseconds (raw monotonic-clock delta).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Nanoseconds(pub u64);

/// A duration in milliseconds (what `calculate_stats` consumes).
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Milliseconds(pub f64);

/// Convert nanoseconds to milliseconds.
///
/// Mirrors the Haskell `nsToMs (Nanoseconds ns) = Milliseconds (fromIntegral ns / 1_000_000)`
/// — exact `/ 1e6` divisor, no rounding.
pub fn ns_to_ms(ns: Nanoseconds) -> Milliseconds {
    Milliseconds(ns.0 as f64 / 1_000_000.0)
}
