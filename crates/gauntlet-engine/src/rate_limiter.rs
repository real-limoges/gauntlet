//! Request pacing for every load mode, via atomic slot reservation. See the
//! crate docs for the mechanism and the deadline ordering it must respect.

use std::time::{Duration, Instant};

use rand::Rng;
use tokio::sync::Mutex;

use gauntlet_core::{LoadMode, LoadStep};

/// Minimum RPM for time-varying modes, so a ramp starting near zero still moves.
const MIN_RPM: f64 = 6.0;

/// Paces request dispatch for a single endpoint run. `Unthrottled` has no limiter
/// (`new` returns `None`); all other modes reserve slots off a shared clock.
#[derive(Debug)]
pub struct RateLimiter {
    mode: LoadMode,
    start: Instant,
    /// Next free slot, in seconds since `start`.
    next_slot: Mutex<f64>,
}

impl RateLimiter {
    /// A limiter for `mode` anchored at `start`, or `None` for `Unthrottled`
    /// (no pacing).
    pub fn new(mode: LoadMode, start: Instant) -> Option<Self> {
        match mode {
            LoadMode::Unthrottled => None,
            _ => Some(RateLimiter {
                mode,
                start,
                next_slot: Mutex::new(0.0),
            }),
        }
    }

    /// Claim the next slot and sleep until it arrives.
    pub async fn wait_for_slot(&self) {
        let target = self.claim().await;
        Self::sleep_until(target).await;
    }

    /// As [`wait_for_slot`](Self::wait_for_slot), but abandons the slot and
    /// returns `false` when it falls at or after `deadline`, checking *before*
    /// sleeping. See the crate docs for why that ordering matters.
    pub async fn wait_for_slot_before(&self, deadline: Instant) -> bool {
        let target = self.claim().await;
        if target >= deadline {
            return false;
        }
        Self::sleep_until(target).await;
        true
    }

    /// Reserve the next slot, returning the instant it falls at.
    async fn claim(&self) -> Instant {
        let claimed = {
            let mut next = self.next_slot.lock().await;
            let claimed = *next;
            *next = claimed + self.interval_at(claimed);
            claimed
        };
        self.start + Duration::from_secs_f64(claimed.max(0.0))
    }

    async fn sleep_until(target: Instant) {
        let now = Instant::now();
        if target > now {
            tokio::time::sleep(target - now).await;
        }
    }

    /// Interval (seconds) until the next request, for a slot scheduled at `t`
    /// seconds since `start`.
    fn interval_at(&self, t: f64) -> f64 {
        match &self.mode {
            LoadMode::Unthrottled => 0.0,
            LoadMode::ConstantRpm { target_rpm } => 60.0 / target_rpm,
            LoadMode::PoissonRpm { target_rpm } => {
                // Exponential interarrival: -ln(U(0,1]) · mean-interval.
                let u: f64 = rand::thread_rng().gen_range(1e-9..=1.0);
                -u.ln() * 60.0 / target_rpm
            }
            LoadMode::RampUp {
                start_rpm,
                end_rpm,
                duration_secs,
            } => {
                let progress = (t / duration_secs).clamp(0.0, 1.0);
                let rpm = (start_rpm + (end_rpm - start_rpm) * progress).max(MIN_RPM);
                60.0 / rpm
            }
            LoadMode::StepLoad { steps } => 60.0 / step_rpm(steps, t).max(MIN_RPM),
        }
    }
}

/// RPM of the step active at elapsed time `t`; past the last step, the last
/// step's RPM holds.
fn step_rpm(steps: &[LoadStep], t: f64) -> f64 {
    step_at(steps, t).map_or(MIN_RPM, |(_, rpm)| rpm)
}

/// The step active at elapsed time `t`, as a 1-based index and its RPM. Past the
/// last step, the last step holds. Exposed for the live view's step readout.
pub fn step_at(steps: &[LoadStep], t: f64) -> Option<(usize, f64)> {
    let mut cum = 0.0;
    for (index, step) in steps.iter().enumerate() {
        cum += step.duration_secs;
        if t < cum {
            return Some((index + 1, step.rpm));
        }
    }
    steps.last().map(|s| (steps.len(), s.rpm))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lim(mode: LoadMode) -> RateLimiter {
        RateLimiter {
            mode,
            start: Instant::now(),
            next_slot: Mutex::new(0.0),
        }
    }

    #[test]
    fn constant_rpm_interval_is_sixty_over_rpm() {
        let l = lim(LoadMode::ConstantRpm { target_rpm: 120.0 });
        assert!((l.interval_at(0.0) - 0.5).abs() < 1e-12);
    }

    #[test]
    fn ramp_interval_tracks_progress_and_floors_at_min_rpm() {
        let l = lim(LoadMode::RampUp {
            start_rpm: 60.0,
            end_rpm: 600.0,
            duration_secs: 10.0,
        });
        // t=0 → 60 rpm → 1.0s; t=10 (end) → 600 rpm → 0.1s.
        assert!((l.interval_at(0.0) - 1.0).abs() < 1e-9);
        assert!((l.interval_at(10.0) - 0.1).abs() < 1e-9);

        // A ramp that would dip below the floor is clamped to 6 rpm (10s interval).
        let low = lim(LoadMode::RampUp {
            start_rpm: 1.0,
            end_rpm: 1.0,
            duration_secs: 10.0,
        });
        assert!((low.interval_at(0.0) - 10.0).abs() < 1e-9);
    }

    #[test]
    fn step_load_selects_active_step_then_holds_last() {
        let steps = vec![
            LoadStep {
                rpm: 60.0,
                duration_secs: 2.0,
            },
            LoadStep {
                rpm: 120.0,
                duration_secs: 2.0,
            },
        ];
        assert!((step_rpm(&steps, 0.0) - 60.0).abs() < 1e-12); // first step
        assert!((step_rpm(&steps, 3.0) - 120.0).abs() < 1e-12); // second step
        assert!((step_rpm(&steps, 99.0) - 120.0).abs() < 1e-12); // past end → last
    }

    #[test]
    fn unthrottled_has_no_limiter() {
        assert!(RateLimiter::new(LoadMode::Unthrottled, Instant::now()).is_none());
    }

    #[tokio::test]
    async fn a_slot_past_the_deadline_is_abandoned_rather_than_slept_to() {
        // 60 rpm → 1s spacing. Ten workers claim slots at 0s..9s up front; with a
        // deadline 2s out, only the slots before it may be used, and the rest
        // must return immediately instead of sleeping out the full 9 seconds.
        //
        // The limiter is built with an explicit `start` so the deadline is exact
        // relative to slot 0 — anchoring it off a later `Instant::now()` would
        // pull the 2.0s slot just inside the window.
        let start = Instant::now();
        let l = RateLimiter {
            mode: LoadMode::ConstantRpm { target_rpm: 60.0 },
            start,
            next_slot: Mutex::new(0.0),
        };
        let deadline = start + Duration::from_secs(2);

        let started = Instant::now();
        let mut used = 0;
        for _ in 0..10 {
            if l.wait_for_slot_before(deadline).await {
                used += 1;
            }
        }
        let elapsed = started.elapsed();

        assert_eq!(
            used, 2,
            "only the slots at 0s and 1s fall before the deadline"
        );
        assert!(
            elapsed < Duration::from_secs(3),
            "abandoned slots must not be slept to; took {elapsed:?}"
        );
    }

    #[tokio::test]
    async fn constant_rpm_paces_consecutive_slots() {
        // 6000 rpm → 10ms spacing. Five slots should take ~40ms (4 gaps).
        let l = lim(LoadMode::ConstantRpm { target_rpm: 6000.0 });
        let t = Instant::now();
        for _ in 0..5 {
            l.wait_for_slot().await;
        }
        let elapsed = t.elapsed().as_secs_f64();
        assert!(elapsed >= 0.035, "expected ≥35ms pacing, got {elapsed}");
    }
}
