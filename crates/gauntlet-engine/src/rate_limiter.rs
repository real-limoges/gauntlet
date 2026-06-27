//! Request pacing for every load mode, via atomic slot reservation.
//!
//! One mechanism covers all modes (the Haskell `MVar`-based limiter), rather than
//! reaching for `governor` (constant-rate only). A shared `next_slot` clock is
//! advanced atomically per request; the caller sleeps until its claimed slot. The
//! interval is recomputed at each claim, so time-varying modes (ramp/step) and
//! stochastic ones (Poisson) fall out naturally.
//!
//! This composes with the concurrency `Semaphore` without double-counting: the
//! limiter paces request *starts*, the semaphore caps *in-flight* requests.

use std::time::{Duration, Instant};

use rand::Rng;
use tokio::sync::Mutex;

use gauntlet_core::{LoadMode, LoadStep};

/// Minimum RPM for time-varying modes, mirroring the Haskell `max 6.0` floor.
const MIN_RPM: f64 = 6.0;

/// Paces request dispatch for a single endpoint run. `Unthrottled` has no limiter
/// (`new` returns `None`); all other modes reserve slots off a shared clock.
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
        let claimed = {
            let mut next = self.next_slot.lock().await;
            let claimed = *next;
            *next = claimed + self.interval_at(claimed);
            claimed
        };
        let target = self.start + Duration::from_secs_f64(claimed.max(0.0));
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
    let mut cum = 0.0;
    for step in steps {
        cum += step.duration_secs;
        if t < cum {
            return step.rpm;
        }
    }
    steps.last().map(|s| s.rpm).unwrap_or(MIN_RPM)
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
