-- | Rate limiting strategies: constant RPM, ramp-up, step-load, and Poisson modes.
module Benchmark.Execution.RateLimiter
  ( RateLimiter
  , waitForSlot
  , makeLimiter
  , currentTargetRpm
  )
where

import Benchmark.Types.Config (LoadMode (..), LoadStep (..), RampUpConfig (..))
import Control.Concurrent (MVar, modifyMVar, newMVar, threadDelay)
import Control.Monad (when)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import System.Random (randomRIO)

-- | Handle for controlling request rate across concurrent workers.
data RateLimiter = RateLimiter
  { rlNextSlot :: MVar UTCTime
  -- ^ The next available dispatch time
  , rlInterval :: UTCTime -> IO NominalDiffTime
  -- ^ Compute interval based on current time (for ramp/step schedules)
  , rlStartTime :: UTCTime
  -- ^ When the limiter was created (for elapsed-time calculations)
  , rlNominalRpm :: Maybe Double
  -- ^ Fixed nominal RPM for display (set for modes where interval is stochastic)
  }

{-| Atomically claim the next dispatch slot and sleep until it arrives.
The MVar holds the time of the next available slot. Each caller swaps
it to (slot + interval) and then sleeps until the claimed slot time.
-}
waitForSlot :: RateLimiter -> IO ()
waitForSlot RateLimiter {..} = do
  claimedSlot <- modifyMVar rlNextSlot $ \nextAvailable -> do
    interval <- rlInterval nextAvailable
    let following = addUTCTime interval nextAvailable
    pure (following, nextAvailable)
  now <- getCurrentTime
  let waitTime = diffUTCTime claimedSlot now
  when (waitTime > 0) $
    threadDelay (round (waitTime * 1_000_000))

-- | Create a rate limiter for the given load mode. Returns 'Nothing' for unthrottled.
makeLimiter :: LoadMode -> IO (Maybe RateLimiter)
makeLimiter LoadUnthrottled = pure Nothing
makeLimiter mode = do
  now <- getCurrentTime
  nextSlot <- newMVar now
  let poissonInterval rpm _ = do
        u <- randomRIO (1e-9, 1.0)
        pure (realToFrac (-(log u * 60.0 / rpm)) :: NominalDiffTime)
      constantInterval rpm _ = pure (realToFrac (60.0 / rpm) :: NominalDiffTime)
      rampInterval RampUpConfig {..} t =
        let elapsed = realToFrac (diffUTCTime t now) :: Double
            progress = min 1.0 (max 0.0 (elapsed / rampDurationSecs))
            rpm = max 6.0 (rampStartRpm + (rampEndRpm - rampStartRpm) * progress)
         in pure (realToFrac (60.0 / rpm) :: NominalDiffTime)
      stepInterval steps t =
        let elapsed = realToFrac (diffUTCTime t now) :: Double
            rpm = findStepRpm steps elapsed
         in pure (realToFrac (60.0 / rpm) :: NominalDiffTime)
      (intervalFn, nominalRpm) = case mode of
        LoadPoissonRpm rpm -> (poissonInterval rpm, Just rpm)
        LoadConstantRpm rpm -> (constantInterval rpm, Nothing)
        LoadRampUp cfg -> (rampInterval cfg, Nothing)
        LoadStepLoad steps -> (stepInterval steps, Nothing)
  pure $
    Just
      RateLimiter
        { rlNextSlot = nextSlot
        , rlInterval = intervalFn
        , rlStartTime = now
        , rlNominalRpm = nominalRpm
        }

-- | Find the RPM for the current elapsed time in a step schedule.
findStepRpm :: [LoadStep] -> Double -> Double
findStepRpm [] _ = 60.0
findStepRpm [s] _ = max 6.0 (loadStepRpm s)
findStepRpm (s : rest) elapsed
  | elapsed < loadStepDurationSecs s = max 6.0 (loadStepRpm s)
  | otherwise = findStepRpm rest (elapsed - loadStepDurationSecs s)

-- | Get the current target RPM based on elapsed time.
currentTargetRpm :: RateLimiter -> IO Double
currentTargetRpm RateLimiter {..} =
  case rlNominalRpm of
    Just rpm -> pure rpm
    Nothing -> do
      now <- getCurrentTime
      interval <- rlInterval now
      let secs = realToFrac interval :: Double
      pure (if secs > 0 then 60.0 / secs else 0)
