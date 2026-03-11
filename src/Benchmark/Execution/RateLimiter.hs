module Benchmark.Execution.RateLimiter
  ( RateLimiter
  , waitForSlot
  , makeLimiter
  , currentTargetRps
  )
where

import Benchmark.Types.Config (LoadMode (..), LoadStep (..))
import Control.Concurrent (MVar, modifyMVar, newMVar, threadDelay)
import Control.Monad (when)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import System.Random (randomRIO)


data RateLimiter = RateLimiter
  { rlNextSlot :: MVar UTCTime
  -- ^ The next available dispatch time
  , rlInterval :: UTCTime -> IO NominalDiffTime
  -- ^ Compute interval based on current time (for ramp/step schedules)
  , rlStartTime :: UTCTime
  -- ^ When the limiter was created (for elapsed-time calculations)
  , rlNominalRps :: Maybe Double
  -- ^ Fixed nominal RPS for display (set for modes where interval is stochastic)
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
  let (intervalFn, nominalRps) = case mode of
        LoadPoissonRps rps ->
          ( \_ -> do
              u <- randomRIO (1e-9, 1.0)
              let delay = -(log u / rps)
              pure (realToFrac delay)
          , Just rps
          )
        LoadConstantRps rps ->
          (\_ -> pure (realToFrac (1.0 / rps) :: NominalDiffTime), Nothing)
        LoadRampUp startRps endRps dur ->
          ( \t -> do
              let elapsed = realToFrac (diffUTCTime t now) :: Double
                  progress = min 1.0 (max 0.0 (elapsed / dur))
                  rps = max 0.1 (startRps + (endRps - startRps) * progress)
              pure (realToFrac (1.0 / rps) :: NominalDiffTime)
          , Nothing
          )
        LoadStepLoad steps ->
          ( \t -> do
              let elapsed = realToFrac (diffUTCTime t now) :: Double
                  rps = findStepRps steps elapsed
              pure (realToFrac (1.0 / rps) :: NominalDiffTime)
          , Nothing
          )
  pure $
    Just
      RateLimiter
        { rlNextSlot = nextSlot
        , rlInterval = intervalFn
        , rlStartTime = now
        , rlNominalRps = nominalRps
        }

-- | Find the RPS for the current elapsed time in a step schedule.
findStepRps :: [LoadStep] -> Double -> Double
findStepRps [] _ = 1.0
findStepRps [s] _ = max 0.1 (loadStepRps s)
findStepRps (s : rest) elapsed
  | elapsed < loadStepDurationSecs s = max 0.1 (loadStepRps s)
  | otherwise = findStepRps rest (elapsed - loadStepDurationSecs s)

-- | Get the current target RPS based on elapsed time.
currentTargetRps :: RateLimiter -> IO Double
currentTargetRps RateLimiter {..} =
  case rlNominalRps of
    Just rps -> pure rps
    Nothing -> do
      now <- getCurrentTime
      interval <- rlInterval now
      let secs = realToFrac interval :: Double
      pure (if secs > 0 then 1.0 / secs else 0)
