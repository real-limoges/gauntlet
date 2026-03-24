-- | HTTP request execution with retry logic and connection pooling.
module Benchmark.Network.Exec
  ( BenchmarkEnv (..)
  , runBenchmark
  , runBenchmarkDuration
  )
where

import Benchmark.Execution.RateLimiter (RateLimiter, waitForSlot)
import Benchmark.Network.Request (prepareRequest, timedRequestPrepared)
import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types
  ( Endpoint (..)
  , Settings (..)
  , TestingResponse (..)
  )
import Control.Concurrent (QSem)
import Control.Concurrent.Async (replicateConcurrently)
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Concurrent.STM (TBQueue, atomically, writeTBQueue)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Log (Logger, logInfo)
import Network.HTTP.Client (Manager, Request)

{-| Shared context for benchmark execution, bundling the common parameters
needed by 'runBenchmark' and 'runBenchmarkDuration'.
-}
data BenchmarkEnv = BenchmarkEnv
  { beSettings :: Settings
  -- ^ Benchmark configuration (concurrency, timeouts, retry, etc.)
  , beSem :: QSem
  -- ^ Semaphore controlling concurrent request limit
  , beManager :: Manager
  -- ^ HTTP connection manager (shared across all requests)
  , bePayloadIndex :: Int
  -- ^ Index of the current payload\/endpoint (for progress logging)
  , beEventChan :: Maybe (TBQueue BenchmarkEvent)
  -- ^ Optional TUI event channel for real-time feedback
  , beLogger :: Logger
  -- ^ Logger for progress output
  }

-- | Frequency (in completed requests) at which duration-mode workers log progress.
logFrequency :: Int
logFrequency = 100

{-| Run concurrent benchmark iterations with rate limiting via semaphore.
When a 'RateLimiter' is provided, each thread waits for a rate slot before
acquiring the semaphore.  Optionally emits TUI events when a channel is provided.
-}
runBenchmark ::
  BenchmarkEnv ->
  Int ->
  Endpoint ->
  Maybe RateLimiter ->
  IO [TestingResponse]
runBenchmark BenchmarkEnv {..} iters endpoint mLimiter = do
  countRef <- newIORef 0
  preparedReq <- prepareRequest beSettings endpoint
  replicateConcurrently iters $ do
    mapM_ waitForSlot mLimiter
    bracket_ (waitQSem beSem) (signalQSem beSem) $ do
      res <- timedRequestPrepared beSettings beManager preparedReq
      emitEvent beEventChan res
      current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
      printProgressBar beLogger bePayloadIndex current iters
      return res

{-| Run benchmark for a fixed wall-clock duration (for ramp-up and step-load modes).
Spawns @concurrency@ worker threads that loop until the deadline, each calling
'waitForSlot' before executing a request.  Optionally emits TUI events.
-}
runBenchmarkDuration ::
  BenchmarkEnv ->
  NominalDiffTime ->
  Endpoint ->
  RateLimiter ->
  IO [TestingResponse]
runBenchmarkDuration BenchmarkEnv {..} duration endpoint limiter = do
  now <- getCurrentTime
  let deadline = addUTCTime duration now
      conc = concurrency beSettings
  countRef <- newIORef 0
  preparedReq <- prepareRequest beSettings endpoint
  resultRefs <-
    replicateConcurrently conc $
      workerLoop BenchmarkEnv {..} preparedReq deadline countRef limiter
  pure (concat resultRefs)

-- | Worker loop that runs requests until the deadline.
workerLoop ::
  BenchmarkEnv ->
  Request ->
  UTCTime ->
  IORef Int ->
  RateLimiter ->
  IO [TestingResponse]
workerLoop BenchmarkEnv {..} preparedReq deadline countRef limiter = go []
  where
    go acc = do
      now <- getCurrentTime
      if now >= deadline
        then pure (reverse acc)
        else do
          waitForSlot limiter
          nowAfterSlot <- getCurrentTime
          if nowAfterSlot >= deadline
            then pure (reverse acc)
            else do
              res <-
                bracket_ (waitQSem beSem) (signalQSem beSem) $
                  timedRequestPrepared beSettings beManager preparedReq
              emitEvent beEventChan res
              current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
              when (current `mod` logFrequency == 0) $
                logInfo beLogger $
                  T.pack $
                    "[Ep " ++ show bePayloadIndex ++ "] Completed " ++ show current ++ " requests"
              go (res : acc)

-- | Emit a TUI event for a completed request when a channel is present.
emitEvent :: Maybe (TBQueue BenchmarkEvent) -> TestingResponse -> IO ()
emitEvent Nothing _ = pure ()
emitEvent (Just chan) res = do
  let event = case errorMessage res of
        Nothing -> RequestCompleted (durationNs res) (statusCode res)
        Just err -> RequestFailed (T.pack err)
  atomically $ writeTBQueue chan event

printProgressBar :: Logger -> Int -> Int -> Int -> IO ()
printProgressBar logger idx c total = do
  let numUpdates = min total 10
      step = max 1 (total `div` numUpdates)
      shouldPrint = c > 0 && (c `mod` step == 0 || c == total)
  when shouldPrint $ do
    let percent = (c * 100) `div` total
    logInfo logger $
      T.pack $
        "[Ep " ++ show idx ++ "] Progress: " ++ show percent ++ "% (" ++ show c ++ "/" ++ show total ++ ")"
