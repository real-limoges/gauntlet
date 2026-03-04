{-|
Module      : Benchmark.Network.Exec
Description : Concurrent benchmark execution loops and A/B comparison
-}
module Benchmark.Network.Exec
  ( runBenchmark
  , runBenchmarkWithEvents
  , runBenchmarkDuration
  , runBenchmarkDurationWithEvents
  )
where

import Benchmark.Network.Request (prepareRequest, timedRequestPrepared)
import Benchmark.RateLimiter (RateLimiter, waitForSlot)
import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types
  ( Endpoint (..)
  , Settings (..)
  , TestingResponse (..)
  , defaultLogLevel
  )
import Benchmark.Types qualified as Types
import Control.Concurrent (QSem)
import Control.Concurrent.Async (replicateConcurrently)
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Log (Logger, logInfo, makeLogger)
import Network.HTTP.Client (Manager, Request)

{-| Run concurrent benchmark iterations with rate limiting via semaphore.
Prepares the request once (H1) or dispatches over the H2 connection.
When a 'RateLimiter' is provided, each thread waits for a rate slot before
acquiring the semaphore.
-}
runBenchmark ::
  Settings -> QSem -> Manager -> Int -> Int -> Endpoint -> Maybe RateLimiter -> IO [TestingResponse]
runBenchmark settings sem mgr iters pIdx endpoint mLimiter = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
  countRef <- newIORef 0
  preparedReq <- prepareRequest settings endpoint
  replicateConcurrently iters $ do
    mapM_ waitForSlot mLimiter
    bracket_ (waitQSem sem) (signalQSem sem) $ do
      res <- timedRequestPrepared settings mgr preparedReq
      current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
      printProgressBar logger pIdx current iters
      return res

{-| Run benchmark with event emission for TUI updates.
Emits 'RequestCompleted' or 'RequestFailed' after each request.
-}
runBenchmarkWithEvents ::
  Settings ->
  QSem ->
  Manager ->
  Int ->
  Int ->
  Endpoint ->
  TChan BenchmarkEvent ->
  Maybe RateLimiter ->
  IO [TestingResponse]
runBenchmarkWithEvents settings sem mgr iters pIdx endpoint eventChan mLimiter = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
  countRef <- newIORef 0
  let emitAndProgress res = do
        let event = case errorMessage res of
              Nothing -> RequestCompleted (durationNs res) (statusCode res)
              Just err -> RequestFailed (T.pack err)
        atomically $ writeTChan eventChan event
        current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
        printProgressBar logger pIdx current iters
        return res
  preparedReq <- prepareRequest settings endpoint
  replicateConcurrently iters $ do
    mapM_ waitForSlot mLimiter
    bracket_ (waitQSem sem) (signalQSem sem) $
      timedRequestPrepared settings mgr preparedReq >>= emitAndProgress

{-| Run benchmark for a fixed wall-clock duration (for ramp-up and step-load modes).
Spawns @concurrency@ worker threads that loop until the deadline, each calling
'waitForSlot' before executing a request.
-}
runBenchmarkDuration ::
  Settings ->
  QSem ->
  Manager ->
  NominalDiffTime ->
  Int ->
  Endpoint ->
  RateLimiter ->
  IO [TestingResponse]
runBenchmarkDuration settings sem mgr duration pIdx endpoint limiter = do
  let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
  now <- getCurrentTime
  let deadline = addUTCTime duration now
      conc = concurrency settings
  countRef <- newIORef 0
  preparedReq <- prepareRequest settings endpoint
  resultRefs <-
    replicateConcurrently conc $ workerLoop preparedReq deadline countRef logger pIdx sem mgr settings limiter
  pure (concat resultRefs)

-- | Duration-based benchmark with TUI event emission.
runBenchmarkDurationWithEvents ::
  Settings ->
  QSem ->
  Manager ->
  NominalDiffTime ->
  Int ->
  Endpoint ->
  RateLimiter ->
  TChan BenchmarkEvent ->
  IO [TestingResponse]
runBenchmarkDurationWithEvents settings sem mgr duration pIdx endpoint limiter eventChan = do
  now <- getCurrentTime
  let deadline = addUTCTime duration now
      conc = concurrency settings
  preparedReq <- prepareRequest settings endpoint
  resultRefs <-
    replicateConcurrently conc $ workerLoopWithEvents preparedReq deadline sem mgr settings limiter eventChan
  pure (concat resultRefs)

-- | Worker loop that runs requests until the deadline.
workerLoop ::
  Request ->
  UTCTime ->
  IORef Int ->
  Logger ->
  Int ->
  QSem ->
  Manager ->
  Settings ->
  RateLimiter ->
  IO [TestingResponse]
workerLoop preparedReq deadline countRef logger pIdx sem mgr settings limiter = go []
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
                bracket_ (waitQSem sem) (signalQSem sem) $
                  timedRequestPrepared settings mgr preparedReq
              current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
              when (current `mod` 100 == 0) $
                logInfo logger $
                  T.pack $
                    "[Ep " ++ show pIdx ++ "] Completed " ++ show current ++ " requests"
              go (res : acc)

-- | Worker loop with TUI event emission.
workerLoopWithEvents ::
  Request ->
  UTCTime ->
  QSem ->
  Manager ->
  Settings ->
  RateLimiter ->
  TChan BenchmarkEvent ->
  IO [TestingResponse]
workerLoopWithEvents preparedReq deadline sem mgr settings limiter eventChan = go []
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
                bracket_ (waitQSem sem) (signalQSem sem) $
                  timedRequestPrepared settings mgr preparedReq
              let event = case errorMessage res of
                    Nothing -> RequestCompleted (durationNs res) (statusCode res)
                    Just err -> RequestFailed (T.pack err)
              atomically $ writeTChan eventChan event
              go (res : acc)

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
