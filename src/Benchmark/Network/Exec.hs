{- |
Module      : Benchmark.Network.Exec
Description : Concurrent benchmark execution loops and A/B comparison
-}
module Benchmark.Network.Exec (
    runBenchmark,
    runBenchmarkWithEvents,
    runComparison,
)
where

import Benchmark.HTTP2 (timedRequestH2)
import Benchmark.Network.Pool (NetworkHandle (..))
import Benchmark.Network.Request (prepareRequest, timedRequestPrepared)
import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types (
    Endpoint (..),
    Settings (..),
    TestingResponse (..),
    defaultLogLevel,
 )
import Benchmark.Types qualified as Types
import Control.Concurrent (QSem)
import Control.Concurrent.Async (concurrently, replicateConcurrently)
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Log (Logger, logInfo, makeLogger)

{- | Run concurrent benchmark iterations with rate limiting via semaphore.
Prepares the request once (H1) or dispatches over the H2 connection.
-}
runBenchmark :: Settings -> QSem -> NetworkHandle -> Int -> Int -> Endpoint -> IO [TestingResponse]
runBenchmark settings sem network iters pIdx endpoint = do
    let logger = makeLogger (fromMaybe defaultLogLevel (Types.logLevel settings))
    countRef <- newIORef 0
    case network of
        H1 mgr -> do
            preparedReq <- prepareRequest settings endpoint
            replicateConcurrently iters $
                bracket_ (waitQSem sem) (signalQSem sem) $ do
                    res <- timedRequestPrepared settings mgr preparedReq
                    current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
                    printProgressBar logger pIdx current iters
                    return res
        H2 conn ->
            replicateConcurrently iters $
                bracket_ (waitQSem sem) (signalQSem sem) $ do
                    res <- timedRequestH2 conn endpoint
                    current <- atomicModifyIORef' countRef (\n -> (n + 1, n + 1))
                    printProgressBar logger pIdx current iters
                    return res

{- | Run benchmark with event emission for TUI updates.
Emits 'RequestCompleted' or 'RequestFailed' after each request.
-}
runBenchmarkWithEvents :: Settings -> QSem -> NetworkHandle -> Int -> Int -> Endpoint -> TChan BenchmarkEvent -> IO [TestingResponse]
runBenchmarkWithEvents settings sem network iters pIdx endpoint eventChan = do
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
    case network of
        H1 mgr -> do
            preparedReq <- prepareRequest settings endpoint
            replicateConcurrently iters $
                bracket_ (waitQSem sem) (signalQSem sem) $
                    timedRequestPrepared settings mgr preparedReq >>= emitAndProgress
        H2 conn ->
            replicateConcurrently iters $
                bracket_ (waitQSem sem) (signalQSem sem) $
                    timedRequestH2 conn endpoint >>= emitAndProgress

-- | Execute two endpoints concurrently for A/B comparison.
runComparison :: Settings -> NetworkHandle -> Endpoint -> Endpoint -> IO (TestingResponse, TestingResponse)
runComparison settings (H1 mgr) epA epB = do
    reqA <- prepareRequest settings epA
    reqB <- prepareRequest settings epB
    concurrently (timedRequestPrepared settings mgr reqA) (timedRequestPrepared settings mgr reqB)
runComparison _settings (H2 conn) epA epB =
    concurrently (timedRequestH2 conn epA) (timedRequestH2 conn epB)

printProgressBar :: Logger -> Int -> Int -> Int -> IO ()
printProgressBar logger idx c total = do
    let numUpdates = min total 10
        step = max 1 (total `div` numUpdates)
        shouldPrint = c > 0 && (c `mod` step == 0 || c == total)
    when shouldPrint $ do
        let percent = (c * 100) `div` total
        logInfo logger $ T.pack $ "[Ep " ++ show idx ++ "] Progress: " ++ show percent ++ "% (" ++ show c ++ "/" ++ show total ++ ")"
