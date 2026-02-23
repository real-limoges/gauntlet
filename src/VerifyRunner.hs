{- |
Module      : VerifyRunner
Description : Response verification orchestration
Stability   : experimental

Runs verification checks comparing responses from primary and candidate targets.
-}
module VerifyRunner where

import Benchmark.Config (buildEndpoints)
import Benchmark.Network (addAuth, initNetwork, readToken, runComparison)
import Benchmark.Report (printVerifyReport)
import Benchmark.Types (Settings (..), TestConfig (..), exitWithError)
import Benchmark.Verify qualified as Verify
import Control.Monad (forM)
import Data.Text qualified as T

runVerify :: TestConfig -> IO ()
runVerify cfg = do
    putStrLn "Running Verification..."

    let epsA = buildEndpoints cfg False
        epsB = buildEndpoints cfg True
        setts = settings cfg

    token <- readToken (T.unpack $ secrets setts) >>= either exitWithError return
    mgr <- initNetwork setts

    results <- forM (zip epsA epsB) $ \(epA, epB) -> do
        let authEpA = addAuth token epA
            authEpB = addAuth token epB
        (resA, resB) <- runComparison setts mgr authEpA authEpB
        let check = Verify.verify resA resB
        return (epA, check)

    printVerifyReport results
