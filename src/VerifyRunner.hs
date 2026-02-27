{-|
Module      : VerifyRunner
Description : Response verification orchestration
Stability   : experimental

Runs verification checks comparing responses from primary and candidate targets.
-}
module VerifyRunner where

import Benchmark.Config (buildEndpoints)
import Benchmark.Network (addAuth, initNetwork, readToken, runComparison)
import Benchmark.Report (printVerifyReport)
import Benchmark.Types
  ( PerfTestError (..)
  , Settings (..)
  , TestConfig (..)
  , VerificationResult (..)
  , exitWithError
  )
import Benchmark.Verify qualified as Verify
import Control.Monad (forM)
import Data.Text qualified as T

runVerify :: TestConfig -> IO Bool
runVerify cfg = do
  putStrLn "Running Verification..."

  let epsA = buildEndpoints cfg False
      epsB = buildEndpoints cfg True
      setts = settings cfg

  token <- readToken (T.unpack $ secrets setts) >>= either exitWithError return
  mgr <- initNetwork setts

  let lenA = length epsA
      lenB = length epsB
  if lenA /= lenB
    then
      exitWithError $
        ConfigValidationError $
          "Endpoint list length mismatch: primary has "
            <> show lenA
            <> " endpoint(s), candidate has "
            <> show lenB
            <> " endpoint(s)"
    else return ()

  results <- forM (zip epsA epsB) $ \(epA, epB) -> do
    let authEpA = addAuth token epA
        authEpB = addAuth token epB
    (resA, resB) <- runComparison setts mgr authEpA authEpB
    let tol = maybe 0.0 id (floatTolerance setts)
        check = Verify.verify tol (compareFields setts) resA resB
    return (epA, check)

  printVerifyReport results
  return (all ((== Match) . snd) results)
