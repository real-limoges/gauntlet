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
import Control.Monad (forM, replicateM)
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

  let n = maybe 1 id (verifyIterations setts)

  results <- forM (zip epsA epsB) $ \(epA, epB) -> do
    let authEpA = addAuth token epA
        authEpB = addAuth token epB
        tol = maybe 0.0 id (floatTolerance setts)
    checks <- replicateM n $ do
      (resA, resB) <- runComparison setts mgr authEpA authEpB
      return $ Verify.verify tol (compareFields setts) resA resB
    return (epA, checks)

  printVerifyReport results
  return (all (all (== Match) . snd) results)
