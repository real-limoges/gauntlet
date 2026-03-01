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
import Benchmark.Report.Markdown (markdownVerifyReport)
import Benchmark.Types
  ( OutputFormat (..)
  , PerfTestError (..)
  , Settings (..)
  , Targets (..)
  , TestConfig (..)
  , VerificationResult (..)
  , defaultLogLevel
  , exitWithError
  )
import Benchmark.Verify qualified as Verify
import Control.Monad (forM, replicateM, when)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Log (logInfo, makeLogger)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

runVerify :: OutputFormat -> TestConfig -> IO Bool
runVerify fmt cfg = do
  let setts = settings cfg
      logger = makeLogger (fromMaybe defaultLogLevel (logLevel setts))
  logInfo logger "Running Verification..."

  let epsA = buildEndpoints (primary (targets cfg)) (payloads cfg)
      epsB = buildEndpoints (candidate (targets cfg)) (payloads cfg)

  token <- readToken (T.unpack $ secrets setts) >>= either exitWithError return
  mgr <- initNetwork setts

  let lenA = length epsA
      lenB = length epsB
  when (lenA /= lenB) $
    exitWithError $
      ConfigValidationError $
        "Endpoint list length mismatch: primary has "
          <> show lenA
          <> " endpoint(s), candidate has "
          <> show lenB
          <> " endpoint(s)"

  let n = fromMaybe 1 (verifyIterations setts)

  results <- forM (zip epsA epsB) $ \(epA, epB) -> do
    let authEpA = addAuth token epA
        authEpB = addAuth token epB
        tol = fromMaybe 0.0 (floatTolerance setts)
    checks <- replicateM n $ do
      (resA, resB) <- runComparison setts mgr authEpA authEpB
      return $ Verify.verifyWithNetworkCheck tol (compareFields setts) (ignoreFields setts) resA resB
    return (epA, checks)

  printVerifyReport results

  case fmt of
    OutputTerminal -> return ()
    OutputMarkdown path -> do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path (markdownVerifyReport results)
      logInfo logger (T.pack ("Markdown report written to: " <> path))

  return (all (all (== Match) . snd) results)
