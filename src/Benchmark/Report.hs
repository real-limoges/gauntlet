{-|
Module      : Benchmark.Report
Description : Terminal output formatting for benchmark results
Stability   : experimental

Formats and prints benchmark statistics, Bayesian comparisons,
and verification results to the terminal.
-}
module Benchmark.Report
  ( printMultipleBenchmarkReport
  , printSingleBenchmarkReport
  , printVerifyReport
  , printValidationSummary
  , printNwayReport
  , lookupStats
  )
where

import Benchmark.Types
  ( ADResult (..)
  , BayesianComparison (..)
  , BenchmarkStats (..)
  , Endpoint (..)
  , JsonDiff (..)
  , KSResult (..)
  , MWUResult (..)
  , PercentileComparison (..)
  , ValidationError (..)
  , ValidationSummary (..)
  , VerificationResult (..)
  )
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

printMultipleBenchmarkReport ::
  Text -> Text -> BenchmarkStats -> BenchmarkStats -> BayesianComparison -> IO ()
printMultipleBenchmarkReport nameA nameB statsA statsB bayes = do
  putStrLn ""
  printHeader "Benchmark Report:"

  printf "(%s):" (T.unpack nameA)
  putStrLn ""
  printStats statsA

  putStrLn ""

  printf "(%s):" (T.unpack nameB)
  putStrLn ""
  printStats statsB

  putStrLn ""
  printHeader "Bayesian Analysis"

  printf "Probability Candidate is Faster (means):   %.2f%%\n" (probBFasterThanA bayes * 100.0)
  printf "Probability Single Request Faster:          %.2f%%\n" (probSingleRequestFaster bayes * 100.0)
  printf "Mean Difference: %.2f ms\n" (meanDifference bayes)
  printf
    "95%% Credible Interval: [%.2f ms, %.2f ms]\n"
    (credibleIntervalLower bayes)
    (credibleIntervalUpper bayes)
  printf "Effect Size (Cohen's d): %.3f\n" (effectSize bayes)
  printf "Relative Effect: %.2f%%\n" (relativeEffect bayes)

  putStrLn ""
  printHeader "Tail Analysis"

  let p95 = p95Comparison bayes
  printf
    "P95 Difference: %.2f ms [%.2f, %.2f]\n"
    (pctDifference p95)
    (pctCredibleLower p95)
    (pctCredibleUpper p95)
  printf "P95 Regression Probability: %.2f%%\n" (probPctRegression p95 * 100.0)

  let p99 = p99Comparison bayes
  printf
    "P99 Difference: %.2f ms [%.2f, %.2f]\n"
    (pctDifference p99)
    (pctCredibleLower p99)
    (pctCredibleUpper p99)
  printf "P99 Regression Probability: %.2f%%\n" (probPctRegression p99 * 100.0)

  putStrLn ""
  printHeader "Distribution Tests"
  printMWU (mannWhitneyU bayes)
  printKS (kolmogorovSmirnov bayes)
  printAD (andersonDarling bayes)

printSingleBenchmarkReport :: Text -> BenchmarkStats -> IO ()
printSingleBenchmarkReport name stats = do
  putStrLn ""

  printf "(%s):" (T.unpack name)
  printStats stats

printVerifyReport :: [(Endpoint, [VerificationResult])] -> IO ()
printVerifyReport results = do
  printHeader "Verification Report:"

  let total = length results
  let failures = filter (any (/= Match) . snd) results
  let successes = total - length failures

  putStrLn $ printf "Total Endpoints: %d" total
  printf "Passed:          %d\n" successes

  if null failures
    then printf "VERIFICATION TESTS PASSED.\n"
    else do
      printf "Failed:          %d\n" (length failures)
      putStrLn ""
      printHeader "FAILURE DETAILS"
      forM_ failures printMultiSampleFailure

{-| Print validation results. Silently no-ops when the list is empty
(i.e., no endpoints had a 'validate' block in their config).
-}
printValidationSummary :: [ValidationSummary] -> IO ()
printValidationSummary [] = return ()
printValidationSummary summaries = do
  putStrLn ""
  printHeader "Response Validation"

  let totalChecked = sum (map totalValidated summaries)
      totalFailed' = sum (map totalFailed summaries)
      totalPassed = totalChecked - totalFailed'

  printf "Checked:  %d responses\n" totalChecked
  printf "Passed:   %d\n" totalPassed

  when (totalFailed' > 0) $ do
    printf "Failed:   %d\n" totalFailed'
    putStrLn ""
    let allErrors = nub (concatMap validationErrors summaries)
        displayErrors = take 10 allErrors
    forM_ displayErrors printValidationError
    when (length allErrors > 10) $
      printf "  ... and %d more unique error(s)\n" (length allErrors - 10)

-- | Print N-way comparison report: ranking table followed by per-pair comparisons.
printNwayReport :: Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> IO ()
printNwayReport namedStats pairs = do
  putStrLn ""
  printHeader "N-Way Benchmark Report"

  -- Ranking table sorted by mean
  putStrLn ""
  printHeader "Ranking (by mean latency)"
  printf
    "%-4s  %-20s  %10s  %10s  %10s  %10s\n"
    ("#" :: String)
    ("Target" :: String)
    ("Mean" :: String)
    ("p50" :: String)
    ("p95" :: String)
    ("p99" :: String)
  printf
    "%-4s  %-20s  %10s  %10s  %10s  %10s\n"
    ("----" :: String)
    ("--------------------" :: String)
    ("----------" :: String)
    ("----------" :: String)
    ("----------" :: String)
    ("----------" :: String)
  let ranked = sortOn (meanMs . snd) (Map.toList namedStats)
  forM_ (zip [1 :: Int ..] ranked) $ \(rank, (name, stats)) ->
    printf
      "%-4d  %-20s  %8.2f ms  %8.2f ms  %8.2f ms  %8.2f ms\n"
      rank
      (T.unpack name)
      (meanMs stats)
      (p50Ms stats)
      (p95Ms stats)
      (p99Ms stats)

  -- Per-pair comparisons
  unless (null pairs) $ do
    putStrLn ""
    printHeader "Pairwise Comparisons"
    forM_ pairs $ \(nameA, nameB, bayes) -> do
      putStrLn ""
      printMultipleBenchmarkReport
        nameA
        nameB
        (lookupStats nameA namedStats)
        (lookupStats nameB namedStats)
        bayes

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

printStats :: BenchmarkStats -> IO ()
printStats stats = do
  printf "  Mean:    %.2f ms\n" (meanMs stats)
  printf "  StdDev:  %.2f ms\n" (stdDevMs stats)
  printf "  p50:     %.2f ms\n" (p50Ms stats)
  printf "  p95:     %.2f ms\n" (p95Ms stats)
  printf "  p99:     %.2f ms\n" (p99Ms stats)
  printf "  ES(p99): %.2f ms\n" (esMs stats)
  printf "  Min:     %.2f ms\n" (minMs stats)
  printf "  Max:     %.2f ms\n" (maxMs stats)
  printf "  Success: %d / %d\n" (countSuccess stats) (totalRequests stats)

printHeader :: String -> IO ()
printHeader h = putStrLn $ "#----- " ++ h ++ " -----#"

printMWU :: Maybe MWUResult -> IO ()
printMWU Nothing = putStrLn "Mann-Whitney U:      (sample too small)"
printMWU (Just r)
  | mwuSignificant r = putStrLn "Mann-Whitney U:      Significant — distributions differ (p < 0.05)"
  | otherwise = putStrLn "Mann-Whitney U:      Not significant (p >= 0.05)"

printKS :: Maybe KSResult -> IO ()
printKS Nothing = putStrLn "Kolmogorov-Smirnov:  (sample too small)"
printKS (Just r) =
  printf
    "Kolmogorov-Smirnov:  D = %.3f, p = %.3f (%s)\n"
    (ksStatistic r)
    (ksPValue r)
    (if ksSignificant r then "significant" else "not significant" :: String)

printAD :: Maybe ADResult -> IO ()
printAD Nothing = putStrLn "Anderson-Darling:    (sample too small)"
printAD (Just r) =
  printf
    "Anderson-Darling:    A² = %.3f, p ≈ %.3f (%s)\n"
    (adStatistic r)
    (adPValue r)
    (if adSignificant r then "significant" else "not significant" :: String)

printMultiSampleFailure :: (Endpoint, [VerificationResult]) -> IO ()
printMultiSampleFailure (ep, results) = do
  let n = length results
      passed = length (filter (== Match) results)
  printf "[%s] %s" (T.unpack $ method ep) (T.unpack $ url ep)
  if n > 1
    then printf " (%d/%d samples passed)\n" passed n
    else putStrLn ""
  case filter (/= Match) results of
    [] -> return ()
    (firstFailure : _) -> printVerificationResult firstFailure
  putStrLn ""

printVerificationResult :: VerificationResult -> IO ()
printVerificationResult Match = return ()
printVerificationResult (StatusMismatch a b) = printf "  Status Mismatch: Expected %d, Got %d\n" a b
printVerificationResult (InvalidJSON err) = printf "  JSON Error: %s\n" err
printVerificationResult (NetworkError msg) = printf "  Network Error: %s\n" msg
printVerificationResult (BodyMismatch diffs) = do
  printf "  Body Mismatch (%d field(s) differ):\n" (length diffs)
  mapM_
    ( \d ->
        printf
          "    %-40s  primary=%-20s  candidate=%s\n"
          (T.unpack $ jdPath d)
          (T.unpack $ jdPrimary d)
          (T.unpack $ jdCandidate d)
    )
    diffs

printValidationError :: ValidationError -> IO ()
printValidationError (StatusCodeMismatch expected actual) =
  printf "  [status]  expected %d, got %d\n" expected actual
printValidationError (FieldNotFound path) =
  printf "  [missing] %s\n" (T.unpack path)
printValidationError (FieldValueMismatch path expected actual) = do
  printf "  [value]   %s\n" (T.unpack path)
  printf "    expected: %s\n" (renderValue expected)
  printf "    actual:   %s\n" (renderValue actual)
printValidationError BodyAbsent =
  putStrLn "  [body]    response body absent"
printValidationError BodyInvalidJSON =
  putStrLn "  [body]    response body is not valid JSON"

renderValue :: Value -> String
renderValue = LBS8.unpack . encode

{-| Look up stats for a target by name.
INVARIANT: callers guarantee the key exists (constructed from the same target list).
-}
lookupStats :: Text -> Map Text BenchmarkStats -> BenchmarkStats
lookupStats name m = m Map.! name
