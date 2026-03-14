-- | Terminal output formatting for benchmark results and comparisons.
module Benchmark.Report
  ( printMultipleBenchmarkReport
  , printSingleBenchmarkReport
  , printValidationSummary
  , printBenchmarkReport
  , lookupStats
  )
where

import Benchmark.Report.Formatting (formatValidationError)
import Benchmark.Types
  ( BayesianComparison (..)
  , BenchmarkStats (..)
  , PercentileComparison (..)
  , ValidationSummary (..)
  )
import Control.Monad (forM_, unless, when)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

-- | Print a tabulated comparison of multiple benchmark results to stdout.
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
  printf "Probability Candidate Less Jittery:         %.2f%%\n" (probBLessJittery bayes * 100.0)
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

-- | Print a single target's benchmark statistics to stdout.
printSingleBenchmarkReport :: Text -> BenchmarkStats -> IO ()
printSingleBenchmarkReport name stats = do
  putStrLn ""

  printf "(%s):" (T.unpack name)
  printStats stats

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
    forM_ displayErrors (putStrLn . ("  " ++) . T.unpack . formatValidationError)
    when (length allErrors > 10) $
      printf "  ... and %d more unique error(s)\n" (length allErrors - 10)

-- | Print comparison report: ranking table followed by per-pair comparisons.
printBenchmarkReport :: Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> IO ()
printBenchmarkReport namedStats pairs = do
  putStrLn ""
  printHeader "Benchmark Report"

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

-- | Look up stats for a target by name, defaulting to zero stats if missing.
lookupStats :: Text -> Map Text BenchmarkStats -> BenchmarkStats
lookupStats = Map.findWithDefault emptyStats

emptyStats :: BenchmarkStats
emptyStats =
  BenchmarkStats
    { totalRequests = 0
    , countSuccess = 0
    , countFailure = 0
    , meanMs = 0
    , stdDevMs = 0
    , minMs = 0
    , maxMs = 0
    , p50Ms = 0
    , p95Ms = 0
    , p99Ms = 0
    , esMs = 0
    }
