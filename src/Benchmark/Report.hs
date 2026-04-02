-- | Terminal output formatting for benchmark results and comparisons.
module Benchmark.Report
  ( printMultipleBenchmarkReport
  , printSingleBenchmarkReport
  , printValidationSummary
  , printBenchmarkReport
  , lookupStats
  , printRegressionResult
  )
where

import Benchmark.Report.Formatting (formatValidationError)
import Benchmark.Types
  ( BayesianComparison (..)
  , BenchmarkStats (..)
  , ComparisonReport (..)
  , MetricRegression (..)
  , PercentileComparison (..)
  , RegressionResult (..)
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
printMultipleBenchmarkReport :: ComparisonReport -> IO ()
printMultipleBenchmarkReport ComparisonReport {..} = do
  putStrLn ""
  printHeader "Benchmark Report:"

  putStrLn $ bold $ "(" <> T.unpack crNameA <> "):"
  printStats crStatsA

  putStrLn ""

  putStrLn $ bold $ "(" <> T.unpack crNameB <> "):"
  printStats crStatsB

  putStrLn ""
  printHeader "Bayesian Analysis"

  let pct p = colorPct p (printf "%.2f%%" p :: String)
  printf
    "P(%s faster than %s, means): %s\n"
    (T.unpack crNameB)
    (T.unpack crNameA)
    (pct $ probBFasterThanA crBayes * 100.0)
  printf "P(%s faster, single request): %s\n" (T.unpack crNameB) (pct $ probSingleRequestFaster crBayes * 100.0)
  printf "P(%s less jittery): %s\n" (T.unpack crNameB) (pct $ probBLessJittery crBayes * 100.0)
  printf "Mean Difference (%s - %s): %.2f ms\n" (T.unpack crNameB) (T.unpack crNameA) (meanDifference crBayes)
  printf
    "95%% Credible Interval: [%.2f ms, %.2f ms]\n"
    (credibleIntervalLower crBayes)
    (credibleIntervalUpper crBayes)
  printf "Effect Size (Cohen's d): %.3f\n" (effectSize crBayes)
  printf "Relative Effect: %.2f%%\n" (relativeEffect crBayes)

  putStrLn ""
  printHeader "Tail Analysis"

  let p95 = p95Comparison crBayes
  printf
    "P95 Difference: %.2f ms [%.2f, %.2f]\n"
    (pctDifference p95)
    (pctCredibleLower p95)
    (pctCredibleUpper p95)
  printf "P95 Regression Probability: %s\n" (pct $ probPctRegression p95 * 100.0)

  let p99 = p99Comparison crBayes
  printf
    "P99 Difference: %.2f ms [%.2f, %.2f]\n"
    (pctDifference p99)
    (pctCredibleLower p99)
    (pctCredibleUpper p99)
  printf "P99 Regression Probability: %s\n" (pct $ probPctRegression p99 * 100.0)

  case emd crBayes of
    Just d -> do
      putStrLn ""
      printHeader "Distribution"
      printf "Earth Mover's Distance: %.3f ms\n" d
    Nothing -> pure ()

-- | Print a single target's benchmark statistics to stdout.
printSingleBenchmarkReport :: Text -> BenchmarkStats -> IO ()
printSingleBenchmarkReport name stats = do
  putStrLn ""
  putStrLn $ bold $ "(" <> T.unpack name <> "):"
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
    forM_ displayErrors $ \err -> do
      let formatted = T.unpack (formatValidationError err)
      putStrLn ("  " ++ formatted)
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
        ComparisonReport
          { crNameA = nameA
          , crNameB = nameB
          , crStatsA = lookupStats nameA namedStats
          , crStatsB = lookupStats nameB namedStats
          , crBayes = bayes
          }

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
  unless (null (histogram stats)) $ do
    putStrLn ""
    printHistogram (histogram stats)

-- | Render a latency distribution as an ASCII horizontal bar chart.
printHistogram :: [(Double, Int)] -> IO ()
printHistogram bins = do
  putStrLn "  Distribution:"
  let maxCount = maximum (map snd bins)
      maxBarWidth = 40 :: Int
      scale c
        | maxCount <= 0 = 0
        | otherwise = round (fromIntegral c / fromIntegral maxCount * fromIntegral maxBarWidth :: Double)
  forM_ bins $ \(lo, count) ->
    when (count > 0) $
      let barLen = scale count :: Int
          bar = replicate barLen '█'
       in printf "  %7.1f ms  %s %d\n" lo bar count

printHeader :: String -> IO ()
printHeader h = putStrLn $ "\ESC[1;36m── " <> h <> " ──\ESC[0m"

-- ---------------------------------------------------------------------------
-- ANSI formatting helpers
-- ---------------------------------------------------------------------------

bold :: String -> String
bold s = "\ESC[1m" <> s <> "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" <> s <> "\ESC[0m"

yellow :: String -> String
yellow s = "\ESC[33m" <> s <> "\ESC[0m"

blue :: String -> String
blue s = "\ESC[34m" <> s <> "\ESC[0m"

-- | Color a percentage string: green ≥ 90%, yellow ≥ 60%, blue otherwise.
colorPct :: Double -> String -> String
colorPct p s
  | p >= 90.0 = green s
  | p >= 60.0 = yellow s
  | otherwise = blue s

-- | Look up stats for a target by name, defaulting to zero stats if missing.
lookupStats :: Text -> Map Text BenchmarkStats -> BenchmarkStats
lookupStats = Map.findWithDefault emptyStats

-- | Zero-valued stats used as fallback when a target name is not found in the results map.
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
    , histogram = []
    }

-- | Print a regression result summary to stdout.
printRegressionResult :: RegressionResult -> IO ()
printRegressionResult regression = do
  putStrLn ""
  printHeader $ "Regression Check vs '" <> T.unpack (regressionBaseline regression) <> "'"
  putStrLn ""
  printf
    "%-8s %12s %12s %10s %10s %s\n"
    ("Metric" :: String)
    ("Baseline" :: String)
    ("Current" :: String)
    ("Change" :: String)
    ("Threshold" :: String)
    ("Status" :: String)
  putStrLn $ replicate 70 '-'
  mapM_ printMetric (regressionMetrics regression)
  putStrLn ""
  if regressionPassed regression
    then putStrLn $ green "Result: PASSED (no regressions detected)"
    else putStrLn $ blue "Result: FAILED (regression detected)"

printMetric :: MetricRegression -> IO ()
printMetric m =
  printf
    "%-8s %10.2f ms %10.2f ms %+9.1f%% %9.0f%% %s\n"
    (T.unpack $ metricName m)
    (metricBaseline m)
    (metricCurrent m)
    (metricChange m * 100)
    (metricThreshold m * 100)
    (if metricRegressed m then blue "REGRESSED" else green "ok" :: String)
