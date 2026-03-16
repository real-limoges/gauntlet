-- | Markdown report generation for benchmark results and regressions.
module Benchmark.Report.Markdown
  ( markdownSingleReport
  , markdownMultipleReport
  , markdownBenchmarkReport
  , markdownRegressionReport
  , markdownValidationReport
  ) where

import Benchmark.Report (lookupStats)
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
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

-- | Markdown report for a single-endpoint benchmark run.
markdownSingleReport :: Text -> BenchmarkStats -> Text
markdownSingleReport label stats =
  T.unlines $
    [ "## Benchmark Report: " <> label
    , ""
    , "### Statistics"
    , ""
    ]
      ++ statsTable stats

-- | Markdown report for an A/B benchmark with Bayesian comparison.
markdownMultipleReport :: ComparisonReport -> Text
markdownMultipleReport ComparisonReport {..} =
  T.unlines $
    [ "## Benchmark Report: " <> crNameB <> " vs " <> crNameA
    , ""
    , "### Statistics"
    , ""
    , "#### " <> crNameA
    , ""
    ]
      ++ statsTable crStatsA
      ++ ["", "#### " <> crNameB, ""]
      ++ statsTable crStatsB
      ++ ["", "### Bayesian Analysis", ""]
      ++ bayesTable crBayes

-- | Markdown report for a multi-target comparison with ranking and per-pair detail.
markdownBenchmarkReport :: Map Text BenchmarkStats -> [(Text, Text, BayesianComparison)] -> Text
markdownBenchmarkReport namedStats pairs =
  T.unlines $
    [ "## Benchmark Report"
    , ""
    , "### Ranking (by mean latency)"
    , ""
    , "| # | Target | Mean | p50 | p95 | p99 |"
    , "|---|--------|------|-----|-----|-----|"
    ]
      ++ zipWith rankRow [1 :: Int ..] ranked
      ++ concatMap pairSection pairs
  where
    ranked = sortOn (meanMs . snd) (Map.toList namedStats)

    rankRow i (name, s) =
      T.pack $
        printf
          "| %d | %s | %.2f ms | %.2f ms | %.2f ms | %.2f ms |"
          i
          (T.unpack name)
          (meanMs s)
          (p50Ms s)
          (p95Ms s)
          (p99Ms s)

    pairSection (nameA, nameB, bayes) =
      let statsA = lookupStats nameA namedStats
          statsB = lookupStats nameB namedStats
       in ["", "---", ""]
            ++ T.lines
              ( markdownMultipleReport
                  ComparisonReport {crNameA = nameA, crNameB = nameB, crStatsA = statsA, crStatsB = statsB, crBayes = bayes}
              )

-- | Markdown report for a regression comparison against a saved baseline.
markdownRegressionReport :: RegressionResult -> Text
markdownRegressionReport result =
  T.unlines $
    [ "## Regression Check vs `" <> regressionBaseline result <> "`"
    , ""
    , statusBadge
    , ""
    , "### Metrics"
    , ""
    , "| Metric | Baseline | Current | Change | Threshold | Status |"
    , "|--------|----------|---------|--------|-----------|--------|"
    ]
      ++ map formatRow (regressionMetrics result)
      ++ [""]
      ++ summary
  where
    statusBadge
      | regressionPassed result = "**Status:** PASSED"
      | otherwise = "**Status:** FAILED — Regression Detected"

    formatRow m =
      T.pack $
        printf
          "| %s | %.2f ms | %.2f ms | %+.1f%% | %.0f%% | %s |"
          (T.unpack $ metricName m)
          (metricBaseline m)
          (metricCurrent m)
          (metricChange m * 100)
          (metricThreshold m * 100)
          (if metricRegressed m then "FAIL" else "PASS" :: String)

    summary
      | regressionPassed result = ["All metrics within acceptable thresholds."]
      | otherwise =
          [ "**Action Required:** Performance regression detected."
          , ""
          , "Regressed metrics:"
          ]
            ++ map formatRegression (filter metricRegressed $ regressionMetrics result)

    formatRegression m =
      T.pack $
        printf
          "- **%s**: increased by %.1f%% (threshold: %.0f%%)"
          (T.unpack $ metricName m)
          (metricChange m * 100)
          (metricThreshold m * 100)

-- | Markdown report for validation summaries.
markdownValidationReport :: [ValidationSummary] -> Text
markdownValidationReport [] = T.empty
markdownValidationReport summaries =
  T.unlines $
    [ "## Validation Results"
    , ""
    , "| Total Validated | Passed | Failed |"
    , "|-----------------|--------|--------|"
    ]
      ++ map formatRow summaries
      ++ concatMap formatErrors (filter (not . null . validationErrors) summaries)
  where
    formatRow s =
      T.pack $
        printf
          "| %d | %d | %d |"
          (totalValidated s)
          (totalValidated s - totalFailed s)
          (totalFailed s)

    formatErrors s =
      ["", "**Validation Errors:**"]
        ++ map (("- " <>) . formatValidationError) (validationErrors s)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

statsTable :: BenchmarkStats -> [Text]
statsTable s =
  [ "| Metric | Value |"
  , "|--------|-------|"
  , row "Total Requests" (show $ totalRequests s)
  , row "Success" (show $ countSuccess s)
  , row "Failure" (show $ countFailure s)
  , row "Mean" (printf "%.2f ms" (meanMs s))
  , row "Std Dev" (printf "%.2f ms" (stdDevMs s))
  , row "Min" (printf "%.2f ms" (minMs s))
  , row "Max" (printf "%.2f ms" (maxMs s))
  , row "p50" (printf "%.2f ms" (p50Ms s))
  , row "p95" (printf "%.2f ms" (p95Ms s))
  , row "p99" (printf "%.2f ms" (p99Ms s))
  , row "ES(p99)" (printf "%.2f ms" (esMs s))
  ]
  where
    row label val = T.pack $ printf "| %s | %s |" (label :: String) (val :: String)

bayesTable :: BayesianComparison -> [Text]
bayesTable b =
  [ "| Metric | Value |"
  , "|--------|-------|"
  , row "P(candidate faster, means)" (printf "%.1f%%" (probBFasterThanA b * 100))
  , row "P(single request faster)" (printf "%.1f%%" (probSingleRequestFaster b * 100))
  , row "P(candidate less jittery)" (printf "%.1f%%" (probBLessJittery b * 100))
  , row "Mean difference" (printf "%+.2f ms" (meanDifference b))
  , row "95% credible interval" credInterval
  , row "Effect size (Cohen's d)" (printf "%.3f" (effectSize b))
  , row "Relative effect" (printf "%+.1f%%" (relativeEffect b * 100))
  , row "p95 difference" (pctRow $ p95Comparison b)
  , row "p99 difference" (pctRow $ p99Comparison b)
  ]
  where
    row label val = T.pack $ printf "| %s | %s |" (label :: String) (val :: String)
    credInterval =
      printf
        "[%+.2f ms, %+.2f ms]"
        (credibleIntervalLower b)
        (credibleIntervalUpper b)
    pctRow p =
      printf
        "%+.2f ms [%+.2f, %+.2f]"
        (pctDifference p)
        (pctCredibleLower p)
        (pctCredibleUpper p)
