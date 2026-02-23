{- |
Module      : Benchmark.Report.Markdown
Description : Pure markdown report generators for benchmark results
Stability   : experimental

Produces self-contained markdown sections for stats, Bayesian analysis,
regression comparisons, and validation summaries.
-}
module Benchmark.Report.Markdown (
    markdownSingleReport,
    markdownMultipleReport,
    markdownRegressionReport,
    markdownValidationReport,
) where

import Benchmark.Types (
    BayesianComparison (..),
    BenchmarkStats (..),
    KSResult (..),
    MWUResult (..),
    MetricRegression (..),
    PercentileComparison (..),
    RegressionResult (..),
    ValidationError (..),
    ValidationSummary (..),
 )
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
markdownMultipleReport ::
    -- | Primary label
    Text ->
    -- | Candidate label
    Text ->
    BenchmarkStats ->
    BenchmarkStats ->
    BayesianComparison ->
    Text
markdownMultipleReport primaryLabel candidateLabel primary candidate bayes =
    T.unlines $
        [ "## Benchmark Report: " <> candidateLabel <> " vs " <> primaryLabel
        , ""
        , "### Statistics"
        , ""
        , "#### " <> primaryLabel
        , ""
        ]
            ++ statsTable primary
            ++ ["", "#### " <> candidateLabel, ""]
            ++ statsTable candidate
            ++ ["", "### Bayesian Analysis", ""]
            ++ bayesTable bayes

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
            ++ map (("- " <>) . describeError) (validationErrors s)

    describeError (StatusCodeMismatch expected actual) =
        T.pack $ printf "Status code mismatch: expected %d, got %d" expected actual
    describeError (FieldNotFound path) =
        "Field not found: `" <> path <> "`"
    describeError (FieldValueMismatch path _ _) =
        "Field value mismatch at `" <> path <> "`"
    describeError BodyNotJSON =
        "Response body is not valid JSON"

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
    ]
  where
    row label val = T.pack $ printf "| %s | %s |" (label :: String) (val :: String)

bayesTable :: BayesianComparison -> [Text]
bayesTable b =
    [ "| Metric | Value |"
    , "|--------|-------|"
    , row "P(candidate faster)" (printf "%.1f%%" (probBFasterThanA b * 100))
    , row "Mean difference" (printf "%+.2f ms" (meanDifference b))
    , row "95% credible interval" credInterval
    , row "Effect size (Cohen's d)" (printf "%.3f" (effectSize b))
    , row "Relative effect" (printf "%+.1f%%" (relativeEffect b * 100))
    , row "p95 difference" (pctRow $ p95Comparison b)
    , row "p99 difference" (pctRow $ p99Comparison b)
    , row "Mann-Whitney U" (mwuCell $ mannWhitneyU b)
    , row "Kolmogorov-Smirnov" (ksCell $ kolmogorovSmirnov b)
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
    mwuCell Nothing = "sample too small"
    mwuCell (Just r)
        | mwuSignificant r = "significant (p < 0.05)"
        | otherwise = "not significant (p >= 0.05)"
    ksCell Nothing = "sample too small"
    ksCell (Just r) =
        printf
            "D = %.3f, p = %.3f (%s)"
            (ksStatistic r)
            (ksPValue r)
            (if ksSignificant r then "significant" else "not significant" :: String)
