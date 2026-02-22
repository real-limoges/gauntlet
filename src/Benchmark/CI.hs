{- |
Module      : Benchmark.CI
Description : GitLab CI integration for benchmark results
Stability   : experimental

Detects CI environment and formats regression output for GitLab pipelines.
Writes markdown reports for merge request artifacts.
-}
module Benchmark.CI (
    CIMode (..),
    detectCIMode,
    formatForCI,
    writeArtifactReport,
) where

import Benchmark.Types (MetricRegression (..), RegressionResult (..))
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv)
import Text.Printf (printf)

-- | CI environment detection.
data CIMode = GitLab | None
    deriving (Show, Eq)

-- | Detect if running in GitLab CI.
detectCIMode :: IO CIMode
detectCIMode = do
    gitlab <- lookupEnv "GITLAB_CI"
    return $ case gitlab of
        Just "true" -> GitLab
        _ -> None

{- | Format regression results for CI output.
In GitLab, section markers and colored output help visibility.
-}
formatForCI :: CIMode -> RegressionResult -> IO ()
formatForCI None _ = return ()
formatForCI GitLab result = do
    epoch <- (round <$> getPOSIXTime) :: IO Int
    putStrLn ""
    putStrLn $ "\x1b[0Ksection_start:" ++ show epoch ++ ":benchmark_results[collapsed=false]"
    putStrLn "\x1b[0K\x1b[1;34m========== Benchmark Regression Check ==========\x1b[0m"
    putStrLn ""

    forM_ (regressionMetrics result) $ \m -> do
        let status = if metricRegressed m then "\x1b[1;31mREGRESSED\x1b[0m" else "\x1b[1;32mok\x1b[0m"
        printf
            "  %s: %.2fms -> %.2fms (%+.1f%%) [%s]\n"
            (T.unpack $ metricName m)
            (metricBaseline m)
            (metricCurrent m)
            (metricChange m * 100)
            status

    putStrLn ""
    if regressionPassed result
        then putStrLn "\x1b[1;32m[PASS] Benchmark passed - no regressions detected\x1b[0m"
        else putStrLn "\x1b[1;31m[FAIL] Benchmark FAILED - regression detected\x1b[0m"

    putStrLn ""
    putStrLn $ "\x1b[0Ksection_end:" ++ show epoch ++ ":benchmark_results"

-- | Write a markdown report for GitLab artifacts/MR comments.
writeArtifactReport :: FilePath -> RegressionResult -> IO ()
writeArtifactReport path result =
    TIO.writeFile path (formatMarkdownReport result)

formatMarkdownReport :: RegressionResult -> Text
formatMarkdownReport result =
    T.unlines $
        [ "## Benchmark Results"
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
        | otherwise = "**Status:** FAILED - Regression Detected"

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
        | regressionPassed result =
            [ "All metrics within acceptable thresholds."
            ]
        | otherwise =
            [ "**Action Required:** Performance regression detected. Please investigate before merging."
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
