{-|
Module      : Benchmark.CI
Description : CI environment integration for benchmark results
Stability   : experimental

Detects CI environment and formats regression output for GitLab and GitHub Actions pipelines.
Writes markdown reports for merge request artifacts and job summaries.
-}
module Benchmark.CI
  ( CIMode (..)
  , detectCIMode
  , formatForCI
  , writeArtifactReport
  , writeGitHubStepSummary
  ) where

import Benchmark.Report.Markdown (markdownRegressionReport)
import Benchmark.Types (MetricRegression (..), RegressionResult (..))
import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv)
import Text.Printf (printf)

-- | CI environment detection.
data CIMode = GitLab | GitHub | None
  deriving (Show, Eq)

-- | Detect if running in a known CI environment.
detectCIMode :: IO CIMode
detectCIMode = do
  gitlab <- lookupEnv "GITLAB_CI"
  github <- lookupEnv "GITHUB_ACTIONS"
  return $ case (gitlab, github) of
    (Just "true", _) -> GitLab
    (_, Just "true") -> GitHub
    _ -> None

{-| Format regression results for CI output.
In GitLab, section markers and colored output help visibility.
GitHub Actions uses plain stdout (no section markers).
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
    let status :: String
        status = if metricRegressed m then "\x1b[1;31mREGRESSED\x1b[0m" else "\x1b[1;32mok\x1b[0m"
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
formatForCI GitHub result = do
  putStrLn ""
  putStrLn "========== Benchmark Regression Check =========="
  putStrLn ""

  forM_ (regressionMetrics result) $ \m -> do
    let status = if metricRegressed m then "REGRESSED" else "ok" :: String
    printf
      "  %s: %.2fms -> %.2fms (%+.1f%%) [%s]\n"
      (T.unpack $ metricName m)
      (metricBaseline m)
      (metricCurrent m)
      (metricChange m * 100)
      status

  putStrLn ""
  if regressionPassed result
    then putStrLn "[PASS] Benchmark passed - no regressions detected"
    else putStrLn "[FAIL] Benchmark FAILED - regression detected"
  putStrLn ""

-- | Write a markdown report for CI artifacts / GitHub Step Summary.
writeArtifactReport :: FilePath -> RegressionResult -> IO ()
writeArtifactReport path result =
  TIO.writeFile path (markdownRegressionReport result)

-- | Write the markdown regression report to $GITHUB_STEP_SUMMARY if set.
writeGitHubStepSummary :: RegressionResult -> IO ()
writeGitHubStepSummary result = do
  mpath <- lookupEnv "GITHUB_STEP_SUMMARY"
  case mpath of
    Nothing -> return ()
    Just path -> TIO.appendFile path (markdownRegressionReport result)
