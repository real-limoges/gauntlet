-- | CI integration for GitLab CI and GitHub Actions artifact reports.
module Benchmark.Report.CI
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
  printRegressionMetrics ansiRed ansiGreen result
  putStrLn $ "\x1b[0Ksection_end:" ++ show epoch ++ ":benchmark_results"
formatForCI GitHub result = do
  putStrLn ""
  putStrLn "========== Benchmark Regression Check =========="
  putStrLn ""
  printRegressionMetrics id id result

-- | Print per-metric lines and pass/fail summary.
-- Takes colorizers for "bad" (red) and "good" (green) text respectively.
printRegressionMetrics :: (String -> String) -> (String -> String) -> RegressionResult -> IO ()
printRegressionMetrics colorBad colorGood result = do
  forM_ (regressionMetrics result) $ \m -> do
    let status = if metricRegressed m then colorBad "REGRESSED" else colorGood "ok"
    printf
      "  %s: %.2fms -> %.2fms (%+.1f%%) [%s]\n"
      (T.unpack $ metricName m)
      (metricBaseline m)
      (metricCurrent m)
      (metricChange m * 100)
      status
  putStrLn ""
  if regressionPassed result
    then putStrLn $ colorGood "[PASS] Benchmark passed - no regressions detected"
    else putStrLn $ colorBad "[FAIL] Benchmark FAILED - regression detected"
  putStrLn ""

ansiRed :: String -> String
ansiRed s = "\x1b[1;31m" ++ s ++ "\x1b[0m"

ansiGreen :: String -> String
ansiGreen s = "\x1b[1;32m" ++ s ++ "\x1b[0m"

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
