-- | Tests for Benchmark.Reporter.HTML.
module HTMLSpec (htmlSpec) where

import Benchmark.Reporter (Reporter (..))
import Benchmark.Reporter.HTML (htmlReporter)
import Benchmark.Types
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (textShouldContain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (mockBayesianComparison, mockStats)

htmlSpec :: TestTree
htmlSpec =
  testGroup
    "Benchmark.Reporter.HTML"
    [ testCase "single report contains HTML structure" $
        withTempHTML $ \path reporter -> do
          reportSingle reporter "http://test.local" (mockStats 50 10) []
          html <- TIO.readFile path
          html `textShouldContain` "<!DOCTYPE html>"
          html `textShouldContain` "<title>Gauntlet Report</title>"
          html `textShouldContain` "</html>"
          html `textShouldContain` "<style>"
    , testCase "single report contains stats table" $
        withTempHTML $ \path reporter -> do
          reportSingle reporter "http://test.local" (mockStats 50 10) []
          html <- TIO.readFile path
          html `textShouldContain` "Mean (ms)"
          html `textShouldContain` "p50 (ms)"
          html `textShouldContain` "p95 (ms)"
          html `textShouldContain` "p99 (ms)"
    , testCase "single report contains SVG chart" $
        withTempHTML $ \path reporter -> do
          reportSingle reporter "http://test.local" (mockStats 50 10) []
          html <- TIO.readFile path
          html `textShouldContain` "<svg"
          html `textShouldContain` "</svg>"
          html `textShouldContain` "<rect"
    , testCase "benchmark report contains comparison section" $
        withTempHTML $ \path reporter -> do
          let namedStats = Map.fromList [("alpha", mockStats 50 10), ("beta", mockStats 60 12)]
              pairs = [("alpha", "beta", mockBayesianComparison)]
          reportBenchmark reporter namedStats pairs []
          html <- TIO.readFile path
          html `textShouldContain` "Benchmark Comparison"
          html `textShouldContain` "alpha"
          html `textShouldContain` "beta"
          html `textShouldContain` "P(B faster)"
          html `textShouldContain` "Cohen"
    , testCase "regression report shows pass/fail status" $
        withTempHTML $ \path reporter -> do
          let regression =
                RegressionResult
                  { regressionBaseline = "v1"
                  , regressionMetrics =
                      [ MetricRegression
                          { metricName = "mean_ms"
                          , metricBaseline = 50.0
                          , metricCurrent = 57.5
                          , metricChange = 0.15
                          , metricThreshold = 0.10
                          , metricRegressed = True
                          }
                      ]
                  , regressionPassed = False
                  }
          reportRegression reporter regression
          html <- TIO.readFile path
          html `textShouldContain` "REGRESSED"
          html `textShouldContain` "FAILED"
          html `textShouldContain` "mean_ms"
    , testCase "validation failures appear in report" $
        withTempHTML $ \path reporter -> do
          let valids = [ValidationSummary {totalValidated = 10, totalFailed = 3, validationErrors = []}]
          reportSingle reporter "http://test.local" (mockStats 50 10) valids
          html <- TIO.readFile path
          html `textShouldContain` "Validation"
          html `textShouldContain` "FAIL"
    , testCase "escapes HTML entities in target URL" $
        withTempHTML $ \path reporter -> do
          reportSingle reporter "http://test.local?a=1&b=2" (mockStats 50 10) []
          html <- TIO.readFile path
          html `textShouldContain` "&amp;"
    ]

withTempHTML :: (FilePath -> Reporter -> IO ()) -> IO ()
withTempHTML action =
  withSystemTempFile "html-test.html" $ \path h -> do
    hClose h
    action path (htmlReporter path)
