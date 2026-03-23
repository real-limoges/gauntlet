-- | Tests for Benchmark.Reporter.JUnit.
module JUnitSpec (junitSpec) where

import Benchmark.Reporter (Reporter (..))
import Benchmark.Reporter.JUnit (junitReporter)
import Benchmark.Types
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (shouldBe, textShouldContain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (mockBayesianComparison, mockStats)

junitSpec :: TestTree
junitSpec =
  testGroup
    "Benchmark.Reporter.JUnit"
    [ testCase "single report contains XML header and testsuite" $
        withTempJUnit $ \path reporter -> do
          reportSingle reporter "http://test.local" (mockStats 50 10) []
          xml <- TIO.readFile path
          xml `textShouldContain` "<?xml version=\"1.0\""
          xml `textShouldContain` "<testsuite"
          xml `textShouldContain` "mean_ms"
          xml `textShouldContain` "p50_ms"
          xml `textShouldContain` "p95_ms"
          xml `textShouldContain` "p99_ms"
    , testCase "single report escapes URL in testsuite name" $
        withTempJUnit $ \path reporter -> do
          reportSingle reporter "http://test.local?a=1&b=2" (mockStats 50 10) []
          xml <- TIO.readFile path
          xml `textShouldContain` "&amp;"
    , testCase "benchmark report contains testsuites wrapper" $
        withTempJUnit $ \path reporter -> do
          let namedStats = Map.fromList [("alpha", mockStats 50 10), ("beta", mockStats 60 12)]
              pairs = [("alpha", "beta", mockBayesianComparison)]
          reportBenchmark reporter namedStats pairs []
          xml <- TIO.readFile path
          xml `textShouldContain` "<testsuites name=\"gauntlet\">"
          xml `textShouldContain` "</testsuites>"
          xml `textShouldContain` "alpha"
          xml `textShouldContain` "beta"
    , testCase "benchmark report includes comparison test cases" $
        withTempJUnit $ \path reporter -> do
          let namedStats = Map.fromList [("a", mockStats 50 10)]
              pairs = [("a", "b", mockBayesianComparison)]
          reportBenchmark reporter namedStats pairs []
          xml <- TIO.readFile path
          xml `textShouldContain` "prob_b_faster"
          xml `textShouldContain` "cohens_d"
    , testCase "regression report marks regressed metrics as failures" $
        withTempJUnit $ \path reporter -> do
          let regression =
                RegressionResult
                  { regressionBaseline = "baseline-v1"
                  , regressionMetrics =
                      [ MetricRegression
                          { metricName = "mean_ms"
                          , metricBaseline = 50.0
                          , metricCurrent = 57.5
                          , metricChange = 0.15
                          , metricThreshold = 0.10
                          , metricRegressed = True
                          }
                      , MetricRegression
                          { metricName = "p50_ms"
                          , metricBaseline = 48.0
                          , metricCurrent = 50.4
                          , metricChange = 0.05
                          , metricThreshold = 0.10
                          , metricRegressed = False
                          }
                      ]
                  , regressionPassed = False
                  }
          reportRegression reporter regression
          xml <- TIO.readFile path
          xml `textShouldContain` "<failure"
          xml `textShouldContain` "mean_ms"
          -- Non-regressed metric should not have a failure element
          let failureCount = length $ T.breakOnAll "<failure" xml
          failureCount `shouldBe` 1
    , testCase "validation failures appear in single report" $
        withTempJUnit $ \path reporter -> do
          let valids = [ValidationSummary {totalValidated = 10, totalFailed = 3, validationErrors = []}]
          reportSingle reporter "http://test.local" (mockStats 50 10) valids
          xml <- TIO.readFile path
          xml `textShouldContain` "<failure"
          xml `textShouldContain` "3 of 10"
    , testCase "validation passes produce no failures" $
        withTempJUnit $ \path reporter -> do
          let valids = [ValidationSummary {totalValidated = 10, totalFailed = 0, validationErrors = []}]
          reportSingle reporter "http://test.local" (mockStats 50 10) valids
          xml <- TIO.readFile path
          let failureCount = length $ T.breakOnAll "<failure" xml
          failureCount `shouldBe` 0
    ]

withTempJUnit :: (FilePath -> Reporter -> IO ()) -> IO ()
withTempJUnit action =
  withSystemTempFile "junit-test.xml" $ \path h -> do
    hClose h
    action path (junitReporter path)
