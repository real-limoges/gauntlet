-- | Tests for Benchmark.Report.Baseline.
module BaselineSpec (baselineSpec) where

import Benchmark.Config.CLI (BaselineMode (..))
import Benchmark.Report.Baseline (compareToBaseline, handleBaseline)
import Benchmark.Reporter (ReportingContext (..), noOpReporter)
import Benchmark.Types
  ( BenchmarkStats (..)
  , LogLevel (..)
  , MetricRegression (..)
  , RegressionResult (..)
  , RegressionThresholds (..)
  , RunResult (..)
  , defaultThresholds
  )
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (find)
import Data.Text qualified as T
import Log (Logger)
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers (cleanTest, makeBaseline, makeCapturingLogger, mockStats)

baselineSpec :: TestTree
baselineSpec =
  testGroup
    "Benchmark.Baseline"
    [ testGroup
        "compareToBaseline"
        [ testCase "passes when all metrics are within threshold" $ do
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 105.0 10.0
            let result = compareToBaseline defaultThresholds baseline current
            regressionPassed result `shouldBe` True
            regressionBaseline result `shouldBe` "v1.0"
        , testCase "fails when mean exceeds threshold" $ do
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 115.0 10.0
            let result = compareToBaseline defaultThresholds baseline current
            regressionPassed result `shouldBe` False
        , testCase "fails when any percentile exceeds threshold" $ do
            let baseStats = mockStats 100.0 10.0
                baseline = makeBaseline "v1.0" baseStats
                check currStats =
                  regressionPassed (compareToBaseline defaultThresholds baseline currStats) `shouldBe` False
            -- p50 exceeded
            check (baseStats {p50Ms = 115.0})
            -- p95 exceeded
            check (baseStats {p95Ms = p95Ms baseStats * 1.15})
            -- p99 exceeded
            check (baseStats {p99Ms = p99Ms baseStats * 1.20})
        , testCase "passes when p99 is within higher threshold (15%)" $ do
            let baseStats = mockStats 100.0 10.0
            let currStats = baseStats {p99Ms = p99Ms baseStats * 1.10}
            let baseline = makeBaseline "v1.0" baseStats
            let result = compareToBaseline defaultThresholds baseline currStats
            regressionPassed result `shouldBe` True
        , testCase "passes when metrics improve (negative change)" $ do
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 80.0 8.0
            let result = compareToBaseline defaultThresholds baseline current
            regressionPassed result `shouldBe` True
        , testCase "calculates correct percentage change" $ do
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 120.0 10.0
            let result = compareToBaseline defaultThresholds baseline current
            case find ((== "mean") . metricName) (regressionMetrics result) of
              Just meanMetric -> metricChange meanMetric `shouldSatisfy` (\c -> abs (c - 0.20) < 0.001)
              Nothing -> assertFailure "Expected mean metric"
        , testCase "marks only regressed metrics as regressed" $ do
            let baseStats = mockStats 100.0 10.0
            let currStats = baseStats {meanMs = 115.0}
            let baseline = makeBaseline "v1.0" baseStats
            let result = compareToBaseline defaultThresholds baseline currStats
            let regressedMetrics = filter metricRegressed (regressionMetrics result)
            length regressedMetrics `shouldBe` 1
            case regressedMetrics of
              [m] -> metricName m `shouldBe` "mean"
              _ -> assertFailure "Expected exactly one regressed metric"
        ]
    , testGroup
        "checkMetric edge cases (via compareToBaseline)"
        [ testCase "handles zero baseline (current also zero)" $ do
            let baseStats = (mockStats 100.0 10.0) {meanMs = 0.0}
            let currStats = baseStats {meanMs = 0.0}
            let baseline = makeBaseline "v1.0" baseStats
            let result = compareToBaseline defaultThresholds baseline currStats
            case find ((== "mean") . metricName) (regressionMetrics result) of
              Just meanMetric -> do
                metricChange meanMetric `shouldBe` 0.0
                metricRegressed meanMetric `shouldBe` False
              Nothing -> assertFailure "Expected mean metric"
        , testCase "handles zero baseline (current non-zero)" $ do
            let baseStats = (mockStats 100.0 10.0) {meanMs = 0.0}
            let currStats = baseStats {meanMs = 50.0}
            let baseline = makeBaseline "v1.0" baseStats
            let result = compareToBaseline defaultThresholds baseline currStats
            case find ((== "mean") . metricName) (regressionMetrics result) of
              Just meanMetric -> do
                metricChange meanMetric `shouldBe` 1.0
                metricRegressed meanMetric `shouldBe` True
              Nothing -> assertFailure "Expected mean metric"
        , testCase "exact threshold boundary does not regress" $ do
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 110.0 10.0
            let result = compareToBaseline defaultThresholds baseline current
            case find ((== "mean") . metricName) (regressionMetrics result) of
              Just meanMetric -> metricRegressed meanMetric `shouldBe` False
              Nothing -> assertFailure "Expected mean metric"
        ]
    , testGroup
        "custom thresholds"
        [ testCase "uses stricter thresholds correctly" $ do
            let strictThresholds = RegressionThresholds 0.05 0.05 0.05 0.05
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 108.0 10.0
            let result = compareToBaseline strictThresholds baseline current
            regressionPassed result `shouldBe` False
        , testCase "uses looser thresholds correctly" $ do
            let looseThresholds = RegressionThresholds 0.25 0.25 0.25 0.25
            let baseline = makeBaseline "v1.0" (mockStats 100.0 10.0)
            let current = mockStats 120.0 10.0
            let result = compareToBaseline looseThresholds baseline current
            regressionPassed result `shouldBe` True
        ]
    , handleBaselineSpec
    ]

handleBaselineSpec :: TestTree
handleBaselineSpec =
  sequentialTestGroup
    "handleBaseline"
    AllSucceed
    [ testGroup
        "NoBaseline"
        [ cleanTest "returns RunSuccess with no log output" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <- handleBaseline (ReportingContext noOpReporter NoBaseline logger) hbTimestamp hbStats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              msgs `shouldBe` []
        ]
    , testGroup
        "SaveBaseline"
        [ cleanTest "returns RunSuccess and logs 'Baseline saved'" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <- handleBaseline (ReportingContext noOpReporter (SaveBaseline "test") logger) hbTimestamp hbStats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "Baseline saved"
        , cleanTest "logs warning when save path is invalid" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <- handleBaseline (ReportingContext noOpReporter (SaveBaseline "foo\0bar") logger) hbTimestamp hbStats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "save"
        ]
    , testGroup
        "CompareBaseline"
        [ cleanTest "returns RunError when baseline missing" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <-
                handleBaseline (ReportingContext noOpReporter (CompareBaseline "missing") logger) hbTimestamp hbStats
              case result of
                RunError _ -> pure ()
                other -> error $ "Expected RunError, got: " <> show other
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "not found"
        , cleanTest "returns RunSuccess when baseline matches (no regression)" $
            inTempDir $ do
              (logger, _) <- makeTestLogger
              _ <- handleBaseline (ReportingContext noOpReporter (SaveBaseline "test") logger) hbTimestamp hbStats
              (logger2, _) <- makeTestLogger
              result <- handleBaseline (ReportingContext noOpReporter (CompareBaseline "test") logger2) hbTimestamp hbStats
              result `shouldBe` RunSuccess
        , cleanTest "returns RunRegression when stats regressed" $
            inTempDir $ do
              (logger, _) <- makeTestLogger
              let goodStats = mockStats 10.0 1.0
              _ <- handleBaseline (ReportingContext noOpReporter (SaveBaseline "perf") logger) hbTimestamp goodStats
              let badStats = mockStats 50.0 5.0
              (logger2, _) <- makeTestLogger
              result <- handleBaseline (ReportingContext noOpReporter (CompareBaseline "perf") logger2) hbTimestamp badStats
              case result of
                RunRegression _ -> pure ()
                other -> assertFailure $ "Expected RunRegression, got: " ++ show other
        ]
    , testGroup
        "SaveAndCompare"
        [ cleanTest "saves and compares" $
            inTempDir $ do
              (logger, _) <- makeTestLogger
              _ <- handleBaseline (ReportingContext noOpReporter (SaveBaseline "compare") logger) hbTimestamp hbStats
              (logger2, logRef) <- makeTestLogger
              result <-
                handleBaseline (ReportingContext noOpReporter (SaveAndCompare "save" "compare") logger2) hbTimestamp hbStats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "Baseline saved"
        ]
    ]
  where
    hbStats = mockStats 50.0 5.0
    hbTimestamp = "2024-01-01T00-00-00" :: T.Text

makeTestLogger :: IO (Logger, IORef [(LogLevel, T.Text)])
makeTestLogger = do
  logRef <- newIORef ([] :: [(LogLevel, T.Text)])
  let logger = makeCapturingLogger Debug logRef
  pure (logger, logRef)

inTempDir :: IO a -> IO a
inTempDir action =
  withSystemTempDirectory "baseline-test" $ \dir ->
    withCurrentDirectory dir action
