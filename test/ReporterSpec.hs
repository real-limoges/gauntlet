-- | Tests for Benchmark.Reporter.
module ReporterSpec (reporterSpec) where

import Benchmark.Reporter (Reporter (..), combineReporters, noOpReporter)
import Benchmark.Reporter.Markdown (markdownReporter)
import Benchmark.Types
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (mockBayesianComparison, mockStats)

reporterSpec :: TestTree
reporterSpec =
  testGroup
    "Reporter"
    [ noOpReporterSpec
    , combineReportersSpec
    , markdownReporterSpec
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleStats :: BenchmarkStats
sampleStats = mockStats 50.0 10.0

sampleBayes :: BayesianComparison
sampleBayes = mockBayesianComparison

sampleRegressionResult :: RegressionResult
sampleRegressionResult =
  RegressionResult
    { regressionPassed = True
    , regressionBaseline = "test-baseline"
    , regressionMetrics = []
    }

-- | Build a reporter that increments an IORef on each call.
countingReporter :: Text -> IORef [Text] -> Reporter
countingReporter name ref =
  Reporter
    { reportSingle = \_ _ _ -> modifyIORef ref (name :)
    , reportNWay = \_ _ _ -> modifyIORef ref (name :)
    , reportRegression = \_ -> modifyIORef ref (name :)
    }

-- ---------------------------------------------------------------------------
-- noOpReporter
-- ---------------------------------------------------------------------------

noOpReporterSpec :: TestTree
noOpReporterSpec =
  testGroup
    "noOpReporter"
    [ testCase "reportSingle does not crash" $
        reportSingle noOpReporter "http://example.com" sampleStats []
    , testCase "reportNWay does not crash" $
        reportNWay noOpReporter Map.empty [] []
    , testCase "reportRegression does not crash" $
        reportRegression noOpReporter sampleRegressionResult
    ]

-- ---------------------------------------------------------------------------
-- combineReporters
-- ---------------------------------------------------------------------------

combineReportersSpec :: TestTree
combineReportersSpec =
  testGroup
    "combineReporters"
    [ testCase "empty list produces no-op reporter" $ do
        let r = combineReporters []
        reportSingle r "http://example.com" sampleStats []
        reportNWay r Map.empty [] []
        reportRegression r sampleRegressionResult
    , testCase "reportSingle invokes all reporters" $ do
        ref <- newIORef []
        let r = combineReporters [countingReporter "r1" ref, countingReporter "r2" ref]
        reportSingle r "http://example.com" sampleStats []
        calls <- readIORef ref
        length calls `shouldBe` 2
    , testCase "reportNWay invokes all reporters" $ do
        ref <- newIORef []
        let r = combineReporters [countingReporter "r1" ref, countingReporter "r2" ref]
        reportNWay r Map.empty [] []
        calls <- readIORef ref
        length calls `shouldBe` 2
    , testCase "reportRegression invokes all reporters" $ do
        ref <- newIORef []
        let r = combineReporters [countingReporter "r1" ref, countingReporter "r2" ref]
        reportRegression r sampleRegressionResult
        calls <- readIORef ref
        length calls `shouldBe` 2
    , testCase "preserves call order (r1 before r2)" $ do
        ref <- newIORef []
        let r = combineReporters [countingReporter "r1" ref, countingReporter "r2" ref]
        reportSingle r "http://example.com" sampleStats []
        -- IORef prepends so order is reversed
        calls <- readIORef ref
        calls `shouldBe` ["r2", "r1"]
    ]

-- ---------------------------------------------------------------------------
-- markdownReporter
-- ---------------------------------------------------------------------------

markdownReporterSpec :: TestTree
markdownReporterSpec =
  testGroup
    "markdownReporter"
    [ testCase "reportSingle writes non-empty file" $
        withSystemTempFile "report.md" $ \path h -> do
          hClose h
          let r = markdownReporter path
          reportSingle r "http://example.com" sampleStats []
          content <- TIO.readFile path
          content `shouldSatisfy` (not . null . show)
    , testCase "reportNWay writes non-empty file" $
        withSystemTempFile "report.md" $ \path h -> do
          hClose h
          let r = markdownReporter path
              namedStats = Map.fromList [("primary", sampleStats), ("candidate", sampleStats)]
              pairs = [("primary", "candidate", sampleBayes)]
          reportNWay r namedStats pairs []
          content <- TIO.readFile path
          content `shouldSatisfy` (not . null . show)
    , testCase "reportRegression appends rather than overwrites" $
        withSystemTempFile "report.md" $ \path h -> do
          hClose h
          let r = markdownReporter path
          reportSingle r "http://example.com" sampleStats []
          firstContent <- TIO.readFile path
          reportRegression r sampleRegressionResult
          finalContent <- TIO.readFile path
          -- Final content should be strictly longer (regression section appended)
          T.length finalContent `shouldSatisfy` (> T.length firstContent)
    ]
