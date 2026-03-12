-- | Tests for Benchmark.Report.CI.
module CISpec (ciSpec) where

import Benchmark.Report.CI
import Benchmark.Types (MetricRegression (..), RegressionResult (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (setEnv, unsetEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (shouldBe, shouldReturn, shouldSatisfy)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (testCase)
import TestHelpers (cleanTest, withCleanEnv)

ciSpec :: TestTree
ciSpec =
  testGroup
    "Benchmark.CI"
    [ sequentialTestGroup
        "detectCIMode"
        AllSucceed
        [ cleanTest "returns GitLab when GITLAB_CI=true" $ do
            setEnv "GITLAB_CI" "true"
            mode <- detectCIMode
            mode `shouldBe` GitLab
        , cleanTest "returns GitHub when GITHUB_ACTIONS=true" $ do
            setEnv "GITHUB_ACTIONS" "true"
            mode <- detectCIMode
            mode `shouldBe` GitHub
        , cleanTest "returns None when neither env var is set" $ do
            mode <- detectCIMode
            mode `shouldBe` None
        , cleanTest "GitLab wins when both are set" $ do
            setEnv "GITLAB_CI" "true"
            setEnv "GITHUB_ACTIONS" "true"
            mode <- detectCIMode
            mode `shouldBe` GitLab
        ]
    , testGroup
        "formatForCI"
        [ testCase "is a no-op for None mode regardless of result" $ do
            formatForCI None mockPassed `shouldReturn` ()
            formatForCI None mockFailed `shouldReturn` ()
        ]
    , testGroup
        "writeArtifactReport"
        [ testCase "writes regression markdown with PASSED for passing result" $
            withSystemTempFile "report.md" $ \path h -> do
              hClose h
              writeArtifactReport path mockPassed
              contents <- TIO.readFile path
              contents `shouldSatisfy` T.isInfixOf "Regression Check"
              contents `shouldSatisfy` T.isInfixOf "my-baseline"
              contents `shouldSatisfy` T.isInfixOf "PASSED"
        , testCase "written file contains FAILED for regression result" $
            withSystemTempFile "report.md" $ \path h -> do
              hClose h
              writeArtifactReport path mockFailed
              contents <- TIO.readFile path
              contents `shouldSatisfy` T.isInfixOf "FAILED"
        ]
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mockPassed :: RegressionResult
mockPassed =
  RegressionResult
    { regressionBaseline = "my-baseline"
    , regressionPassed = True
    , regressionMetrics =
        [ MetricRegression "p50" 50.0 51.0 0.02 0.2 False
        ]
    }

mockFailed :: RegressionResult
mockFailed =
  RegressionResult
    { regressionBaseline = "my-baseline"
    , regressionPassed = False
    , regressionMetrics =
        [ MetricRegression "p99" 100.0 135.0 0.35 0.2 True
        ]
    }
