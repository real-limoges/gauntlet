module RunnerBaselineSpec (runnerBaselineSpec) where

import Benchmark.CLI (BaselineMode (..))
import Benchmark.Types
import Data.IORef
import Data.Text qualified as T
import Log (Logger)
import Runner.Baseline (handleBaseline)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.Environment (unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers (makeCapturingLogger, mockStats)

-- Module-level constants for tests
stats :: BenchmarkStats
stats = mockStats 50.0 5.0

timestamp :: T.Text
timestamp = "2024-01-01T00-00-00"

runnerBaselineSpec :: TestTree
runnerBaselineSpec =
  sequentialTestGroup
    "Runner.Baseline.handleBaseline"
    AllSucceed
    [ testGroup
        "NoBaseline"
        [ cleanTest "returns RunSuccess with no log output" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <- handleBaseline logger NoBaseline timestamp stats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              msgs `shouldBe` []
        ]
    , testGroup
        "SaveBaseline"
        [ cleanTest "returns RunSuccess and logs 'Baseline saved'" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <- handleBaseline logger (SaveBaseline "test") timestamp stats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "Baseline saved"
        , cleanTest "logs 'Error:' when save path is invalid" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              -- Use a path with null bytes which is invalid on all platforms
              result <- handleBaseline logger (SaveBaseline "foo\0bar") timestamp stats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "Error:"
        ]
    , testGroup
        "CompareBaseline"
        [ cleanTest "returns RunSuccess with error log when baseline missing" $
            inTempDir $ do
              (logger, logRef) <- makeTestLogger
              result <- handleBaseline logger (CompareBaseline "missing") timestamp stats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "Error:"
        , cleanTest "returns RunSuccess when baseline matches (no regression)" $
            inTempDir $ do
              (logger, _) <- makeTestLogger
              -- First save a baseline
              _ <- handleBaseline logger (SaveBaseline "test") timestamp stats
              -- Then compare against it (same stats = no regression)
              (logger2, _) <- makeTestLogger
              result <- handleBaseline logger2 (CompareBaseline "test") timestamp stats
              result `shouldBe` RunSuccess
        , cleanTest "returns RunRegression when stats regressed" $
            inTempDir $ do
              (logger, _) <- makeTestLogger
              -- Save a baseline with low latency
              let goodStats = mockStats 10.0 1.0
              _ <- handleBaseline logger (SaveBaseline "perf") timestamp goodStats
              -- Compare with much worse stats (>10% regression on all metrics)
              let badStats = mockStats 50.0 5.0
              (logger2, _) <- makeTestLogger
              result <- handleBaseline logger2 (CompareBaseline "perf") timestamp badStats
              case result of
                RunRegression _ -> pure ()
                other -> assertFailure $ "Expected RunRegression, got: " ++ show other
        ]
    , testGroup
        "SaveAndCompare"
        [ cleanTest "saves and compares" $
            inTempDir $ do
              -- First create the compare baseline
              (logger, _) <- makeTestLogger
              _ <- handleBaseline logger (SaveBaseline "compare") timestamp stats
              -- Now save-and-compare
              (logger2, logRef) <- makeTestLogger
              result <- handleBaseline logger2 (SaveAndCompare "save" "compare") timestamp stats
              result `shouldBe` RunSuccess
              msgs <- readIORef logRef
              let logText = T.concat [msg | (_, msg) <- msgs]
              logText `shouldSatisfy` T.isInfixOf "Baseline saved"
        ]
    ]

-- Helpers

cleanTest :: String -> IO () -> TestTree
cleanTest name action = testCase name $ withCleanEnv action

makeTestLogger :: IO (Logger, IORef [(LogLevel, T.Text)])
makeTestLogger = do
  logRef <- newIORef ([] :: [(LogLevel, T.Text)])
  let logger = makeCapturingLogger Debug logRef
  pure (logger, logRef)

withCleanEnv :: IO () -> IO ()
withCleanEnv action = do
  unsetEnv "GITLAB_CI"
  unsetEnv "GITHUB_ACTIONS"
  action
  unsetEnv "GITLAB_CI"
  unsetEnv "GITHUB_ACTIONS"

inTempDir :: IO a -> IO a
inTempDir action =
  withSystemTempDirectory "baseline-test" $ \dir -> do
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    action
