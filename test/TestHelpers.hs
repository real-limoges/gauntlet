-- | Shared test fixtures and helper functions.
module TestHelpers
  ( makeResult
  , makeErrorResult
  , makeResponseWithBody
  , mockStats
  , makeValidConfig
  , makeLifecycleHooks
  , makeHookCommand
  , makeSpan
  , makeBaseline
  , mockBayesianComparison
  , mockPercentileComparison
  , captureStdout
  , makeCapturingLogger
  , cleanTest
  , withCleanEnv
  )
where

import Benchmark.Types
  ( Baseline (..)
  , BayesianComparison (..)
  , BenchmarkConfig (..)
  , BenchmarkStats (..)
  , HookCommand (..)
  , LifecycleHooks (..)
  , LogLevel (..)
  , NamedTarget (..)
  , Nanoseconds (..)
  , PayloadSpec (..)
  , PercentileComparison (..)
  , Settings (..)
  , TestingResponse (..)
  )
import Control.Monad (when)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, modifyIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Word (Word64)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Log (Logger (..))
import System.Environment (unsetEnv)
import System.IO (hClose, hFlush, hSetEncoding, stdout, utf8)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Tracing.Types (Span (..), SpanKind (..), SpanStatus (..))

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2000 1 1) 0

-- | Build a successful 200 response with the given duration in nanoseconds.
makeResult :: Integer -> TestingResponse
makeResult ns =
  TestingResponse
    { durationNs = Nanoseconds (fromIntegral ns)
    , statusCode = 200
    , respBody = Nothing
    , errorMessage = Nothing
    , requestedAt = epoch
    }

-- | Build a failed response with the given error message and zero duration.
makeErrorResult :: String -> TestingResponse
makeErrorResult msg =
  TestingResponse
    { durationNs = Nanoseconds 0
    , statusCode = 0
    , respBody = Nothing
    , errorMessage = Just msg
    , requestedAt = epoch
    }

-- | Build a successful response with the given status code and body.
makeResponseWithBody :: Int -> LBS.ByteString -> TestingResponse
makeResponseWithBody status body =
  TestingResponse
    { durationNs = Nanoseconds 1000
    , statusCode = status
    , respBody = Just body
    , errorMessage = Nothing
    , requestedAt = epoch
    }

-- | Build a BenchmarkStats with 30 successful requests, given mean and stddev.
mockStats :: Double -> Double -> BenchmarkStats
mockStats mean dev =
  BenchmarkStats
    { totalRequests = 30
    , countSuccess = 30
    , countFailure = 0
    , meanMs = mean
    , stdDevMs = dev
    , minMs = mean - 3 * dev
    , maxMs = mean + 3 * dev
    , p50Ms = mean
    , p95Ms = mean + 1.65 * dev
    , p99Ms = mean + 2.33 * dev
    , esMs = mean + 3 * dev
    , histogram = [(mean - 3 * dev, 2), (mean - dev, 10), (mean, 12), (mean + dev, 4), (mean + 3 * dev, 2)]
    }

-- | A valid 'BenchmarkConfig' with two named targets and one payload.
makeValidConfig :: BenchmarkConfig
makeValidConfig =
  BenchmarkConfig
    { benchTargets =
        [ NamedTarget
            { targetName = "primary"
            , targetUrl = "http://primary.test"
            , targetBranch = Nothing
            , targetLifecycle = Nothing
            }
        , NamedTarget
            { targetName = "candidate"
            , targetUrl = "http://candidate.test"
            , targetBranch = Just "feature"
            , targetLifecycle = Nothing
            }
        ]
    , benchSettings =
        Settings
          { iterations = 10
          , concurrency = 2
          , secrets = Just "secrets.txt"
          , maxConnections = Nothing
          , requestTimeout = Nothing
          , retry = Nothing
          , warmup = Nothing
          , logLevel = Nothing
          , tempo = Nothing
          , loadMode = Nothing
          }
    , benchPayloads =
        [ PayloadSpec
            { specName = "test-payload"
            , specMethod = "POST"
            , specPath = "/api/test"
            , specBody = Nothing
            , specHeaders = Nothing
            , specValidate = Nothing
            }
        ]
    }

-- | A minimal 'LifecycleHooks' with all fields set to Nothing.
makeLifecycleHooks :: LifecycleHooks
makeLifecycleHooks =
  LifecycleHooks
    { hookSetup = Nothing
    , hookTeardown = Nothing
    , hookHealthCheck = Nothing
    }

-- | A 'HookCommand' with the given shell command and no timeout or working dir.
makeHookCommand :: Text -> HookCommand
makeHookCommand cmd =
  HookCommand
    { hookCmd = cmd
    , hookTimeoutSecs = Nothing
    , hookWorkingDir = Nothing
    }

-- | Build a Span with the given operation name and duration in nanoseconds.
makeSpan :: Text -> Word64 -> Span
makeSpan name duration =
  Span
    { spanId = "span-id-1"
    , spanParentId = Nothing
    , spanName = name
    , spanServiceName = "test-service"
    , spanKind = SpanKindInternal
    , spanStartTimeNs = Nanoseconds 0
    , spanEndTimeNs = Nanoseconds duration
    , spanDurationNs = Nanoseconds duration
    , spanStatus = StatusOk
    , spanAttributes = Map.empty
    }

-- | Build a Baseline with the given name, a fixed timestamp, and the provided stats.
makeBaseline :: Text -> BenchmarkStats -> Baseline
makeBaseline name stats =
  Baseline
    { baselineName = name
    , baselineTimestamp = "2024-01-01T00:00:00Z"
    , baselineStats = stats
    }

-- | Fixture BayesianComparison with realistic values and Nothing for distribution tests.
mockBayesianComparison :: BayesianComparison
mockBayesianComparison =
  BayesianComparison
    { probBFasterThanA = 0.85
    , probSingleRequestFaster = 0.65
    , probBLessJittery = 0.72
    , meanDifference = 5.0
    , credibleIntervalLower = 2.0
    , credibleIntervalUpper = 8.0
    , effectSize = 0.45
    , relativeEffect = 10.0
    , p95Comparison = mockPercentileComparison
    , p99Comparison = mockPercentileComparison
    , emd = Just 2.5
    }

-- | Fixture PercentileComparison.
mockPercentileComparison :: PercentileComparison
mockPercentileComparison =
  PercentileComparison
    { pctDifference = 3.0
    , pctCredibleLower = 1.0
    , pctCredibleUpper = 5.0
    , probPctRegression = 0.15
    }

-- | Capture stdout by redirecting to a temp file.
captureStdout :: IO () -> IO String
captureStdout action =
  withSystemTempFile "stdout-capture" $ \path handle -> do
    hSetEncoding handle utf8
    oldStdout <- hDuplicate stdout
    hDuplicateTo handle stdout
    action
    hFlush stdout
    hDuplicateTo oldStdout stdout
    hClose handle
    readFile path

{-| Create a Logger that captures (LogLevel, Text) pairs into an IORef.
Uses the same level-filtering logic as makeLogger.
-}
makeCapturingLogger :: LogLevel -> IORef [(LogLevel, Text)] -> Logger
makeCapturingLogger minLevel ref =
  Logger
    { logLevel = minLevel
    , logAction = \(level, _, msg) ->
        when (level >= minLevel) $ modifyIORef ref ((level, msg) :)
    }

-- | Create a test case that cleans CI environment variables before and after.
cleanTest :: String -> IO () -> TestTree
cleanTest name action = testCase name $ withCleanEnv action

-- | Unset both CI env vars before and after an action to avoid cross-test contamination.
withCleanEnv :: IO () -> IO ()
withCleanEnv action = do
  unsetEnv "GITLAB_CI"
  unsetEnv "GITHUB_ACTIONS"
  action
  unsetEnv "GITLAB_CI"
  unsetEnv "GITHUB_ACTIONS"
