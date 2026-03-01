module TestHelpers
  ( makeResult
  , makeErrorResult
  , makeResponseWithBody
  , mockStats
  , makeValidConfig
  , makeSpan
  , makeBaseline
  , mockBayesianComparison
  , mockPercentileComparison
  , captureStdout
  , makeCapturingLogger
  )
where

import Benchmark.Types
import Control.Monad (when)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Log (Logger (..))
import System.IO (hClose, hFlush, hSetEncoding, stdout, utf8)
import System.IO.Temp (withSystemTempFile)
import Tracing.Types

makeResult :: Integer -> TestingResponse
makeResult ns =
  TestingResponse
    { durationNs = Nanoseconds (fromIntegral ns)
    , statusCode = 200
    , respBody = Nothing
    , errorMessage = Nothing
    }

makeErrorResult :: String -> TestingResponse
makeErrorResult msg =
  TestingResponse
    { durationNs = Nanoseconds 0
    , statusCode = 0
    , respBody = Nothing
    , errorMessage = Just msg
    }

makeResponseWithBody :: Int -> LBS.ByteString -> TestingResponse
makeResponseWithBody status body =
  TestingResponse
    { durationNs = Nanoseconds 1000
    , statusCode = status
    , respBody = Just body
    , errorMessage = Nothing
    }

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
    }

makeValidConfig :: TestConfig
makeValidConfig =
  TestConfig
    { targets =
        Targets
          { primary = "http://primary.test"
          , candidate = "http://candidate.test"
          }
    , git =
        Targets
          { primary = "main"
          , candidate = "feature"
          }
    , settings =
        Settings
          { iterations = 10
          , concurrency = 2
          , secrets = "secrets.txt"
          , maxConnections = Nothing
          , connIdleTimeout = Nothing
          , requestTimeout = Nothing
          , retry = Nothing
          , warmup = Nothing
          , logLevel = Nothing
          , tempo = Nothing
          , healthCheckPath = Nothing
          , healthCheckTimeout = Nothing
          , floatTolerance = Nothing
          , compareFields = Nothing
          , ignoreFields = Nothing
          , verifyIterations = Nothing
          }
    , payloads =
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
    , meanDifference = 5.0
    , credibleIntervalLower = 2.0
    , credibleIntervalUpper = 8.0
    , effectSize = 0.45
    , relativeEffect = 10.0
    , p95Comparison = mockPercentileComparison
    , p99Comparison = mockPercentileComparison
    , mannWhitneyU = Nothing
    , kolmogorovSmirnov = Nothing
    , andersonDarling = Nothing
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
