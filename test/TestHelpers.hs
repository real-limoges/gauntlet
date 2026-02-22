module TestHelpers (
    makeResult,
    makeErrorResult,
    makeResponseWithBody,
    mockStats,
    makeValidConfig,
    makeSpan,
    makeBaseline,
)
where

import Benchmark.Types
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64)
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
                , httpVersion = Nothing
                , healthCheckPath = Nothing
                , healthCheckTimeout = Nothing
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
