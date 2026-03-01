module PropertySpec (propertySpec) where

import Benchmark.Baseline (compareToBaseline)
import Benchmark.Config (toEndpoint)
import Benchmark.TUI.State
import Benchmark.TUI.Widgets
import Benchmark.Types
import Benchmark.Verify (verify)
import Data.Aeson (encode)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Stats.Benchmark (calculateStats)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestHelpers
import Tracing.Analysis (aggregateBySpanName)
import Tracing.Types (SpanAggregation (..))

propertySpec :: TestTree
propertySpec =
  testGroup
    "Property-Based Tests"
    [ testGroup
        "calculateStats properties"
        [ testProperty "mean is always between min and max" $
            \(NonEmpty durations) ->
              let responses = map (makeResult . abs . (* 1_000_000)) durations
                  stats = calculateStats responses
               in countSuccess stats > 0 ==>
                    meanMs stats >= minMs stats && meanMs stats <= maxMs stats
        , testProperty "p50 <= p95 <= p99" $
            \(NonEmpty durations) ->
              let responses = map (makeResult . abs . (* 1_000_000)) durations
                  stats = calculateStats responses
               in countSuccess stats > 0 ==>
                    p50Ms stats <= p95Ms stats && p95Ms stats <= p99Ms stats
        , testProperty "min <= p50" $
            \(NonEmpty durations) ->
              let responses = map (makeResult . abs . (* 1_000_000)) durations
                  stats = calculateStats responses
               in countSuccess stats > 0 ==>
                    minMs stats <= p50Ms stats
        , testProperty "p99 <= max" $
            \(NonEmpty durations) ->
              let responses = map (makeResult . abs . (* 1_000_000)) durations
                  stats = calculateStats responses
               in countSuccess stats > 0 ==>
                    p99Ms stats <= maxMs stats
        , testProperty "stddev is non-negative" $
            \(NonEmpty durations) ->
              let responses = map (makeResult . abs . (* 1_000_000)) durations
                  stats = calculateStats responses
               in stdDevMs stats >= 0
        , testProperty "total = success + failure" $
            \(NonEmpty durations) ->
              let successes = map (makeResult . abs . (* 1_000_000)) durations
                  failures = [makeErrorResult "error"]
                  stats = calculateStats (successes ++ failures)
               in totalRequests stats == countSuccess stats + countFailure stats
        ]
    , testGroup
        "verify properties"
        [ testProperty "same response always matches" $
            \(Positive status) ->
              let body = encode (Map.fromList [("key" :: Text, "test" :: Text)])
                  r = makeResponseWithBody status body
               in verify 0.0 Nothing Nothing r r == Match
        , testProperty "status mismatch when codes differ" $
            \(Positive s1) (Positive s2) ->
              s1 /= s2 ==>
                let r1 = makeResponseWithBody s1 ""
                    r2 = makeResponseWithBody s2 ""
                 in case verify 0.0 Nothing Nothing r1 r2 of
                      StatusMismatch _ _ -> True
                      _ -> False
        ]
    , testGroup
        "aggregateBySpanName properties"
        [ testProperty "count equals number of spans with that name" $
            \(NonEmpty ns) ->
              let durations = map (abs . (* 1_000_000)) ns
                  spans = map (makeSpan "same-name") durations
                  aggs = aggregateBySpanName spans
               in case aggs of
                    [agg] -> aggCount agg == length spans
                    _ -> False
        , testProperty "mean is between min and max" $
            \(NonEmpty ns) ->
              let durations = map (abs . (* 1_000_000)) ns
                  spans = map (makeSpan "test") durations
                  aggs = aggregateBySpanName spans
               in case aggs of
                    [agg] -> aggMeanMs agg >= aggMinMs agg && aggMeanMs agg <= aggMaxMs agg
                    _ -> False
        , testProperty "stddev is non-negative" $
            \(NonEmpty ns) ->
              let durations = map (abs . (* 1_000_000)) ns
                  spans = map (makeSpan "test") durations
                  aggs = aggregateBySpanName spans
               in case aggs of
                    [agg] -> aggStdDevMs agg >= 0
                    _ -> False
        ]
    , testGroup
        "compareToBaseline properties"
        [ testProperty "no regression when current equals baseline" $
            \(Positive mean) (Positive stdDev) ->
              let stats = mockStats mean stdDev
                  baseline = makeBaseline "test" stats
                  result = compareToBaseline defaultThresholds baseline stats
               in regressionPassed result
        , testProperty "always returns four metrics" $
            \(Positive meanA) (Positive meanB) (Positive stdDev) ->
              let baseStats = mockStats meanA stdDev
                  currStats = mockStats meanB stdDev
                  baseline = makeBaseline "test" baseStats
                  result = compareToBaseline defaultThresholds baseline currStats
               in length (regressionMetrics result) == 4
        , testProperty "regression detected when current >> baseline" $
            \(Positive baseMean) (Positive stdDev) ->
              let baseStats = mockStats baseMean stdDev
                  currStats = mockStats (baseMean * 2) stdDev
                  baseline = makeBaseline "test" baseStats
                  result = compareToBaseline defaultThresholds baseline currStats
               in not (regressionPassed result)
        , testProperty "improvement never causes regression" $
            forAll (choose (11, 10000)) $ \baseMean ->
              forAll (choose (1, 1000)) $ \stdDev ->
                let baseStats = mockStats baseMean stdDev
                    currStats = mockStats (baseMean * 0.5) (stdDev * 0.5)
                    baseline = makeBaseline "test" baseStats
                    result = compareToBaseline defaultThresholds baseline currStats
                 in regressionPassed result
        ]
    , testGroup
        "TUI.State properties"
        [ testProperty "completed count matches number of RequestCompleted events" $
            \(Positive n) ->
              n <= 1000 ==>
                ioProperty $ do
                  now <- getCurrentTime
                  let state = initialState "http://test" 1000 1
                      addRequest s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                      finalState = iterate addRequest state !! n
                  return $ _tsCompleted finalState == n
        , testProperty "success + error = completed" $
            \successes failures ->
              let n = abs successes `mod` 100
                  m = abs failures `mod` 100
               in ioProperty $ do
                    now <- getCurrentTime
                    let state = initialState "http://test" 1000 1
                        addSuccess s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                        addFailure s = updateState now (RequestFailed "error") s
                        afterSuccesses = iterate addSuccess state !! n
                        finalState = iterate addFailure afterSuccesses !! m
                    return $ _tsSuccessCount finalState + _tsErrorCount finalState == _tsCompleted finalState
        , testProperty "rolling window never exceeds limit" $
            \(Positive n) ->
              n <= 200 ==>
                ioProperty $ do
                  now <- getCurrentTime
                  let state = initialState "http://test" 1000 1
                      addRequest s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                      finalState = iterate addRequest state !! n
                  return $ Seq.length (_tsRecentDurations finalState) <= rollingWindow
        ]
    , testGroup
        "TUI.Widgets properties"
        [ testProperty "formatDuration always returns non-empty text" $
            \(NonNegative ms) ->
              not (null (show (formatDuration ms)))
        , testProperty "formatRPS always returns non-empty text" $
            \(NonNegative rps) ->
              not (null (show (formatRPS rps)))
        , testProperty "formatElapsed always returns valid time format" $
            \(NonNegative secs) ->
              let result = formatElapsed secs
               in elem ':' (show result)
        ]
    , testGroup
        "Custom headers properties"
        [ testProperty "empty headers map results in only default Content-Type" $
            \() ->
              let payload = PayloadSpec "test" "GET" "/api" Nothing (Just Map.empty) Nothing
                  endpoint = toEndpoint "http://test" payload
               in headers endpoint == [("Content-Type", "application/json")]
        , testProperty "custom Content-Type overrides default" $
            \customType ->
              not (null customType) ==>
                let customHeaders = Map.fromList [("Content-Type", T.pack customType)]
                    payload = PayloadSpec "test" "POST" "/api" Nothing (Just customHeaders) Nothing
                    endpoint = toEndpoint "http://test" payload
                    contentTypes = filter (\(k, _) -> k == "Content-Type") (headers endpoint)
                 in length contentTypes == 1 && contentTypes == [("Content-Type", T.pack customType)]
        , testProperty "custom headers are preserved" $
            \key value ->
              not (null key) && not (null value) && key /= "Content-Type" ==>
                let customHeaders = Map.fromList [(T.pack key, T.pack value)]
                    payload = PayloadSpec "test" "GET" "/api" Nothing (Just customHeaders) Nothing
                    endpoint = toEndpoint "http://test" payload
                 in (T.pack key, T.pack value) `elem` headers endpoint
        ]
    ]
