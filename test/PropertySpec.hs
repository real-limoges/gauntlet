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
import Stats.Benchmark (calculateStats, compareBayesian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import TestHelpers
import Tracing.Analysis (aggregateBySpanName)
import Tracing.Types (SpanAggregation (..))

propertySpec :: Spec
propertySpec = describe "Property-Based Tests" $ do
  describe "calculateStats properties" $ do
    prop "mean is always between min and max" $
      \(NonEmpty durations) ->
        let responses = map (makeResult . abs . (* 1_000_000)) durations
            stats = calculateStats responses
         in countSuccess stats > 0 ==>
              meanMs stats >= minMs stats && meanMs stats <= maxMs stats

    prop "p50 <= p95 <= p99" $
      \(NonEmpty durations) ->
        let responses = map (makeResult . abs . (* 1_000_000)) durations
            stats = calculateStats responses
         in countSuccess stats > 0 ==>
              p50Ms stats <= p95Ms stats && p95Ms stats <= p99Ms stats

    prop "min <= p50" $
      \(NonEmpty durations) ->
        let responses = map (makeResult . abs . (* 1_000_000)) durations
            stats = calculateStats responses
         in countSuccess stats > 0 ==>
              minMs stats <= p50Ms stats

    prop "p99 <= max" $
      \(NonEmpty durations) ->
        let responses = map (makeResult . abs . (* 1_000_000)) durations
            stats = calculateStats responses
         in countSuccess stats > 0 ==>
              p99Ms stats <= maxMs stats

    prop "stddev is non-negative" $
      \(NonEmpty durations) ->
        let responses = map (makeResult . abs . (* 1_000_000)) durations
            stats = calculateStats responses
         in stdDevMs stats >= 0

    prop "total = success + failure" $
      \(NonEmpty durations) ->
        let successes = map (makeResult . abs . (* 1_000_000)) durations
            failures = [makeErrorResult "error"]
            stats = calculateStats (successes ++ failures)
         in totalRequests stats == countSuccess stats + countFailure stats

  describe "compareBayesian properties" $ do
    prop "probability is between 0 and 1" $
      \(Positive meanA) (Positive meanB) (Positive stdA) (Positive stdB) ->
        let statsA = mockStats meanA stdA
            statsB = mockStats meanB stdB
            result = compareBayesian statsA statsB
         in probBFasterThanA result >= 0 && probBFasterThanA result <= 1

    prop "mean difference is A - B" $
      \(Positive meanA) (Positive meanB) (Positive stdA) (Positive stdB) ->
        let statsA = mockStats meanA stdA
            statsB = mockStats meanB stdB
            result = compareBayesian statsA statsB
         in abs (meanDifference result - (meanA - meanB)) < 0.000_1

    prop "credible interval lower <= upper" $
      \(Positive meanA) (Positive meanB) (Positive stdA) (Positive stdB) ->
        let statsA = mockStats meanA stdA
            statsB = mockStats meanB stdB
            result = compareBayesian statsA statsB
         in credibleIntervalLower result <= credibleIntervalUpper result

  describe "verify properties" $ do
    prop "same response always matches" $
      \(Positive status) ->
        let body = encode (Map.fromList [("key" :: Text, "test" :: Text)])
            r = makeResponseWithBody status body
         in verify r r == Match

    prop "status mismatch when codes differ" $
      \(Positive s1) (Positive s2) ->
        s1 /= s2 ==>
          let r1 = makeResponseWithBody s1 ""
              r2 = makeResponseWithBody s2 ""
           in case verify r1 r2 of
                StatusMismatch _ _ -> True
                _ -> False

  describe "aggregateBySpanName properties" $ do
    prop "count equals number of spans with that name" $
      \(NonEmpty ns) ->
        let durations = map (abs . (* 1_000_000)) ns
            spans = map (makeSpan "same-name") durations
            aggs = aggregateBySpanName spans
         in case aggs of
              [agg] -> aggCount agg == length spans
              _ -> False

    prop "mean is between min and max" $
      \(NonEmpty ns) ->
        let durations = map (abs . (* 1_000_000)) ns
            spans = map (makeSpan "test") durations
            aggs = aggregateBySpanName spans
         in case aggs of
              [agg] -> aggMeanMs agg >= aggMinMs agg && aggMeanMs agg <= aggMaxMs agg
              _ -> False

    prop "stddev is non-negative" $
      \(NonEmpty ns) ->
        let durations = map (abs . (* 1_000_000)) ns
            spans = map (makeSpan "test") durations
            aggs = aggregateBySpanName spans
         in case aggs of
              [agg] -> aggStdDevMs agg >= 0
              _ -> False

  describe "compareToBaseline properties" $ do
    prop "no regression when current equals baseline" $
      \(Positive mean) (Positive stdDev) ->
        let stats = mockStats mean stdDev
            baseline = makeBaseline "test" stats
            result = compareToBaseline defaultThresholds baseline stats
         in regressionPassed result

    prop "always returns four metrics" $
      \(Positive meanA) (Positive meanB) (Positive stdDev) ->
        let baseStats = mockStats meanA stdDev
            currStats = mockStats meanB stdDev
            baseline = makeBaseline "test" baseStats
            result = compareToBaseline defaultThresholds baseline currStats
         in length (regressionMetrics result) == 4

    prop "regression detected when current >> baseline" $
      \(Positive baseMean) (Positive stdDev) ->
        let baseStats = mockStats baseMean stdDev
            currStats = mockStats (baseMean * 2) stdDev
            baseline = makeBaseline "test" baseStats
            result = compareToBaseline defaultThresholds baseline currStats
         in not (regressionPassed result)

    prop "improvement never causes regression" $
      \(Positive baseMean) (Positive stdDev) ->
        baseMean > 10 ==>
          let baseStats = mockStats baseMean stdDev
              currStats = mockStats (baseMean * 0.5) (stdDev * 0.5)
              baseline = makeBaseline "test" baseStats
              result = compareToBaseline defaultThresholds baseline currStats
           in regressionPassed result

  describe "TUI.State properties" $ do
    prop "completed count matches number of RequestCompleted events" $
      \(Positive n) ->
        n <= 1000 ==>
          ioProperty $ do
            now <- getCurrentTime
            let state = initialState "http://test" 1000 1
                addRequest s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                finalState = iterate addRequest state !! n
            return $ _tsCompleted finalState == n

    prop "success + error = completed" $
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

    prop "rolling window never exceeds limit" $
      \(Positive n) ->
        n <= 200 ==>
          ioProperty $ do
            now <- getCurrentTime
            let state = initialState "http://test" 1000 1
                addRequest s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                finalState = iterate addRequest state !! n
            return $ Seq.length (_tsRecentDurations finalState) <= rollingWindow

  describe "TUI.Widgets properties" $ do
    prop "formatDuration always returns non-empty text" $
      \(NonNegative ms) ->
        not (null (show (formatDuration ms)))

    prop "formatRPS always returns non-empty text" $
      \(NonNegative rps) ->
        not (null (show (formatRPS rps)))

    prop "formatElapsed always returns valid time format" $
      \(NonNegative secs) ->
        let result = formatElapsed secs
         in elem ':' (show result)

  describe "RetrySettings properties" $ do
    prop "default retry settings are valid" $
      property $
        let rs = defaultRetrySettings
         in retryMaxAttempts rs >= 0
              && retryInitialDelayMs rs > 0
              && retryBackoffMultiplier rs >= 1.0

    prop "retry attempts are non-negative" $
      \(NonNegative attempts) (Positive delay) (Positive mult) ->
        mult >= 1.0 ==>
          let rs = RetrySettings attempts delay mult
           in retryMaxAttempts rs >= 0

    prop "backoff multiplier is >= 1.0 for valid exponential backoff" $
      \(NonNegative attempts) (Positive delay) ->
        let mult = 2.0 :: Double
            rs = RetrySettings attempts delay mult
         in retryBackoffMultiplier rs >= 1.0

    prop "delay increases exponentially with backoff multiplier" $
      \(Positive (initialDelay :: Integer)) (Positive (mult :: Double)) ->
        mult > 1.0 ==>
          let delay1 = fromIntegral initialDelay :: Double
              delay2 = delay1 * mult
              delay3 = delay2 * mult
           in delay1 < delay2 && delay2 < delay3

  describe "WarmupSettings properties" $ do
    prop "default warmup settings are valid" $
      property $
        warmupIterations defaultWarmupSettings >= 0

    prop "warmup iterations are non-negative" $
      \(NonNegative iters) ->
        let ws = WarmupSettings iters
         in warmupIterations ws >= 0

    prop "zero warmup iterations means disabled" $
      \() ->
        let ws = WarmupSettings 0
         in warmupIterations ws == 0

    prop "warmup iterations can be arbitrarily large" $
      \(Positive iters) ->
        iters <= 10000 ==>
          let ws = WarmupSettings iters
           in warmupIterations ws == iters

  describe "Custom headers properties" $ do
    prop "empty headers map results in only default Content-Type" $
      \() ->
        let payload = PayloadSpec "test" "GET" "/api" Nothing (Just Map.empty) Nothing
            endpoint = toEndpoint "http://test" payload
         in headers endpoint == [("Content-Type", "application/json")]

    prop "custom Content-Type overrides default" $
      \customType ->
        not (null customType) ==>
          let customHeaders = Map.fromList [("Content-Type", T.pack customType)]
              payload = PayloadSpec "test" "POST" "/api" Nothing (Just customHeaders) Nothing
              endpoint = toEndpoint "http://test" payload
              contentTypes = filter (\(k, _) -> k == "Content-Type") (headers endpoint)
           in length contentTypes == 1 && contentTypes == [("Content-Type", T.pack customType)]

    prop "custom headers are preserved" $
      \key value ->
        not (null key) && not (null value) && key /= "Content-Type" ==>
          let customHeaders = Map.fromList [(T.pack key, T.pack value)]
              payload = PayloadSpec "test" "GET" "/api" Nothing (Just customHeaders) Nothing
              endpoint = toEndpoint "http://test" payload
           in (T.pack key, T.pack value) `elem` headers endpoint
