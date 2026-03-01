module PropertySpec (propertySpec) where

import Benchmark.Baseline (compareToBaseline)
import Benchmark.TUI.State
import Benchmark.Types
import Data.Sequence qualified as Seq
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
        [ testProperty "regression detected when current >> baseline" $
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
                      addRequest = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200)
                      finalState = iterate addRequest state !! n
                  return $ tsCompleted finalState == n
        , testProperty "success + error = completed" $
            \successes failures ->
              let n = abs successes `mod` 100
                  m = abs failures `mod` 100
               in ioProperty $ do
                    now <- getCurrentTime
                    let state = initialState "http://test" 1000 1
                        addSuccess = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200)
                        addFailure = updateState now (RequestFailed "error")
                        afterSuccesses = iterate addSuccess state !! n
                        finalState = iterate addFailure afterSuccesses !! m
                    return $ tsSuccessCount finalState + tsErrorCount finalState == tsCompleted finalState
        , testProperty "rolling window never exceeds limit" $
            \(Positive n) ->
              n <= 200 ==>
                ioProperty $ do
                  now <- getCurrentTime
                  let state = initialState "http://test" 1000 1
                      addRequest = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200)
                      finalState = iterate addRequest state !! n
                  return $ Seq.length (tsRecentDurations finalState) <= rollingWindow
        ]
    ]
