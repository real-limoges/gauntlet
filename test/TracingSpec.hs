module TracingSpec (tracingSpec) where

import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers
import Tracing.Analysis (aggregateBySpanName)
import Tracing.Types (SpanAggregation (..))

tracingSpec :: TestTree
tracingSpec =
  testGroup
    "Tracing.Analysis"
    [ testGroup
        "aggregateBySpanName"
        [ testCase "returns empty list for empty spans" $ do
            aggregateBySpanName [] `shouldBe` []
        , testCase "aggregates single span correctly" $ do
            let span1 = makeSpan "test-span" 10_000_000
            case aggregateBySpanName [span1] of
              [agg] -> do
                aggSpanName agg `shouldBe` "test-span"
                aggCount agg `shouldBe` 1
                aggMeanMs agg `shouldBe` 10.0
              _ -> assertFailure "Expected exactly one aggregation"
        , testCase "groups spans by name" $ do
            let spans =
                  [ makeSpan "span-a" 10_000_000
                  , makeSpan "span-b" 20_000_000
                  , makeSpan "span-a" 30_000_000
                  ]
            let aggs = aggregateBySpanName spans
            length aggs `shouldBe` 2
        , testCase "calculates correct mean for grouped spans" $ do
            let spans =
                  [ makeSpan "test" 10_000_000
                  , makeSpan "test" 20_000_000
                  , makeSpan "test" 30_000_000
                  ]
            case aggregateBySpanName spans of
              [agg] -> aggMeanMs agg `shouldBe` 20.0
              _ -> assertFailure "Expected exactly one aggregation"
        , testCase "calculates min and max correctly" $ do
            let spans =
                  [ makeSpan "test" 5_000_000
                  , makeSpan "test" 100_000_000
                  , makeSpan "test" 25_000_000
                  ]
            case aggregateBySpanName spans of
              [agg] -> do
                aggMinMs agg `shouldBe` 5.0
                aggMaxMs agg `shouldBe` 100.0
              _ -> assertFailure "Expected exactly one aggregation"
        , testCase "calculates standard deviation" $ do
            let spans =
                  [ makeSpan "test" 10_000_000
                  , makeSpan "test" 20_000_000
                  , makeSpan "test" 30_000_000
                  ]
            case aggregateBySpanName spans of
              [agg] -> aggStdDevMs agg `shouldSatisfy` (\x -> x > 9.5 && x < 10.5)
              _ -> assertFailure "Expected exactly one aggregation"
        ]
    ]
