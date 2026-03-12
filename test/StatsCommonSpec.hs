-- | Tests for Stats.Common.
module StatsCommonSpec (statsCommonSpec) where

import Data.Vector.Unboxed qualified as V
import Stats.Common (percentile, stdDev)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.QuickCheck (NonEmptyList (NonEmpty), Positive (getPositive), ioProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

statsCommonSpec :: TestTree
statsCommonSpec =
  testGroup
    "Stats.Common"
    [ testGroup
        "percentile"
        [ testCase "returns correct R-7 interpolated value for 10-element vector" $ do
            let vec = V.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            percentile 0.5 vec `shouldBe` 5.5
        , testCase "returns 0 for empty vector" $
            percentile 0.5 V.empty `shouldBe` 0
        , testCase "returns the element for single-element vector" $
            percentile 0.5 (V.singleton 42.0) `shouldBe` 42.0
        , testCase "returns minimum for percentile 0.0" $ do
            let vec = V.fromList [3, 1, 4, 1, 5]
            percentile 0.0 vec `shouldBe` 1.0
        , testCase "returns maximum for percentile 1.0" $ do
            let vec = V.fromList [3, 1, 4, 1, 5]
            percentile 1.0 vec `shouldBe` 5.0
        ]
    , testGroup
        "stdDev"
        [ testCase "returns expected sample stddev for known dataset" $ do
            let vec = V.fromList [2, 4, 4, 4, 5, 5, 7, 9]
            abs (stdDev vec - 2.138) `shouldSatisfy` (< 0.01)
        , testCase "returns 0 for single element" $
            stdDev (V.singleton 5.0) `shouldBe` 0
        , testCase "returns 0 for empty vector" $
            stdDev V.empty `shouldBe` 0
        ]
    , testGroup
        "QuickCheck properties"
        [ testProperty "percentile is between min and max for non-empty vectors" $
            \(NonEmpty xs) -> ioProperty $ do
              let vec = V.fromList (map getPositive xs :: [Double])
                  p = 0.5
                  result = percentile p vec
              result `shouldSatisfy` (>= V.minimum vec)
              result `shouldSatisfy` (<= V.maximum vec)
        , testProperty "stdDev is non-negative" $
            \xs -> ioProperty $ do
              let vec = V.fromList (xs :: [Double])
              stdDev vec `shouldSatisfy` (>= 0)
        ]
    ]
