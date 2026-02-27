module StatsCommonSpec (statsCommonSpec) where

import Data.Vector.Unboxed qualified as V
import Stats.Common
import Test.Hspec
import Test.QuickCheck

statsCommonSpec :: Spec
statsCommonSpec = describe "Stats.Common" $ do
  describe "percentile" $ do
    it "returns correct R-7 interpolated value for 10-element vector" $ do
      let vec = V.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      percentile 0.5 vec `shouldBe` 5.5

    it "returns 0 for empty vector" $
      percentile 0.5 V.empty `shouldBe` 0

    it "returns the element for single-element vector" $
      percentile 0.5 (V.singleton 42.0) `shouldBe` 42.0

    it "returns minimum for percentile 0.0" $ do
      let vec = V.fromList [3, 1, 4, 1, 5]
      percentile 0.0 vec `shouldBe` 1.0

    it "returns maximum for percentile 1.0" $ do
      let vec = V.fromList [3, 1, 4, 1, 5]
      percentile 1.0 vec `shouldBe` 5.0

  describe "percentileSorted" $ do
    it "agrees with percentile on pre-sorted input" $ do
      let sorted = V.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      percentileSorted 0.75 sorted `shouldBe` percentile 0.75 sorted

  describe "percentileList" $ do
    it "agrees with percentileSorted on same data" $ do
      let sorted = V.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      percentileList 0.75 (V.toList sorted) `shouldBe` percentileSorted 0.75 sorted

  describe "stdDev" $ do
    it "returns expected sample stddev for known dataset" $ do
      let vec = V.fromList [2, 4, 4, 4, 5, 5, 7, 9]
      abs (stdDev vec - 2.138) `shouldSatisfy` (< 0.01)

    it "returns 0 for single element" $
      stdDev (V.singleton 5.0) `shouldBe` 0

    it "returns 0 for empty vector" $
      stdDev V.empty `shouldBe` 0

  describe "stdDevList" $ do
    it "agrees with stdDev given precomputed mean" $ do
      let xs = [2, 4, 4, 4, 5, 5, 7, 9]
          avg = sum xs / fromIntegral (length xs)
      abs (stdDevList avg xs - stdDev (V.fromList xs)) `shouldSatisfy` (< 1e-10)

  describe "QuickCheck properties" $ do
    it "percentile is between min and max for non-empty vectors" $
      property $ \(NonEmpty xs) -> do
        let vec = V.fromList (map getPositive xs :: [Double])
            p = 0.5
            result = percentile p vec
        result `shouldSatisfy` (>= V.minimum vec)
        result `shouldSatisfy` (<= V.maximum vec)

    it "stdDev is non-negative" $
      property $ \xs -> do
        let vec = V.fromList (xs :: [Double])
        stdDev vec `shouldSatisfy` (>= 0)
