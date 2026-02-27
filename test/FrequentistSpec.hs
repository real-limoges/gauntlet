module FrequentistSpec (frequentistSpec) where

import Benchmark.Types
import Data.Vector.Unboxed qualified as V
import Stats.Benchmark
import Test.Hspec
import Test.QuickCheck
import TestHelpers (makeResult, mockStats)

-- | Make N responses with given duration in nanoseconds
makeResponses :: Int -> Integer -> [TestingResponse]
makeResponses n ns = replicate n (makeResult ns)

frequentistSpec :: Spec
frequentistSpec = describe "Stats.Benchmark (Frequentist)" $ do
  describe "addFrequentistTests" $ do
    it "identical samples yield MWU not significant" $ do
      let responses = makeResponses 30 10_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 10 1)
          result = addFrequentistTests responses responses bayes
      mannWhitneyU result `shouldBe` Just (MWUResult False)

    it "clearly different samples yield MWU significant" $ do
      let responsesA = makeResponses 30 10_000_000
          responsesB = makeResponses 30 100_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 100 1)
          result = addFrequentistTests responsesA responsesB bayes
      case mannWhitneyU result of
        Just (MWUResult sig) -> sig `shouldBe` True
        Nothing -> expectationFailure "Expected MWU result"

    it "fewer than 2 responses yields Nothing for MWU and KS" $ do
      let responsesA = makeResponses 1 10_000_000
          responsesB = makeResponses 1 20_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 20 1)
          result = addFrequentistTests responsesA responsesB bayes
      mannWhitneyU result `shouldBe` Nothing
      kolmogorovSmirnov result `shouldBe` Nothing

    it "identical samples yield KS statistic near 0" $ do
      let responses = makeResponses 30 10_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 10 1)
          result = addFrequentistTests responses responses bayes
      case kolmogorovSmirnov result of
        Just ks -> ksStatistic ks `shouldSatisfy` (< 0.01)
        Nothing -> expectationFailure "Expected KS result"

    it "clearly different samples yield KS significant" $ do
      let responsesA = makeResponses 30 10_000_000
          responsesB = makeResponses 30 100_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 100 1)
          result = addFrequentistTests responsesA responsesB bayes
      case kolmogorovSmirnov result of
        Just ks -> ksSignificant ks `shouldBe` True
        Nothing -> expectationFailure "Expected KS result"

    it "fewer than 5 per side yields Nothing for AD" $ do
      let responsesA = makeResponses 4 10_000_000
          responsesB = makeResponses 4 20_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 20 1)
          result = addFrequentistTests responsesA responsesB bayes
      andersonDarling result `shouldBe` Nothing

    it "clearly different samples with 50+ each yield AD significant" $ do
      let responsesA = makeResponses 50 10_000_000
          responsesB = makeResponses 50 100_000_000
          bayes = compareBayesian (mockStats 10 1) (mockStats 100 1)
          result = addFrequentistTests responsesA responsesB bayes
      case andersonDarling result of
        Just ad -> adSignificant ad `shouldBe` True
        Nothing -> expectationFailure "Expected AD result"

  describe "earthMoversDistance" $ do
    it "returns 0 for identical vectors" $ do
      let v = V.fromList [1.0, 2.0, 3.0]
      earthMoversDistance v v `shouldBe` 0

    it "returns 1.0 for [0,0,0] vs [1,1,1]" $ do
      let a = V.fromList [0, 0, 0]
          b = V.fromList [1, 1, 1]
      earthMoversDistance a b `shouldBe` 1.0

    it "returns positive result for unequal-size samples" $ do
      let a = V.fromList [1.0, 2.0, 3.0]
          b = V.fromList [4.0, 5.0]
      earthMoversDistance a b `shouldSatisfy` (> 0)

    it "returns 0 for empty vs non-empty vectors" $ do
      earthMoversDistance V.empty (V.fromList [1]) `shouldBe` 0
      earthMoversDistance (V.fromList [1]) V.empty `shouldBe` 0

    it "earthMoversDistance a b >= 0 for non-empty vectors (QuickCheck)" $
      property $ \(NonEmpty xs) (NonEmpty ys) -> do
        let a = V.fromList (map getPositive xs :: [Double])
            b = V.fromList (map getPositive ys :: [Double])
        earthMoversDistance a b `shouldSatisfy` (>= 0)

    it "earthMoversDistance a a == 0 for non-empty vectors (QuickCheck)" $
      property $ \(NonEmpty xs) -> do
        let a = V.fromList (map getPositive xs :: [Double])
        earthMoversDistance a a `shouldSatisfy` (<= 1e-10)
