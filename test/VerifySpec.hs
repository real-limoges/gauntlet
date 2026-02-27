module VerifySpec (verifySpec) where

import Benchmark.Types (Nanoseconds (..), TestingResponse (..), VerificationResult (..))
import Benchmark.Verify (verify)
import Data.Aeson (encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Hspec
import TestHelpers

verifySpec :: Spec
verifySpec = describe "Benchmark.Verify" $ do
  describe "verify" $ do
    it "returns Match for identical responses" $ do
      let body = encode (Map.fromList [("key" :: Text, "value" :: Text)])
      let r1 = makeResponseWithBody 200 body
      let r2 = makeResponseWithBody 200 body
      verify 0.0 Nothing r1 r2 `shouldBe` Match

    it "returns StatusMismatch for different status codes" $ do
      let r1 = makeResponseWithBody 200 ""
      let r2 = makeResponseWithBody 404 ""
      case verify 0.0 Nothing r1 r2 of
        StatusMismatch a b -> do
          a `shouldBe` 200
          b `shouldBe` 404
        _ -> expectationFailure "Expected StatusMismatch"

    it "returns Match for both empty bodies" $ do
      let r1 = TestingResponse (Nanoseconds 1000) 200 Nothing Nothing
      let r2 = TestingResponse (Nanoseconds 2000) 200 Nothing Nothing
      verify 0.0 Nothing r1 r2 `shouldBe` Match

    it "returns InvalidJSON when one body is empty" $ do
      let body = encode (Map.fromList [("key" :: Text, "value" :: Text)])
      let r1 = makeResponseWithBody 200 body
      let r2 = TestingResponse (Nanoseconds 2000) 200 Nothing Nothing
      case verify 0.0 Nothing r1 r2 of
        InvalidJSON _ -> pure ()
        _ -> expectationFailure "Expected InvalidJSON"

    it "returns BodyMismatch for different JSON" $ do
      let body1 = encode (Map.fromList [("key" :: Text, "value1" :: Text)])
      let body2 = encode (Map.fromList [("key" :: Text, "value2" :: Text)])
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      case verify 0.0 Nothing r1 r2 of
        BodyMismatch _ -> pure ()
        _ -> expectationFailure "Expected BodyMismatch"

    it "returns Match for semantically equal JSON (different formatting)" $ do
      let body1 = "{\"a\":1,\"b\":2}"
      let body2 = "{\"b\":2,\"a\":1}"
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      verify 0.0 Nothing r1 r2 `shouldBe` Match

  describe "verify with float tolerance" $ do
    it "returns Match when floats are within tolerance" $ do
      let body1 = "{\"predictions\":[[1.0,2.0],[3.0,4.0]]}"
      let body2 = "{\"predictions\":[[1.0001,2.0001],[3.0001,4.0001]]}"
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      verify 0.001 Nothing r1 r2 `shouldBe` Match

    it "returns BodyMismatch when floats exceed tolerance" $ do
      let body1 = "{\"predictions\":[[1.0,2.0]]}"
      let body2 = "{\"predictions\":[[1.1,2.0]]}"
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      case verify 0.001 Nothing r1 r2 of
        BodyMismatch _ -> pure ()
        _ -> expectationFailure "Expected BodyMismatch"

    it "returns BodyMismatch when arrays have different lengths" $ do
      let body1 = "{\"data\":[[1.0,2.0],[3.0,4.0]]}"
      let body2 = "{\"data\":[[1.0,2.0]]}"
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      case verify 1.0 Nothing r1 r2 of
        BodyMismatch _ -> pure ()
        _ -> expectationFailure "Expected BodyMismatch"

    it "exact match still works with zero tolerance" $ do
      let body1 = "{\"x\":0.123456789}"
      let body2 = "{\"x\":0.123456789}"
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      verify 0.0 Nothing r1 r2 `shouldBe` Match

    it "returns Match for nested objects with floats within tolerance" $ do
      let body1 = "{\"result\":{\"score\":0.95,\"confidence\":0.80}}"
      let body2 = "{\"result\":{\"score\":0.9505,\"confidence\":0.8005}}"
      let r1 = makeResponseWithBody 200 body1
      let r2 = makeResponseWithBody 200 body2
      verify 0.001 Nothing r1 r2 `shouldBe` Match
