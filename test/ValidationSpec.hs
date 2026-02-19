module ValidationSpec (validationSpec) where

import Benchmark.Types
  ( FieldAssertion (..),
    TestingResponse (..),
    ValidationError (..),
    ValidationSpec (..),
    ValidationSummary (..),
  )
import Benchmark.Validation (validateResponse, validateResponses)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Hspec
import TestHelpers (makeResponseWithBody)

-- Helpers -------------------------------------------------------------------

makeSpec :: Maybe Int -> [(Text, FieldAssertion)] -> ValidationSpec
makeSpec status fields =
  ValidationSpec
    { validateStatus = status,
      validateFields = if null fields then Nothing else Just (Map.fromList fields)
    }

-- Spec ----------------------------------------------------------------------

validationSpec :: Spec
validationSpec = describe "Benchmark.Validation" $ do
  describe "validateResponse" $ do
    describe "status code" $ do
      it "passes when status matches" $ do
        let resp = makeResponseWithBody 200 (encode (object ["id" .= (1 :: Int)]))
            spec = makeSpec (Just 200) []
        validateResponse spec resp `shouldBe` []

      it "fails when status mismatches" $ do
        let resp = makeResponseWithBody 404 (encode (object ["id" .= (1 :: Int)]))
            spec = makeSpec (Just 200) []
        validateResponse spec resp `shouldBe` [StatusCodeMismatch 200 404]

      it "ignores status when not specified" $ do
        let resp = makeResponseWithBody 500 (encode (object ["id" .= (1 :: Int)]))
            spec = makeSpec Nothing []
        validateResponse spec resp `shouldBe` []

    describe "field presence" $ do
      it "passes when field is present" $ do
        let body = encode (object ["id" .= (42 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldPresent)]
        validateResponse spec resp `shouldBe` []

      it "fails when field is absent" $ do
        let body = encode (object ["name" .= ("Alice" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldPresent)]
        validateResponse spec resp `shouldBe` [FieldNotFound "$.id"]

      it "handles nested path" $ do
        let body = encode (object ["user" .= object ["id" .= (1 :: Int)]])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("user.id", FieldPresent)]
        validateResponse spec resp `shouldBe` []

      it "fails for missing intermediate key" $ do
        let body = encode (object ["name" .= ("Alice" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("user.id", FieldPresent)]
        validateResponse spec resp `shouldBe` [FieldNotFound "user.id"]

    describe "field equality" $ do
      it "passes when value matches" $ do
        let body = encode (object ["status" .= ("active" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.status", FieldEq (String "active"))]
        validateResponse spec resp `shouldBe` []

      it "fails when value differs" $ do
        let body = encode (object ["status" .= ("inactive" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.status", FieldEq (String "active"))]
        case validateResponse spec resp of
          [FieldValueMismatch path _expected _actual] ->
            path `shouldBe` "$.status"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

      it "passes for numeric equality" $ do
        let body = encode (object ["count" .= (5 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("count", FieldEq (Number 5))]
        validateResponse spec resp `shouldBe` []

    describe "body handling" $ do
      it "returns BodyNotJSON when body is Nothing and fields are checked" $ do
        let resp = makeResponseWithBody 200 ""
            resp' = resp {respBody = Nothing}
            spec = makeSpec Nothing [("$.id", FieldPresent)]
        validateResponse spec resp' `shouldBe` [BodyNotJSON]

      it "returns BodyNotJSON for non-JSON body" $ do
        let resp = makeResponseWithBody 200 "not json"
            spec = makeSpec Nothing [("$.id", FieldPresent)]
        validateResponse spec resp `shouldBe` [BodyNotJSON]

      it "allows no-field spec with null body (no errors)" $ do
        let resp = makeResponseWithBody 200 "not json"
            spec = makeSpec (Just 200) []
        validateResponse spec resp `shouldBe` []

    describe "combined checks" $ do
      it "returns multiple errors for multiple failures" $ do
        let body = encode (object ["name" .= ("Alice" :: Text)])
            resp = makeResponseWithBody 404 body
            spec = makeSpec (Just 200) [("$.id", FieldPresent), ("$.status", FieldEq (String "active"))]
            errs = validateResponse spec resp
        length errs `shouldBe` 3

  describe "validateResponses" $ do
    it "counts correctly across multiple responses" $ do
      let good = makeResponseWithBody 200 (encode (object ["id" .= (1 :: Int)]))
          bad = makeResponseWithBody 404 (encode (object ["id" .= (1 :: Int)]))
          spec = makeSpec (Just 200) []
          summary = validateResponses spec [good, good, bad]
      totalValidated summary `shouldBe` 3
      totalFailed summary `shouldBe` 1

    it "returns zero failures for all-passing responses" $ do
      let resp = makeResponseWithBody 200 (encode (object ["id" .= (1 :: Int)]))
          spec = makeSpec (Just 200) [("$.id", FieldPresent)]
          summary = validateResponses spec [resp, resp]
      totalFailed summary `shouldBe` 0

    it "handles empty response list" $ do
      let spec = makeSpec (Just 200) []
          summary = validateResponses spec []
      totalValidated summary `shouldBe` 0
      totalFailed summary `shouldBe` 0
