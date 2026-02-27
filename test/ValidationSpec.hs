module ValidationSpec (validationSpec) where

import Benchmark.Types
  ( FieldAssertion (..)
  , TestingResponse (..)
  , ValidationError (..)
  , ValidationSpec (..)
  , ValidationSummary (..)
  )
import Benchmark.Validation (validateResponse, validateResponses)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Test.Hspec
import TestHelpers (makeResponseWithBody)

-- Helpers -------------------------------------------------------------------

makeSpec :: Maybe Int -> [(Text, FieldAssertion)] -> ValidationSpec
makeSpec status fields =
  ValidationSpec
    { validateStatus = status
    , validateFields = if null fields then Nothing else Just (Map.fromList fields)
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
      it "returns BodyAbsent when body is Nothing and fields are checked" $ do
        let resp = makeResponseWithBody 200 ""
            resp' = resp {respBody = Nothing}
            spec = makeSpec Nothing [("$.id", FieldPresent)]
        validateResponse spec resp' `shouldBe` [BodyAbsent]

      it "returns BodyInvalidJSON for non-JSON body" $ do
        let resp = makeResponseWithBody 200 "not json"
            spec = makeSpec Nothing [("$.id", FieldPresent)]
        validateResponse spec resp `shouldBe` [BodyInvalidJSON]

      it "allows no-field spec with null body (no errors)" $ do
        let resp = makeResponseWithBody 200 "not json"
            spec = makeSpec (Just 200) []
        validateResponse spec resp `shouldBe` []

    describe "FieldNull" $ do
      it "passes when field is null" $ do
        let body = encode (object ["id" .= Null])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldNull)]
        validateResponse spec resp `shouldBe` []

      it "fails when field is not null" $ do
        let body = encode (object ["id" .= (1 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldNull)]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.id"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

    describe "FieldNotNull" $ do
      it "passes when field is not null" $ do
        let body = encode (object ["id" .= (1 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldNotNull)]
        validateResponse spec resp `shouldBe` []

      it "fails when field is null" $ do
        let body = encode (object ["id" .= Null])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldNotNull)]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.id"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

    describe "FieldType" $ do
      it "passes when type matches" $ do
        let body = encode (object ["name" .= ("Alice" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.name", FieldType "string")]
        validateResponse spec resp `shouldBe` []

      it "fails when type differs" $ do
        let body = encode (object ["count" .= (5 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.count", FieldType "string")]
        case validateResponse spec resp of
          [FieldValueMismatch path (String "string") (String "number")] -> path `shouldBe` "$.count"
          other -> expectationFailure $ "Expected FieldValueMismatch string/number, got: " ++ show other

      it "passes for array type" $ do
        let body = encode (object ["items" .= ([] :: [Int])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.items", FieldType "array")]
        validateResponse spec resp `shouldBe` []

    describe "FieldMatches" $ do
      it "passes when string matches regex" $ do
        let body = encode (object ["email" .= ("user@example.com" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.email", FieldMatches "^[^@]+@[^@]+$")]
        validateResponse spec resp `shouldBe` []

      it "fails when string does not match regex" $ do
        let body = encode (object ["email" .= ("not-an-email" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.email", FieldMatches "^[^@]+@[^@]+$")]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.email"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

      it "fails with FieldValueMismatch when field is not a string" $ do
        let body = encode (object ["id" .= (42 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.id", FieldMatches "[0-9]+")]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.id"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

    describe "FieldRange" $ do
      it "passes when number is within range" $ do
        let body = encode (object ["age" .= (25 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.age", FieldRange (Just 0) (Just 150))]
        validateResponse spec resp `shouldBe` []

      it "fails when number is below min" $ do
        let body = encode (object ["age" .= ((-1) :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.age", FieldRange (Just (0 :: Scientific)) Nothing)]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.age"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

      it "fails when number is above max" $ do
        let body = encode (object ["pct" .= (101 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.pct", FieldRange Nothing (Just (100 :: Scientific)))]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.pct"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

      it "passes with only min bound" $ do
        let body = encode (object ["n" .= (5 :: Int)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.n", FieldRange (Just 0) Nothing)]
        validateResponse spec resp `shouldBe` []

    describe "ArrayLength" $ do
      it "passes when array has correct length" $ do
        let body = encode (object ["tags" .= (["a", "b", "c"] :: [Text])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.tags", ArrayLength 3)]
        validateResponse spec resp `shouldBe` []

      it "fails when array has wrong length" $ do
        let body = encode (object ["tags" .= (["a", "b"] :: [Text])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.tags", ArrayLength 3)]
        case validateResponse spec resp of
          [FieldValueMismatch path (Number 3) (Number 2)] -> path `shouldBe` "$.tags"
          other -> expectationFailure $ "Expected FieldValueMismatch 3/2, got: " ++ show other

      it "fails with FieldValueMismatch when field is not an array" $ do
        let body = encode (object ["tags" .= ("not-array" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.tags", ArrayLength 1)]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.tags"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

    describe "ArrayContains" $ do
      it "passes when value is in array" $ do
        let body = encode (object ["roles" .= (["admin", "user"] :: [Text])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.roles", ArrayContains (String "admin"))]
        validateResponse spec resp `shouldBe` []

      it "fails when value is not in array" $ do
        let body = encode (object ["roles" .= (["user"] :: [Text])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.roles", ArrayContains (String "admin"))]
        case validateResponse spec resp of
          [FieldValueMismatch path (String "admin") (String "<not in array>")] -> path `shouldBe` "$.roles"
          other -> expectationFailure $ "Expected FieldValueMismatch admin/<not in array>, got: " ++ show other

      it "fails with FieldValueMismatch when field is not an array" $ do
        let body = encode (object ["roles" .= ("admin" :: Text)])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("$.roles", ArrayContains (String "admin"))]
        case validateResponse spec resp of
          [FieldValueMismatch path _ _] -> path `shouldBe` "$.roles"
          other -> expectationFailure $ "Expected FieldValueMismatch, got: " ++ show other

    describe "array index traversal" $ do
      it "indexes into arrays with numeric path segments" $ do
        let body = encode (object ["items" .= ([10, 20, 30] :: [Int])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("items.1", FieldEq (Number 20))]
        validateResponse spec resp `shouldBe` []

      it "returns FieldNotFound for out-of-bounds index" $ do
        let body = encode (object ["items" .= ([10] :: [Int])])
            resp = makeResponseWithBody 200 body
            spec = makeSpec Nothing [("items.5", FieldPresent)]
        validateResponse spec resp `shouldBe` [FieldNotFound "items.5"]

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

    it "caps error collection at 50 failing responses" $ do
      let resp = makeResponseWithBody 404 (encode (object ["id" .= (1 :: Int)]))
          spec = makeSpec (Just 200) []
          summary = validateResponses spec (replicate 100 resp)
      totalFailed summary `shouldBe` 100
      length (validationErrors summary) `shouldBe` 50
