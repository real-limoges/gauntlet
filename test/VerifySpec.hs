module VerifySpec (verifySpec) where

import Benchmark.Types (Nanoseconds (..), TestingResponse (..), VerificationResult (..))
import Benchmark.Verify (verify, verifyWithNetworkCheck)
import Data.Aeson (encode)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers

verifySpec :: TestTree
verifySpec =
  testGroup
    "Benchmark.Verify"
    [ testGroup
        "verify"
        [ testCase "returns Match for identical responses" $ do
            let body = encode (Map.fromList [("key" :: Text, "value" :: Text)])
            let r1 = makeResponseWithBody 200 body
            let r2 = makeResponseWithBody 200 body
            verify 0.0 Nothing Nothing r1 r2 `shouldBe` Match
        , testCase "returns StatusMismatch for different status codes" $ do
            let r1 = makeResponseWithBody 200 ""
            let r2 = makeResponseWithBody 404 ""
            case verify 0.0 Nothing Nothing r1 r2 of
              StatusMismatch a b -> do
                a `shouldBe` 200
                b `shouldBe` 404
              _ -> assertFailure "Expected StatusMismatch"
        , testCase "returns Match for both empty bodies" $ do
            let r1 = TestingResponse (Nanoseconds 1000) 200 Nothing Nothing
            let r2 = TestingResponse (Nanoseconds 2000) 200 Nothing Nothing
            verify 0.0 Nothing Nothing r1 r2 `shouldBe` Match
        , testCase "returns InvalidJSON when one body is empty" $ do
            let body = encode (Map.fromList [("key" :: Text, "value" :: Text)])
            let r1 = makeResponseWithBody 200 body
            let r2 = TestingResponse (Nanoseconds 2000) 200 Nothing Nothing
            case verify 0.0 Nothing Nothing r1 r2 of
              InvalidJSON _ -> pure ()
              _ -> assertFailure "Expected InvalidJSON"
        , testCase "returns BodyMismatch for different JSON" $ do
            let body1 = encode (Map.fromList [("key" :: Text, "value1" :: Text)])
            let body2 = encode (Map.fromList [("key" :: Text, "value2" :: Text)])
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            case verify 0.0 Nothing Nothing r1 r2 of
              BodyMismatch _ -> pure ()
              _ -> assertFailure "Expected BodyMismatch"
        , testCase "returns Match for semantically equal JSON (different formatting)" $ do
            let body1 = "{\"a\":1,\"b\":2}"
            let body2 = "{\"b\":2,\"a\":1}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            verify 0.0 Nothing Nothing r1 r2 `shouldBe` Match
        ]
    , testGroup
        "verify with float tolerance"
        [ testCase "returns Match when floats are within tolerance" $ do
            let body1 = "{\"predictions\":[[1.0,2.0],[3.0,4.0]]}"
            let body2 = "{\"predictions\":[[1.0001,2.0001],[3.0001,4.0001]]}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            verify 0.001 Nothing Nothing r1 r2 `shouldBe` Match
        , testCase "returns BodyMismatch when floats exceed tolerance" $ do
            let body1 = "{\"predictions\":[[1.0,2.0]]}"
            let body2 = "{\"predictions\":[[1.1,2.0]]}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            case verify 0.001 Nothing Nothing r1 r2 of
              BodyMismatch _ -> pure ()
              _ -> assertFailure "Expected BodyMismatch"
        , testCase "returns BodyMismatch when arrays have different lengths" $ do
            let body1 = "{\"data\":[[1.0,2.0],[3.0,4.0]]}"
            let body2 = "{\"data\":[[1.0,2.0]]}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            case verify 1.0 Nothing Nothing r1 r2 of
              BodyMismatch _ -> pure ()
              _ -> assertFailure "Expected BodyMismatch"
        , testCase "exact match still works with zero tolerance" $ do
            let body1 = "{\"x\":0.123456789}"
            let body2 = "{\"x\":0.123456789}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            verify 0.0 Nothing Nothing r1 r2 `shouldBe` Match
        , testCase "returns Match for nested objects with floats within tolerance" $ do
            let body1 = "{\"result\":{\"score\":0.95,\"confidence\":0.80}}"
            let body2 = "{\"result\":{\"score\":0.9505,\"confidence\":0.8005}}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            verify 0.001 Nothing Nothing r1 r2 `shouldBe` Match
        ]
    , testGroup
        "verify with ignoreFields"
        [ testCase "ignores specified top-level key in comparison" $ do
            let body1 = "{\"id\":\"abc\",\"value\":1}"
            let body2 = "{\"id\":\"xyz\",\"value\":1}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            verify 0.0 Nothing (Just ["id"]) r1 r2 `shouldBe` Match
        , testCase "still detects mismatch on non-ignored key" $ do
            let body1 = "{\"id\":\"abc\",\"value\":1}"
            let body2 = "{\"id\":\"xyz\",\"value\":2}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            case verify 0.0 Nothing (Just ["id"]) r1 r2 of
              BodyMismatch _ -> pure ()
              _ -> assertFailure "Expected BodyMismatch"
        , testCase "ignores specified nested key at any depth" $ do
            let body1 = "{\"meta\":{\"ts\":\"2024-01-01\"},\"data\":42}"
            let body2 = "{\"meta\":{\"ts\":\"2024-06-01\"},\"data\":42}"
            let r1 = makeResponseWithBody 200 body1
            let r2 = makeResponseWithBody 200 body2
            verify 0.0 Nothing (Just ["ts"]) r1 r2 `shouldBe` Match
        ]
    , testGroup
        "verifyWithNetworkCheck"
        [ testCase "returns NetworkError when primary has errorMessage" $ do
            let r1 = makeErrorResult "connection refused"
            let r2 = makeResponseWithBody 200 "{}"
            case verifyWithNetworkCheck 0.0 Nothing Nothing r1 r2 of
              NetworkError msg -> msg `shouldBe` "Primary request failed: connection refused"
              _ -> assertFailure "Expected NetworkError"
        , testCase "returns NetworkError when candidate has errorMessage" $ do
            let r1 = makeResponseWithBody 200 "{}"
            let r2 = makeErrorResult "timeout"
            case verifyWithNetworkCheck 0.0 Nothing Nothing r1 r2 of
              NetworkError msg -> msg `shouldBe` "Candidate request failed: timeout"
              _ -> assertFailure "Expected NetworkError"
        , testCase "delegates to verify when neither has errorMessage" $ do
            let body = "{\"ok\":true}"
            let r1 = makeResponseWithBody 200 body
            let r2 = makeResponseWithBody 200 body
            verifyWithNetworkCheck 0.0 Nothing Nothing r1 r2 `shouldBe` Match
        ]
    ]
