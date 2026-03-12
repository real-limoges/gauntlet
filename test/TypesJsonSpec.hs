-- | Tests for JSON serialization of benchmark types.
module TypesJsonSpec (typesJsonSpec) where

import Benchmark.Types
import Data.Aeson (ToJSON (..), decode, encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map
import TastyCompat (Expectation, shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

typesJsonSpec :: TestTree
typesJsonSpec =
  testGroup
    "JSON instances"
    [ testGroup
        "FieldAssertion round-trip"
        [ testCase "FieldPresent" $
            roundTrip FieldPresent
        , testCase "FieldEq Number" $
            roundTrip (FieldEq (toJSON (42 :: Int)))
        , testCase "FieldEq String" $
            roundTrip (FieldEq (toJSON ("hello" :: String)))
        , testCase "FieldEq Null" $
            roundTrip (FieldEq (toJSON ()))
        , testCase "FieldNull" $
            roundTrip FieldNull
        , testCase "FieldNotNull" $
            roundTrip FieldNotNull
        , testCase "FieldType" $
            roundTrip (FieldType "string")
        , testCase "FieldMatches" $
            roundTrip (FieldMatches "^[a-z]+$")
        , testCase "FieldRange both bounds" $
            roundTrip (FieldRange (Just 1) (Just 10))
        , testCase "FieldRange upper only" $
            roundTrip (FieldRange Nothing (Just 5))
        , testCase "FieldRange lower only" $
            roundTrip (FieldRange (Just 0) Nothing)
        , testCase "FieldRange no bounds" $
            roundTrip (FieldRange Nothing Nothing)
        , testCase "ArrayLength" $
            roundTrip (ArrayLength 5)
        , testCase "ArrayContains" $
            roundTrip (ArrayContains (toJSON ("foo" :: String)))
        ]
    , testGroup
        "FieldAssertion error cases"
        [ testCase "{\"present\": false} fails to decode" $
            (decode (LBS8.pack "{\"present\": false}") :: Maybe FieldAssertion) `shouldBe` Nothing
        , testCase "{\"null\": false} fails to decode" $
            (decode (LBS8.pack "{\"null\": false}") :: Maybe FieldAssertion) `shouldBe` Nothing
        , testCase "{\"notNull\": false} fails to decode" $
            (decode (LBS8.pack "{\"notNull\": false}") :: Maybe FieldAssertion) `shouldBe` Nothing
        , testCase "{} (no recognized key) fails to decode" $
            (decode (LBS8.pack "{}") :: Maybe FieldAssertion) `shouldBe` Nothing
        ]
    , testGroup
        "LogLevel round-trip"
        [ testCase "Debug" $ roundTripLogLevel Debug "\"debug\""
        , testCase "Info" $ roundTripLogLevel Info "\"info\""
        , testCase "Warning" $ roundTripLogLevel Warning "\"warning\""
        , testCase "Error" $ roundTripLogLevel Error "\"error\""
        , testCase "invalid string fails to decode" $
            (decode (LBS8.pack "\"trace\"") :: Maybe LogLevel) `shouldBe` Nothing
        ]
    , testGroup
        "NwayConfig FromJSON"
        [ testCase "decodes valid config" $ do
            let json =
                  LBS8.pack $
                    unlines
                      [ "{"
                      , "  \"targets\": [{\"name\": \"a\", \"url\": \"http://x\"}],"
                      , "  \"settings\": {\"iterations\": 10, \"concurrency\": 2, \"secrets\": \"s.txt\"},"
                      , "  \"payloads\": [{\"name\": \"p\", \"method\": \"GET\", \"path\": \"/\"}]"
                      , "}"
                      ]
            (decode json :: Maybe NwayConfig) `shouldSatisfy` isJust
        , testCase "missing required key fails" $ do
            let json = LBS8.pack "{\"targets\": []}"
            (decode json :: Maybe NwayConfig) `shouldBe` Nothing
        ]
    , testGroup
        "NamedTarget FromJSON"
        [ testCase "decodes with remapped keys" $ do
            let json = LBS8.pack "{\"name\": \"alpha\", \"url\": \"http://x\", \"branch\": \"main\"}"
            case decode json of
              Just nt -> do
                targetName nt `shouldBe` "alpha"
                targetUrl nt `shouldBe` "http://x"
                targetBranch nt `shouldBe` Just "main"
              Nothing -> assertFailure "failed to decode NamedTarget"
        , testCase "branch is optional" $ do
            let json = LBS8.pack "{\"name\": \"a\", \"url\": \"http://x\"}"
            case decode json of
              Just nt -> targetBranch nt `shouldBe` Nothing
              Nothing -> assertFailure "failed to decode NamedTarget without branch"
        ]
    , testGroup
        "PayloadSpec FromJSON"
        [ testCase "decodes with remapped keys" $ do
            let json = LBS8.pack "{\"name\": \"test\", \"method\": \"POST\", \"path\": \"/api\"}"
            case decode json of
              Just ps -> do
                specName ps `shouldBe` "test"
                specMethod ps `shouldBe` "POST"
                specPath ps `shouldBe` "/api"
              Nothing -> assertFailure "failed to decode PayloadSpec"
        ]
    , testGroup
        "ValidationSpec round-trip"
        [ testCase "both fields present" $ do
            let spec = ValidationSpec (Just 200) (Just (Map.singleton "$.id" FieldPresent))
            (decode (encode spec) :: Maybe ValidationSpec) `shouldBe` Just spec
        , testCase "missing status decodes to Nothing" $ do
            let json = LBS8.pack "{\"fields\": {\"$.id\": {\"present\": true}}}"
            case (decode json :: Maybe ValidationSpec) of
              Just vs -> validateStatus vs `shouldBe` Nothing
              Nothing -> assertFailure "failed to decode ValidationSpec"
        , testCase "missing fields decodes to Nothing" $ do
            let json = LBS8.pack "{\"status\": 200}"
            case (decode json :: Maybe ValidationSpec) of
              Just vs -> validateFields vs `shouldBe` Nothing
              Nothing -> assertFailure "failed to decode ValidationSpec"
        ]
    ]

-- Helpers

roundTrip :: FieldAssertion -> Expectation
roundTrip fa = (decode (encode fa) :: Maybe FieldAssertion) `shouldBe` Just fa

roundTripLogLevel :: LogLevel -> LBS8.ByteString -> Expectation
roundTripLogLevel level expectedJson = do
  encode level `shouldBe` expectedJson
  (decode expectedJson :: Maybe LogLevel) `shouldBe` Just level

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
