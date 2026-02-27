module TypesJsonSpec (typesJsonSpec) where

import Benchmark.Types
import Data.Aeson (ToJSON (..), decode, encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map
import Test.Hspec

typesJsonSpec :: Spec
typesJsonSpec = describe "JSON instances" $ do
  describe "FieldAssertion round-trip" $ do
    it "FieldPresent" $
      roundTrip FieldPresent

    it "FieldEq Number" $
      roundTrip (FieldEq (toJSON (42 :: Int)))

    it "FieldEq String" $
      roundTrip (FieldEq (toJSON ("hello" :: String)))

    it "FieldEq Null" $
      roundTrip (FieldEq (toJSON ()))

    it "FieldNull" $
      roundTrip FieldNull

    it "FieldNotNull" $
      roundTrip FieldNotNull

    it "FieldType" $
      roundTrip (FieldType "string")

    it "FieldMatches" $
      roundTrip (FieldMatches "^[a-z]+$")

    it "FieldRange both bounds" $
      roundTrip (FieldRange (Just 1) (Just 10))

    it "FieldRange upper only" $
      roundTrip (FieldRange Nothing (Just 5))

    it "FieldRange lower only" $
      roundTrip (FieldRange (Just 0) Nothing)

    it "FieldRange no bounds" $
      roundTrip (FieldRange Nothing Nothing)

    it "ArrayLength" $
      roundTrip (ArrayLength 5)

    it "ArrayContains" $
      roundTrip (ArrayContains (toJSON ("foo" :: String)))

  describe "FieldAssertion error cases" $ do
    it "{\"present\": false} fails to decode" $
      (decode (LBS8.pack "{\"present\": false}") :: Maybe FieldAssertion) `shouldBe` Nothing

    it "{\"null\": false} fails to decode" $
      (decode (LBS8.pack "{\"null\": false}") :: Maybe FieldAssertion) `shouldBe` Nothing

    it "{\"notNull\": false} fails to decode" $
      (decode (LBS8.pack "{\"notNull\": false}") :: Maybe FieldAssertion) `shouldBe` Nothing

    it "{} (no recognized key) fails to decode" $
      (decode (LBS8.pack "{}") :: Maybe FieldAssertion) `shouldBe` Nothing

  describe "LogLevel round-trip" $ do
    it "Debug" $ roundTripLogLevel Debug "\"debug\""
    it "Info" $ roundTripLogLevel Info "\"info\""
    it "Warning" $ roundTripLogLevel Warning "\"warning\""
    it "Error" $ roundTripLogLevel Error "\"error\""

    it "invalid string fails to decode" $
      (decode (LBS8.pack "\"trace\"") :: Maybe LogLevel) `shouldBe` Nothing

  describe "NwayConfig FromJSON" $ do
    it "decodes valid config" $ do
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

    it "missing required key fails" $ do
      let json = LBS8.pack "{\"targets\": []}"
      (decode json :: Maybe NwayConfig) `shouldBe` Nothing

  describe "NamedTarget FromJSON" $ do
    it "decodes with remapped keys" $ do
      let json = LBS8.pack "{\"name\": \"alpha\", \"url\": \"http://x\", \"branch\": \"main\"}"
      case decode json of
        Just nt -> do
          targetName nt `shouldBe` "alpha"
          targetUrl nt `shouldBe` "http://x"
          targetBranch nt `shouldBe` Just "main"
        Nothing -> expectationFailure "failed to decode NamedTarget"

    it "branch is optional" $ do
      let json = LBS8.pack "{\"name\": \"a\", \"url\": \"http://x\"}"
      case decode json of
        Just nt -> targetBranch nt `shouldBe` Nothing
        Nothing -> expectationFailure "failed to decode NamedTarget without branch"

  describe "PayloadSpec FromJSON" $ do
    it "decodes with remapped keys" $ do
      let json = LBS8.pack "{\"name\": \"test\", \"method\": \"POST\", \"path\": \"/api\"}"
      case decode json of
        Just ps -> do
          specName ps `shouldBe` "test"
          specMethod ps `shouldBe` "POST"
          specPath ps `shouldBe` "/api"
        Nothing -> expectationFailure "failed to decode PayloadSpec"

    it "optional fields absent produces Nothing" $ do
      let json = LBS8.pack "{\"name\": \"t\", \"method\": \"GET\", \"path\": \"/\"}"
      case decode json of
        Just ps -> do
          specBody ps `shouldBe` Nothing
          specHeaders ps `shouldBe` Nothing
          specValidate ps `shouldBe` Nothing
        Nothing -> expectationFailure "failed to decode PayloadSpec"

  describe "ValidationSpec round-trip" $ do
    it "both fields present" $ do
      let spec = ValidationSpec (Just 200) (Just (Map.singleton "$.id" FieldPresent))
      (decode (encode spec) :: Maybe ValidationSpec) `shouldBe` Just spec

    it "missing status decodes to Nothing" $ do
      let json = LBS8.pack "{\"fields\": {\"$.id\": {\"present\": true}}}"
      case (decode json :: Maybe ValidationSpec) of
        Just vs -> validateStatus vs `shouldBe` Nothing
        Nothing -> expectationFailure "failed to decode ValidationSpec"

    it "missing fields decodes to Nothing" $ do
      let json = LBS8.pack "{\"status\": 200}"
      case (decode json :: Maybe ValidationSpec) of
        Just vs -> validateFields vs `shouldBe` Nothing
        Nothing -> expectationFailure "failed to decode ValidationSpec"

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
