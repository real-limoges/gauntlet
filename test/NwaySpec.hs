module NwaySpec (nwaySpec) where

import Benchmark.Config (validateNwayConfig)
import Benchmark.Report.Markdown (markdownNwayReport)
import Benchmark.Types
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text (Text)
import Data.Text qualified as T
import Runner.Nway (allPairComparisons)
import Test.Hspec
import TestHelpers (makeResult, mockStats)

nwaySpec :: Spec
nwaySpec = describe "N-Way Benchmarking" $ do
  describe "NwayConfig JSON parsing" $ do
    it "parses a valid 3-target config" $ do
      let json =
            LBS8.pack $
              unlines
                [ "{"
                , "  \"targets\": ["
                , "    {\"name\": \"prod\", \"url\": \"http://prod:8080\"},"
                , "    {\"name\": \"staging\", \"url\": \"http://staging:8080\"},"
                , "    {\"name\": \"dev\", \"url\": \"http://dev:8080\"}"
                , "  ],"
                , "  \"settings\": {"
                , "    \"iterations\": 100,"
                , "    \"concurrency\": 4,"
                , "    \"secrets\": \"token.txt\""
                , "  },"
                , "  \"payloads\": ["
                , "    {\"name\": \"get-users\", \"method\": \"GET\", \"path\": \"/api/users\"}"
                , "  ]"
                , "}"
                ]
      case eitherDecode json :: Either String NwayConfig of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right cfg -> do
          length (nwayTargets cfg) `shouldBe` 3
          case nwayTargets cfg of
            (t : _) -> do
              targetName t `shouldBe` "prod"
              targetUrl t `shouldBe` "http://prod:8080"
            [] -> expectationFailure "Expected at least one target"
          iterations (nwaySettings cfg) `shouldBe` 100

    it "parses targets with optional branch field" $ do
      let json =
            LBS8.pack $
              unlines
                [ "{"
                , "  \"targets\": ["
                , "    {\"name\": \"main\", \"url\": \"http://localhost:8080\", \"branch\": \"main\"},"
                , "    {\"name\": \"feature\", \"url\": \"http://localhost:8080\", \"branch\": \"feature-x\"}"
                , "  ],"
                , "  \"settings\": {\"iterations\": 10, \"concurrency\": 1, \"secrets\": \"s.txt\"},"
                , "  \"payloads\": [{\"name\": \"p\", \"method\": \"GET\", \"path\": \"/\"}]"
                , "}"
                ]
      case eitherDecode json :: Either String NwayConfig of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right cfg -> case nwayTargets cfg of
          (t1 : t2 : _) -> do
            targetBranch t1 `shouldBe` Just "main"
            targetBranch t2 `shouldBe` Just "feature-x"
          _ -> expectationFailure "Expected at least two targets"

    it "parses targets without branch field as Nothing" $ do
      let json =
            LBS8.pack $
              unlines
                [ "{"
                , "  \"targets\": ["
                , "    {\"name\": \"a\", \"url\": \"http://a:8080\"},"
                , "    {\"name\": \"b\", \"url\": \"http://b:8080\"}"
                , "  ],"
                , "  \"settings\": {\"iterations\": 10, \"concurrency\": 1, \"secrets\": \"s.txt\"},"
                , "  \"payloads\": [{\"name\": \"p\", \"method\": \"GET\", \"path\": \"/\"}]"
                , "}"
                ]
      case eitherDecode json :: Either String NwayConfig of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right cfg -> case nwayTargets cfg of
          (t : _) -> targetBranch t `shouldBe` Nothing
          [] -> expectationFailure "Expected at least one target"

  describe "validateNwayConfig" $ do
    it "rejects configs with fewer than 2 targets" $ do
      let cfg = makeNwayConfig [NamedTarget "a" "http://a" Nothing]
      case validateNwayConfig cfg of
        Left (ConfigValidationError msg) ->
          msg `shouldBe` "Must have at least 2 targets"
        _ -> expectationFailure "Expected ConfigValidationError"

    it "rejects empty payloads" $ do
      let cfg =
            NwayConfig
              { nwayTargets =
                  [ NamedTarget "a" "http://a" Nothing
                  , NamedTarget "b" "http://b" Nothing
                  ]
              , nwaySettings = defaultSettings
              , nwayPayloads = []
              }
      case validateNwayConfig cfg of
        Left (ConfigValidationError msg) ->
          msg `shouldBe` "No payloads defined in config"
        _ -> expectationFailure "Expected ConfigValidationError"

    it "rejects invalid HTTP methods" $ do
      let cfg =
            NwayConfig
              { nwayTargets =
                  [ NamedTarget "a" "http://a" Nothing
                  , NamedTarget "b" "http://b" Nothing
                  ]
              , nwaySettings = defaultSettings
              , nwayPayloads = [PayloadSpec "test" "INVALID" "/path" Nothing Nothing Nothing]
              }
      case validateNwayConfig cfg of
        Left (ConfigValidationError msg) ->
          msg `shouldContain` "Invalid HTTP method"
        _ -> expectationFailure "Expected ConfigValidationError"

    it "accepts valid 2-target config" $ do
      let cfg = makeNwayConfig [NamedTarget "a" "http://a" Nothing, NamedTarget "b" "http://b" Nothing]
      validateNwayConfig cfg `shouldBe` Right cfg

  describe "allPairComparisons" $ do
    it "produces 1 pair for 2 targets" $ do
      let stats = makePairStats 2
      length (allPairComparisons stats) `shouldBe` 1

    it "produces 3 pairs for 3 targets" $ do
      let stats = makePairStats 3
      length (allPairComparisons stats) `shouldBe` 3

    it "produces 6 pairs for 4 targets" $ do
      let stats = makePairStats 4
      length (allPairComparisons stats) `shouldBe` 6

    it "produces 0 pairs for empty list" $ do
      allPairComparisons [] `shouldBe` []

    it "produces 0 pairs for single target" $ do
      allPairComparisons [("only", mockStats 10 1, makeTimings 10)] `shouldBe` []

    it "includes correct target names in pairs" $ do
      let stats =
            [ ("alpha", mockStats 10 1, makeTimings 10)
            , ("beta", mockStats 20 2, makeTimings 20)
            , ("gamma", mockStats 30 3, makeTimings 30)
            ]
      let pairs = allPairComparisons stats
      let pairNames = [(a, b) | (a, b, _) <- pairs]
      pairNames `shouldBe` [("alpha", "beta"), ("alpha", "gamma"), ("beta", "gamma")]

  describe "markdownNwayReport" $ do
    it "contains ranking table header" $ do
      let triples = [("prod", mockStats 10 1, makeTimings 10), ("staging", mockStats 20 2, makeTimings 20)]
          namedStats = [(n, s) | (n, s, _) <- triples]
          pairs = allPairComparisons triples
          md = markdownNwayReport namedStats pairs
      T.isInfixOf "Ranking" md `shouldBe` True
      T.isInfixOf "| # | Target |" md `shouldBe` True

    it "contains target names" $ do
      let triples = [("prod", mockStats 10 1, makeTimings 10), ("staging", mockStats 20 2, makeTimings 20)]
          namedStats = [(n, s) | (n, s, _) <- triples]
          pairs = allPairComparisons triples
          md = markdownNwayReport namedStats pairs
      T.isInfixOf "prod" md `shouldBe` True
      T.isInfixOf "staging" md `shouldBe` True

-- Helpers

defaultSettings :: Settings
defaultSettings =
  Settings
    { iterations = 10
    , concurrency = 2
    , secrets = "secrets.txt"
    , maxConnections = Nothing
    , connIdleTimeout = Nothing
    , requestTimeout = Nothing
    , retry = Nothing
    , warmup = Nothing
    , logLevel = Nothing
    , tempo = Nothing
    , healthCheckPath = Nothing
    , healthCheckTimeout = Nothing
    , floatTolerance = Nothing
    , compareFields = Nothing
    , ignoreFields = Nothing
    , verifyIterations = Nothing
    }

makeNwayConfig :: [NamedTarget] -> NwayConfig
makeNwayConfig ts =
  NwayConfig
    { nwayTargets = ts
    , nwaySettings = defaultSettings
    , nwayPayloads = [PayloadSpec "test" "GET" "/api/test" Nothing Nothing Nothing]
    }

makePairStats :: Int -> [(Text, BenchmarkStats, [TestingResponse])]
makePairStats n =
  [ ( T.pack ("target-" ++ show i)
    , mockStats (fromIntegral i * 10) (fromIntegral i)
    , makeTimings (fromIntegral i * 10)
    )
  | i <- [1 .. n]
  ]

makeTimings :: Integer -> [TestingResponse]
makeTimings baseNs = [makeResult (baseNs * 1_000_000 + offset) | offset <- [0, 100_000 .. 900_000]]
