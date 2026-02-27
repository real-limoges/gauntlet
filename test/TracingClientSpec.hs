module TracingClientSpec (tracingClientSpec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Test.Hspec
import Tracing.Client (parseSearchResponse, parseTraceResponse)
import Tracing.Types (Nanoseconds (..), Span (..), TempoSearchResponse (..), Trace (..), TraceMetadata (..))

tracingClientSpec :: Spec
tracingClientSpec = describe "Tracing.Client" $ do
  describe "parseSearchResponse" $ do
    it "returns Right with trace IDs from valid JSON" $ do
      let body =
            encode $
              object
                [ "traces"
                    .= [ object
                           [ "traceID" .= ("abc123" :: T.Text)
                           , "rootServiceName" .= ("svc" :: T.Text)
                           , "rootTraceName" .= ("GET /" :: T.Text)
                           ]
                       ]
                ]
      case parseSearchResponse body of
        Right resp -> map metaTraceID (foundTraces resp) `shouldBe` ["abc123"]
        Left err -> expectationFailure $ "Expected Right, got: " ++ err

    it "returns Right with empty list when traces key is missing" $ do
      let body = encode $ object ["version" .= (1 :: Int)]
      case parseSearchResponse body of
        Right resp -> foundTraces resp `shouldBe` []
        Left err -> expectationFailure $ "Expected Right [], got: " ++ err

    it "returns Right with empty list when traces array is empty" $ do
      let body = encode $ object ["traces" .= ([] :: [Value])]
      case parseSearchResponse body of
        Right resp -> foundTraces resp `shouldBe` []
        Left err -> expectationFailure $ "Expected Right [], got: " ++ err

    it "returns Left on malformed JSON" $ do
      let body = LBS.fromStrict "not-valid-json"
      case parseSearchResponse body of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected Left for malformed JSON"

  describe "parseTraceResponse" $ do
    it "parses OTLP JSON and returns trace with spans" $ do
      let body = encode otlpPayload
      case parseTraceResponse "trace-1" body of
        Right trace -> do
          traceId trace `shouldBe` "trace-1"
          length (traceSpans trace) `shouldBe` 1
        Left err -> expectationFailure $ "Expected Right, got: " ++ err

    it "extracts span name correctly" $ do
      let body = encode otlpPayload
      case parseTraceResponse "trace-1" body of
        Right trace ->
          map spanName (traceSpans trace) `shouldBe` ["my-operation"]
        Left err -> expectationFailure $ "Expected Right, got: " ++ err

    it "computes span duration from start/end timestamps" $ do
      let body = encode otlpPayload
      case parseTraceResponse "trace-1" body of
        Right trace -> case traceSpans trace of
          (s : _) -> spanDurationNs s `shouldBe` Nanoseconds 1_000_000
          [] -> expectationFailure "Expected at least one span"
        Left err -> expectationFailure $ "Expected Right, got: " ++ err

    it "returns Right with no spans for empty batches" $ do
      let body = encode $ object ["batches" .= ([] :: [Value])]
      case parseTraceResponse "trace-2" body of
        Right trace -> traceSpans trace `shouldBe` []
        Left err -> expectationFailure $ "Expected Right [], got: " ++ err

    it "returns Left on malformed JSON" $ do
      let body = LBS.fromStrict "not-json"
      case parseTraceResponse "trace-x" body of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected Left for malformed JSON"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Minimal OTLP JSON payload with one span (start=1_000_000ns, end=2_000_000ns).
otlpPayload :: Value
otlpPayload =
  object
    [ "batches"
        .= [ object
               [ "resource"
                   .= object
                     [ "attributes"
                         .= [ object
                                [ "key" .= ("service.name" :: T.Text)
                                , "value" .= object ["stringValue" .= ("my-svc" :: T.Text)]
                                ]
                            ]
                     ]
               , "scopeSpans"
                   .= [ object
                          [ "spans"
                              .= [ object
                                     [ "spanId" .= ("span-abc" :: T.Text)
                                     , "name" .= ("my-operation" :: T.Text)
                                     , "startTimeUnixNano" .= ("1000000" :: T.Text)
                                     , "endTimeUnixNano" .= ("2000000" :: T.Text)
                                     ]
                                 ]
                          ]
                      ]
               ]
           ]
    ]
