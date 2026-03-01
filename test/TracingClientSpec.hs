module TracingClientSpec (tracingClientSpec) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Tracing.Client (parseSearchResponse, parseTraceResponse)
import Tracing.Types
  ( Nanoseconds (..)
  , Span (..)
  , SpanKind (..)
  , SpanStatus (..)
  , TempoSearchResponse (..)
  , Trace (..)
  , TraceMetadata (..)
  )

tracingClientSpec :: TestTree
tracingClientSpec =
  testGroup
    "Tracing.Client"
    [ testGroup
        "parseSearchResponse"
        [ testCase "returns Right with trace IDs from valid JSON" $ do
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
              Left err -> assertFailure $ "Expected Right, got: " ++ err
        , testCase "returns Right with empty list when traces key is missing" $ do
            let body = encode $ object ["version" .= (1 :: Int)]
            case parseSearchResponse body of
              Right resp -> foundTraces resp `shouldBe` []
              Left err -> assertFailure $ "Expected Right [], got: " ++ err
        , testCase "returns Right with empty list when traces array is empty" $ do
            let body = encode $ object ["traces" .= ([] :: [Value])]
            case parseSearchResponse body of
              Right resp -> foundTraces resp `shouldBe` []
              Left err -> assertFailure $ "Expected Right [], got: " ++ err
        , testCase "returns Left on malformed JSON" $ do
            let body = LBS.fromStrict "not-valid-json"
            case parseSearchResponse body of
              Left _ -> pure ()
              Right _ -> assertFailure "Expected Left for malformed JSON"
        ]
    , testGroup
        "parseTraceResponse"
        [ testCase "parses OTLP JSON and returns trace with spans" $ do
            let body = encode otlpPayload
            case parseTraceResponse "trace-1" body of
              Right trace -> do
                traceId trace `shouldBe` "trace-1"
                length (traceSpans trace) `shouldBe` 1
              Left err -> assertFailure $ "Expected Right, got: " ++ err
        , testCase "extracts span name correctly" $ do
            let body = encode otlpPayload
            case parseTraceResponse "trace-1" body of
              Right trace ->
                map spanName (traceSpans trace) `shouldBe` ["my-operation"]
              Left err -> assertFailure $ "Expected Right, got: " ++ err
        , testCase "computes span duration from start/end timestamps" $ do
            let body = encode otlpPayload
            case parseTraceResponse "trace-1" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanDurationNs s `shouldBe` Nanoseconds 1_000_000
                [] -> assertFailure "Expected at least one span"
              Left err -> assertFailure $ "Expected Right, got: " ++ err
        , testCase "returns Right with no spans for empty batches" $ do
            let body = encode $ object ["batches" .= ([] :: [Value])]
            case parseTraceResponse "trace-2" body of
              Right trace -> traceSpans trace `shouldBe` []
              Left err -> assertFailure $ "Expected Right [], got: " ++ err
        , testCase "returns Left on malformed JSON" $ do
            let body = LBS.fromStrict "not-json"
            case parseTraceResponse "trace-x" body of
              Left _ -> pure ()
              Right _ -> assertFailure "Expected Left for malformed JSON"
        ]
    , testGroup
        "parseNanoTime (via parseTraceResponse)"
        [ testCase "parses numeric timestamp values" $ do
            let body =
                  encode $
                    otlpPayloadWith
                      "startTimeUnixNano"
                      (Number (fromFloatDigits (5_000_000 :: Double)))
                      "endTimeUnixNano"
                      (Number (fromFloatDigits (6_000_000 :: Double)))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> do
                  spanStartTimeNs s `shouldBe` Nanoseconds 5_000_000
                  spanEndTimeNs s `shouldBe` Nanoseconds 6_000_000
                [] -> assertFailure "Expected at least one span"
              Left err -> assertFailure $ "Expected Right, got: " ++ err
        , testCase "defaults to 0 for malformed string timestamps" $ do
            let body = encode $ otlpPayloadWith "startTimeUnixNano" (String "not-a-number") "endTimeUnixNano" (String "also-bad")
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> do
                  spanStartTimeNs s `shouldBe` Nanoseconds 0
                  spanEndTimeNs s `shouldBe` Nanoseconds 0
                [] -> assertFailure "Expected at least one span"
              Left err -> assertFailure $ "Expected Right, got: " ++ err
        ]
    , testGroup
        "parseStatus (via parseTraceResponse)"
        [ testCase "parses status code 1 as StatusOk" $ do
            let body = encode $ otlpPayloadWithStatus (Just (object ["code" .= (1 :: Int)]))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanStatus s `shouldBe` StatusOk
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses status code 2 as StatusError" $ do
            let body = encode $ otlpPayloadWithStatus (Just (object ["code" .= (2 :: Int)]))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanStatus s `shouldBe` StatusError
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses missing status as StatusUnset" $ do
            -- The base otlpPayload has no status field
            let body = encode otlpPayload
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanStatus s `shouldBe` StatusUnset
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses unknown status code as StatusUnset" $ do
            let body = encode $ otlpPayloadWithStatus (Just (object ["code" .= (99 :: Int)]))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanStatus s `shouldBe` StatusUnset
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        ]
    , testGroup
        "parseSpanKind (via parseTraceResponse)"
        [ testCase "parses numeric kind 2 as SpanKindServer" $ do
            let body = encode $ otlpPayloadWithKind (Just (Number 2))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindServer
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses numeric kind 3 as SpanKindClient" $ do
            let body = encode $ otlpPayloadWithKind (Just (Number 3))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindClient
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses string SPAN_KIND_SERVER" $ do
            let body = encode $ otlpPayloadWithKind (Just (String "SPAN_KIND_SERVER"))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindServer
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses string SPAN_KIND_CONSUMER" $ do
            let body = encode $ otlpPayloadWithKind (Just (String "SPAN_KIND_CONSUMER"))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindConsumer
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses missing kind as SpanKindUnspecified" $ do
            -- Base payload has no kind
            let body = encode otlpPayload
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindUnspecified
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses unknown string kind as SpanKindUnspecified" $ do
            let body = encode $ otlpPayloadWithKind (Just (String "UNKNOWN_KIND"))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindUnspecified
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        , testCase "parses unknown numeric kind as SpanKindUnspecified" $ do
            let body = encode $ otlpPayloadWithKind (Just (Number 99))
            case parseTraceResponse "t" body of
              Right trace -> case traceSpans trace of
                (s : _) -> spanKind s `shouldBe` SpanKindUnspecified
                [] -> assertFailure "Expected span"
              Left err -> assertFailure err
        ]
    , testGroup
        "parseSearchResponse with startTimeUnixNano"
        [ testCase "parses non-zero startTimeUnixNano" $ do
            let body =
                  encode $
                    object
                      [ "traces"
                          .= [ object
                                 [ "traceID" .= ("abc" :: T.Text)
                                 , "rootServiceName" .= ("svc" :: T.Text)
                                 , "rootTraceName" .= ("op" :: T.Text)
                                 , "startTimeUnixNano" .= ("1000000000" :: T.Text)
                                 , "durationMs" .= (42 :: Int)
                                 ]
                             ]
                      ]
            case parseSearchResponse body of
              Right resp -> case foundTraces resp of
                (m : _) -> do
                  metaStartTimeUnixNano m `shouldBe` 1_000_000_000
                  metaDurationMs m `shouldBe` 42
                [] -> assertFailure "Expected trace metadata"
              Left err -> assertFailure err
        ]
    ]

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

-- | OTLP payload with custom timestamp value types for testing parseNanoTime.
otlpPayloadWith :: T.Text -> Value -> T.Text -> Value -> Value
otlpPayloadWith startKey startVal endKey endVal =
  object
    [ "batches"
        .= [ object
               [ "resource" .= object ["attributes" .= ([] :: [Value])]
               , "scopeSpans"
                   .= [ object
                          [ "spans"
                              .= [ object
                                     [ "spanId" .= ("s1" :: T.Text)
                                     , "name" .= ("op" :: T.Text)
                                     , Key.fromText startKey .= startVal
                                     , Key.fromText endKey .= endVal
                                     ]
                                 ]
                          ]
                      ]
               ]
           ]
    ]

-- | OTLP payload with a specific status object on the span.
otlpPayloadWithStatus :: Maybe Value -> Value
otlpPayloadWithStatus mStatus =
  object
    [ "batches"
        .= [ object
               [ "resource" .= object ["attributes" .= ([] :: [Value])]
               , "scopeSpans"
                   .= [ object
                          [ "spans"
                              .= [ addOptional "status" mStatus $
                                     object
                                       [ "spanId" .= ("s1" :: T.Text)
                                       , "name" .= ("op" :: T.Text)
                                       , "startTimeUnixNano" .= ("1000" :: T.Text)
                                       , "endTimeUnixNano" .= ("2000" :: T.Text)
                                       ]
                                 ]
                          ]
                      ]
               ]
           ]
    ]

-- | OTLP payload with a specific span kind value.
otlpPayloadWithKind :: Maybe Value -> Value
otlpPayloadWithKind mKind =
  object
    [ "batches"
        .= [ object
               [ "resource" .= object ["attributes" .= ([] :: [Value])]
               , "scopeSpans"
                   .= [ object
                          [ "spans"
                              .= [ addOptional "kind" mKind $
                                     object
                                       [ "spanId" .= ("s1" :: T.Text)
                                       , "name" .= ("op" :: T.Text)
                                       , "startTimeUnixNano" .= ("1000" :: T.Text)
                                       , "endTimeUnixNano" .= ("2000" :: T.Text)
                                       ]
                                 ]
                          ]
                      ]
               ]
           ]
    ]

-- | Add an optional key to a JSON object value.
addOptional :: T.Text -> Maybe Value -> Value -> Value
addOptional _ Nothing v = v
addOptional key (Just val) (Object o) = Object (o <> kvPair)
  where
    kvPair = case object [Key.fromText key .= val] of
      Object p -> p
      _ -> mempty
addOptional _ _ v = v
