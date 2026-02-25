{- |
Module      : Tracing.Client
Description : Grafana Tempo HTTP client
Stability   : experimental

Queries Grafana Tempo for distributed traces using TraceQL. Parses OTLP JSON
format responses to extract spans with timing and attribute data.
-}
module Tracing.Client (
    searchTraces,
    fetchTrace,
    fetchTracesForTimeRange,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Client (Manager, Request, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (statusCode)
import Text.Read (readMaybe)
import Tracing.Query (buildTraceQL)
import Tracing.Types

-- | Search for traces matching a TraceQL query within a time range.
searchTraces :: Manager -> TempoConfig -> TraceQuery -> IO (Either String TempoSearchResponse)
searchTraces mgr cfg query = do
    let traceQL = buildTraceQL query
        startSec = queryStartNs query `div` 1_000_000_000
        endSec = queryEndNs query `div` 1_000_000_000
        url =
            T.unpack (tempoUrl cfg)
                <> "/api/search"
                <> "?q="
                <> T.unpack (urlEncode traceQL)
                <> "&start="
                <> show startSec
                <> "&end="
                <> show endSec
                <> "&limit=10000"

    result <- doGet mgr cfg url
    case result of
        Left err -> return $ Left err
        Right body -> return $ parseSearchResponse body

-- | Fetch complete trace data by trace ID.
fetchTrace :: Manager -> TempoConfig -> Text -> IO (Either String Trace)
fetchTrace mgr cfg traceId = do
    let url = T.unpack (tempoUrl cfg) <> "/api/traces/" <> T.unpack traceId

    result <- doGet mgr cfg url
    case result of
        Left err -> return $ Left err
        Right body -> return $ parseTraceResponse traceId body

-- | Search for traces and fetch full details for each match.
fetchTracesForTimeRange ::
    Manager ->
    TempoConfig ->
    TraceQuery ->
    IO (Either String [Trace])
fetchTracesForTimeRange mgr cfg query = do
    searchResult <- searchTraces mgr cfg query
    case searchResult of
        Left err -> return $ Left err
        Right resp -> do
            traces <- mapM (fetchTrace mgr cfg . metaTraceID) (foundTraces resp)
            return $ Right $ concatMap (either (const []) pure) traces

{- | Perform HTTP GET request with error handling.

Returns Left on HTTP errors or non-2xx status codes instead of crashing.
-}
doGet :: Manager -> TempoConfig -> String -> IO (Either String LBS.ByteString)
doGet mgr cfg url = do
    result <- try $ do
        initialReq <- parseRequest url
        let req = addAuthHeader cfg initialReq
        httpLbs req mgr
    case result of
        Left (err :: SomeException) ->
            return $ Left $ "HTTP error: " <> show err
        Right resp -> do
            let code = statusCode (Client.responseStatus resp)
            if code >= 200 && code < 300
                then return $ Right $ responseBody resp
                else return $ Left $ "Tempo API error: HTTP " <> show code

addAuthHeader :: TempoConfig -> Request -> Request
addAuthHeader cfg req = case tempoAuthToken cfg of
    Nothing -> req
    Just token ->
        req
            { Client.requestHeaders =
                ("Authorization", "Bearer " <> encodeUtf8 token)
                    : Client.requestHeaders req
            }

urlEncode :: Text -> Text
urlEncode = T.concatMap encodeChar
  where
    encodeChar c
        | c == ' ' = "%20"
        | c == '"' = "%22"
        | c == '{' = "%7B"
        | c == '}' = "%7D"
        | c == '=' = "%3D"
        | c == '&' = "%26"
        | otherwise = T.singleton c

parseSearchResponse :: LBS.ByteString -> Either String TempoSearchResponse
parseSearchResponse body = do
    val <- eitherDecode body
    parseEither parseSearchJson val

parseSearchJson :: Value -> Parser TempoSearchResponse
parseSearchJson = withObject "TempoSearchResponse" $ \o -> do
    traces <- o .:? "traces" .!= []
    metas <- mapM parseTraceMetadata traces
    return $ TempoSearchResponse metas

parseTraceMetadata :: Value -> Parser TraceMetadata
parseTraceMetadata = withObject "TraceMetadata" $ \o -> do
    traceID <- o .: "traceID"
    rootServiceName <- o .:? "rootServiceName" .!= ""
    rootTraceName <- o .:? "rootTraceName" .!= ""
    startTimeStr <- o .:? "startTimeUnixNano" .!= "0"
    durationMs <- o .:? "durationMs" .!= 0
    -- Use readMaybe for safe parsing, defaulting to 0 on parse failure
    let startTimeNano = fromMaybe 0 (readMaybe startTimeStr)
    return
        TraceMetadata
            { metaTraceID = traceID
            , metaRootServiceName = rootServiceName
            , metaRootTraceName = rootTraceName
            , metaStartTimeUnixNano = startTimeNano
            , metaDurationMs = durationMs
            }

parseTraceResponse :: Text -> LBS.ByteString -> Either String Trace
parseTraceResponse traceId body = do
    val <- eitherDecode body
    parseEither (parseOTLPTrace traceId) val

parseOTLPTrace :: Text -> Value -> Parser Trace
parseOTLPTrace traceId = withObject "OTLPTrace" $ \o -> do
    batches <- o .:? "batches" .!= []
    spans <- concat <$> mapM parseBatch batches
    let totalDuration = case spans of
            [] -> 0
            _ -> maximum (map spanEndTimeNs spans) - minimum (map spanStartTimeNs spans)
    return
        Trace
            { traceId = traceId
            , traceSpans = spans
            , traceTotalDurationNs = totalDuration
            }

parseBatch :: Value -> Parser [Span]
parseBatch = withObject "Batch" $ \o -> do
    resource <- o .:? "resource"
    serviceName <- case resource of
        Nothing -> return ""
        Just r -> parseServiceName r
    scopeSpans <- o .:? "scopeSpans" .!= []
    concat <$> mapM (parseScopeSpans serviceName) scopeSpans

parseServiceName :: Value -> Parser Text
parseServiceName = withObject "Resource" $ \o -> do
    attrs <- o .:? "attributes" .!= []
    return $ findAttribute "service.name" attrs

parseScopeSpans :: Text -> Value -> Parser [Span]
parseScopeSpans serviceName = withObject "ScopeSpans" $ \o -> do
    spans <- o .:? "spans" .!= []
    mapM (parseSpan serviceName) spans

parseSpan :: Text -> Value -> Parser Span
parseSpan serviceName = withObject "Span" $ \o -> do
    spanId <- o .: "spanId"
    parentSpanId <- o .:? "parentSpanId"
    name <- o .: "name"
    kindVal <- o .:? "kind"
    startTimeStr <- o .: "startTimeUnixNano"
    endTimeStr <- o .: "endTimeUnixNano"
    statusObj <- o .:? "status"
    attrs <- o .:? "attributes" .!= []

    let startNs = parseNanoTime startTimeStr
        endNs = parseNanoTime endTimeStr
        status = parseStatus statusObj
        kind = parseSpanKind kindVal

    return
        Span
            { spanId = spanId
            , spanParentId = parentSpanId
            , spanName = name
            , spanServiceName = serviceName
            , spanKind = kind
            , spanStartTimeNs = startNs
            , spanEndTimeNs = endNs
            , spanDurationNs = endNs - startNs
            , spanStatus = status
            , spanAttributes = parseAttributes attrs
            }

{- | Parse nanosecond timestamp from JSON value.

Handles both string and numeric representations safely,
using readMaybe to avoid crashes on malformed input.
-}
parseNanoTime :: Value -> Nanoseconds
parseNanoTime (String s) = fromMaybe 0 (readMaybe (T.unpack s))
parseNanoTime (Number n) = round n
parseNanoTime _ = 0

parseStatus :: Maybe Value -> SpanStatus
parseStatus Nothing = StatusUnset
parseStatus (Just val) = case val of
    Object o -> case parseMaybe (.: "code") o of
        Just (1 :: Int) -> StatusOk
        Just 2 -> StatusError
        _ -> StatusUnset
    _ -> StatusUnset

-- | Parse span kind from Int or String (OTLP supports both formats).
parseSpanKind :: Maybe Value -> SpanKind
parseSpanKind Nothing = SpanKindUnspecified
parseSpanKind (Just (Number n)) = intToSpanKind (round n)
parseSpanKind (Just (String s)) = case s of
    "SPAN_KIND_INTERNAL" -> SpanKindInternal
    "SPAN_KIND_SERVER" -> SpanKindServer
    "SPAN_KIND_CLIENT" -> SpanKindClient
    "SPAN_KIND_PRODUCER" -> SpanKindProducer
    "SPAN_KIND_CONSUMER" -> SpanKindConsumer
    _ -> SpanKindUnspecified
parseSpanKind _ = SpanKindUnspecified

intToSpanKind :: Int -> SpanKind
intToSpanKind 1 = SpanKindInternal
intToSpanKind 2 = SpanKindServer
intToSpanKind 3 = SpanKindClient
intToSpanKind 4 = SpanKindProducer
intToSpanKind 5 = SpanKindConsumer
intToSpanKind _ = SpanKindUnspecified

findAttribute :: Text -> [Value] -> Text
findAttribute key attrs = fromMaybe "" $ listToMaybe $ mapMaybe findAttr attrs
  where
    findAttr (Object o) = do
        k <- parseMaybe (.: "key") o
        if k == key
            then parseMaybe parseStringValue o
            else Nothing
    findAttr _ = Nothing

    parseStringValue o = do
        v <- o .: "value"
        case v of
            Object vObj -> vObj .: "stringValue"
            _ -> fail "not a string value"

parseAttributes :: [Value] -> Map.Map Text Text
parseAttributes = Map.fromList . mapMaybe parseAttr
  where
    parseAttr (Object o) = parseMaybe parseKV o
    parseAttr _ = Nothing

    parseKV o = do
        k <- o .: "key"
        v <- o .: "value"
        val <- case v of
            Object vObj -> extractValue vObj
            _ -> fail "unknown value type"
        return (k, val)

    extractValue vObj =
        (vObj .: "stringValue")
            <|> (T.pack . show <$> (vObj .: "intValue" :: Parser Int))
            <|> (T.pack . show <$> (vObj .: "doubleValue" :: Parser Double))
            <|> (T.pack . show <$> (vObj .: "boolValue" :: Parser Bool))
            <|> pure ""
