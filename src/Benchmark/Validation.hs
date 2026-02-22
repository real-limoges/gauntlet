{- |
Module      : Benchmark.Validation
Description : Per-response assertion checking
Stability   : experimental

Validates HTTP responses against a 'ValidationSpec': checks status codes
and navigates dot-path field assertions into JSON response bodies.
-}
module Benchmark.Validation (
    validateResponse,
    validateResponses,
)
where

import Benchmark.Types (
    FieldAssertion (..),
    TestingResponse (..),
    ValidationError (..),
    ValidationSpec (..),
    ValidationSummary (..),
 )
import Data.Aeson (Value (..), decode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

{- | Validate a single response against a 'ValidationSpec'.
Returns a list of all assertion failures (empty = all passed).
-}
validateResponse :: ValidationSpec -> TestingResponse -> [ValidationError]
validateResponse spec resp = statusErrors ++ fieldErrors
  where
    statusErrors = case validateStatus spec of
        Nothing -> []
        Just expected
            | statusCode resp == expected -> []
            | otherwise -> [StatusCodeMismatch expected (statusCode resp)]

    fieldErrors = case validateFields spec of
        Nothing -> []
        Just fields -> case respBody resp of
            Nothing -> [BodyNotJSON]
            Just bs -> case decode bs :: Maybe Value of
                Nothing -> [BodyNotJSON]
                Just jsonVal -> concatMap (checkField jsonVal) (Map.toList fields)

-- | Validate all responses for one endpoint, producing a summary.
validateResponses :: ValidationSpec -> [TestingResponse] -> ValidationSummary
validateResponses spec resps =
    let perResp = map (validateResponse spec) resps
        errors = concat perResp
        numFailed = length (filter (not . null) perResp)
     in ValidationSummary
            { totalValidated = length resps
            , totalFailed = numFailed
            , validationErrors = errors
            }

-- ---------------------------------------------------------------------------
-- Internal helpers

checkField :: Value -> (Text, FieldAssertion) -> [ValidationError]
checkField jsonVal (path, assertion) =
    let parts = T.splitOn "." (stripLeadingDollarDot path)
        mval = lookupPath parts jsonVal
     in case (mval, assertion) of
            (Nothing, _) -> [FieldNotFound path]
            (Just _, FieldPresent) -> []
            (Just actual, FieldEq expected)
                | actual == expected -> []
                | otherwise -> [FieldValueMismatch path expected actual]

-- | Strip a leading @$.@ or @$@ JSONPath prefix (e.g. @"$.id"@ → @"id"@).
stripLeadingDollarDot :: Text -> Text
stripLeadingDollarDot t
    | T.isPrefixOf "$." t = T.drop 2 t
    | T.isPrefixOf "$" t = T.drop 1 t
    | otherwise = t

{- | Traverse a JSON value by a sequence of object keys.
Returns 'Nothing' if any key is absent or if a non-object is encountered
before the path is exhausted.
-}
lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] v = Just v
lookupPath (k : rest) (Object obj) =
    case KeyMap.lookup (Key.fromText k) obj of
        Nothing -> Nothing
        Just v -> lookupPath rest v
lookupPath _ _ = Nothing
