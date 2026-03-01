{-|
Module      : Benchmark.Validation
Description : Per-response assertion checking
Stability   : experimental

Validates HTTP responses against a 'ValidationSpec': checks status codes
and navigates dot-path field assertions into JSON response bodies.
-}
module Benchmark.Validation
  ( validateResponse
  , validateResponses
  )
where

import Benchmark.Types
  ( FieldAssertion (..)
  , TestingResponse (..)
  , ValidationError (..)
  , ValidationSpec (..)
  , ValidationSummary (..)
  )
import Data.Aeson (Value (..), decode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

{-| Validate a single response against a 'ValidationSpec'.
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
        Nothing -> [BodyAbsent]
        Just bs -> case decode bs :: Maybe Value of
          Nothing -> [BodyInvalidJSON]
          Just jsonVal -> concatMap (checkField jsonVal) (Map.toList fields)

{-| Validate all responses for one endpoint, producing a summary.
Errors are collected from at most 50 failing responses to prevent unbounded accumulation.
-}
validateResponses :: ValidationSpec -> [TestingResponse] -> ValidationSummary
validateResponses spec resps =
  let perResp = map (validateResponse spec) resps
      failedPerResp = filter (not . null) perResp
      errors = concat (take 50 failedPerResp)
      numFailed = length failedPerResp
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
        -- FieldPresent: key must exist (null counts as present)
        (Just _, FieldPresent) -> []
        -- FieldEq: exact value equality
        (Just actual, FieldEq expected)
          | actual == expected -> []
          | otherwise -> [FieldValueMismatch path expected actual]
        -- FieldNull: value must be JSON null
        (Just Null, FieldNull) -> []
        (Just actual, FieldNull) -> [FieldValueMismatch path Null actual]
        -- FieldNotNull: value must exist and not be null
        (Just Null, FieldNotNull) -> [FieldValueMismatch path (String "<not null>") Null]
        (Just _, FieldNotNull) -> []
        -- FieldType: value must be the specified JSON type
        (Just actual, FieldType expected) ->
          let actualType = jsonTypeName actual
           in [FieldValueMismatch path (String expected) (String actualType) | actualType /= expected]
        -- FieldMatches: string value must match regex (POSIX ERE)
        (Just (String s), FieldMatches pat)
          | T.unpack s =~ T.unpack pat -> []
          | otherwise -> [FieldValueMismatch path (String pat) (String s)]
        (Just actual, FieldMatches pat) ->
          [FieldValueMismatch path (String ("matches:" <> pat)) actual]
        -- FieldRange: numeric value must be within [min, max]
        (Just (Number n), FieldRange mmin mmax) ->
          let tooLow = maybe False (n <) mmin
              tooHigh = maybe False (n >) mmax
           in [FieldValueMismatch path (rangeValue mmin mmax) (Number n) | tooLow || tooHigh]
        (Just actual, FieldRange mmin mmax) ->
          [FieldValueMismatch path (rangeValue mmin mmax) actual]
        -- ArrayLength: array must have exactly N elements
        (Just (Array arr), ArrayLength expected)
          | V.length arr == expected -> []
          | otherwise ->
              [ FieldValueMismatch
                  path
                  (Number (fromIntegral expected))
                  (Number (fromIntegral (V.length arr)))
              ]
        (Just actual, ArrayLength expected) ->
          [FieldValueMismatch path (Number (fromIntegral expected)) actual]
        -- ArrayContains: value must appear in the array
        (Just (Array arr), ArrayContains expected)
          | expected `V.elem` arr -> []
          | otherwise -> [FieldValueMismatch path expected (String "<not in array>")]
        (Just actual, ArrayContains _) ->
          [FieldValueMismatch path (String "<array>") actual]

-- | Return the JSON type name of a value as used in 'FieldType' assertions.
jsonTypeName :: Value -> Text
jsonTypeName (String _) = "string"
jsonTypeName (Number _) = "number"
jsonTypeName (Bool _) = "boolean"
jsonTypeName (Array _) = "array"
jsonTypeName (Object _) = "object"
jsonTypeName Null = "null"

-- | Build a JSON object describing a numeric range for error messages.
rangeValue :: Maybe Scientific -> Maybe Scientific -> Value
rangeValue mmin mmax =
  object (catMaybes [("min" .=) <$> mmin, ("max" .=) <$> mmax])

-- | Strip a leading @$.@ or @$@ JSONPath prefix (e.g. @"$.id"@ → @"id"@).
stripLeadingDollarDot :: Text -> Text
stripLeadingDollarDot t
  | T.isPrefixOf "$." t = T.drop 2 t
  | T.isPrefixOf "$" t = T.drop 1 t
  | otherwise = t

{-| Traverse a JSON value by a sequence of object keys.
Returns 'Nothing' if any key is absent or if a non-object is encountered
before the path is exhausted.
-}
lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] v = Just v
lookupPath (k : rest) (Object obj) =
  case KeyMap.lookup (Key.fromText k) obj of
    Nothing -> Nothing
    Just v -> lookupPath rest v
lookupPath (k : rest) (Array arr)
  | T.all isDigit k =
      case readMaybe (T.unpack k) :: Maybe Int of
        Nothing -> Nothing
        Just idx -> if idx < V.length arr then lookupPath rest (arr V.! idx) else Nothing
lookupPath _ _ = Nothing
