-- | Request endpoint and HTTP response data types.
module Benchmark.Types.Response
  ( -- * Request/Response
    Endpoint (..)
  , TestingResponse (..)

    -- * Validation
  , FieldAssertion (..)
  , ValidationSpec (..)
  , ValidationError (..)
  , ValidationSummary (..)
  )
where

import Benchmark.Types.Internal (dropFieldPrefix)
import Benchmark.Types.Units (Nanoseconds)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Per-field assertion in a validation spec.
data FieldAssertion
  = -- | Assert the key exists (any value, including null)
    FieldPresent
  | -- | Assert exact value match
    FieldEq Value
  | -- | Assert field is explicitly @null@
    FieldNull
  | -- | Assert field exists and is not @null@
    FieldNotNull
  | -- | Assert JSON type: @"string"@, @"number"@, @"boolean"@, @"array"@, @"object"@, @"null"@
    FieldType Text
  | -- | Assert string field matches a regex pattern (POSIX ERE)
    FieldMatches Text
  | -- | Assert numeric field is within [min, max] (both bounds optional)
    FieldRange (Maybe Scientific) (Maybe Scientific)
  | -- | Assert array has exactly N elements
    ArrayLength Int
  | -- | Assert a value is present in an array
    ArrayContains Value
  deriving stock (Show, Eq, Generic)

instance FromJSON FieldAssertion where
  parseJSON = withObject "FieldAssertion" $ \o ->
    asum
      [ FieldPresent <$ (o .: "present" >>= \b -> if (b :: Bool) then pure () else fail "present must be true")
      , FieldEq <$> o .: "eq"
      , FieldNull <$ (o .: "null" >>= \b -> if (b :: Bool) then pure () else fail "null must be true")
      , FieldNotNull <$ (o .: "notNull" >>= \b -> if (b :: Bool) then pure () else fail "notNull must be true")
      , FieldType <$> o .: "type"
      , FieldMatches <$> o .: "matches"
      , o .: "range" >>= withObject "range" (\r -> FieldRange <$> r .:? "min" <*> r .:? "max")
      , ArrayLength <$> o .: "arrayLength"
      , ArrayContains <$> o .: "arrayContains"
      ]

instance ToJSON FieldAssertion where
  toJSON FieldPresent = object ["present" .= True]
  toJSON (FieldEq v) = object ["eq" .= v]
  toJSON FieldNull = object ["null" .= True]
  toJSON FieldNotNull = object ["notNull" .= True]
  toJSON (FieldType t) = object ["type" .= t]
  toJSON (FieldMatches pat) = object ["matches" .= pat]
  toJSON (FieldRange mmin mmax) =
    object ["range" .= object (catMaybes [("min" .=) <$> mmin, ("max" .=) <$> mmax])]
  toJSON (ArrayLength n) = object ["arrayLength" .= n]
  toJSON (ArrayContains v) = object ["arrayContains" .= v]

-- | Declarative validation rules applied to every response for an endpoint.
data ValidationSpec = ValidationSpec
  { validateStatus :: Maybe Int
  -- ^ Expected HTTP status code
  , validateFields :: Maybe (Map Text FieldAssertion)
  -- ^ Map from dot-path (e.g. "$.user.id") to assertion
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ValidationSpec where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = dropFieldPrefix "validate"
        }

instance ToJSON ValidationSpec where
  toJSON spec =
    object $
      maybe [] (\s -> ["status" .= s]) (validateStatus spec)
        ++ maybe [] (\f -> ["fields" .= f]) (validateFields spec)

-- | A single validation failure on a response.
data ValidationError
  = -- | Status code did not match: expected, actual
    StatusCodeMismatch Int Int
  | -- | Field path was not found in the response body
    FieldNotFound Text
  | -- | Field path found but value differed: path, expected, actual
    FieldValueMismatch Text Value Value
  | -- | Response body was absent (no body returned)
    BodyAbsent
  | -- | Response body was present but could not be decoded as JSON
    BodyInvalidJSON
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Aggregate validation results for all responses from one endpoint.
data ValidationSummary = ValidationSummary
  { totalValidated :: Int
  , totalFailed :: Int
  , validationErrors :: [ValidationError]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | An HTTP endpoint to benchmark, with method, URL, optional body, headers, and validation.
data Endpoint = Endpoint
  { method :: Text
  , url :: Text
  , body :: Maybe Value
  , headers :: [(Text, Text)]
  , validate :: Maybe ValidationSpec
  -- ^ Optional per-response validation rules
  }
  deriving stock (Show, Eq)

-- | Result of a single benchmarked HTTP request.
data TestingResponse = TestingResponse
  { durationNs :: Nanoseconds
  , statusCode :: Int
  , respBody :: Maybe LBS.ByteString
  , errorMessage :: Maybe String
  , requestedAt :: UTCTime
  }
  deriving stock (Show, Eq)
