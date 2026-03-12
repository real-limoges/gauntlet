-- | Validation error formatting for human-readable output.
module Benchmark.Report.Formatting
  ( formatValidationError
  ) where

import Benchmark.Types (ValidationError (..))
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

-- | Format a validation error as a plain text line.
formatValidationError :: ValidationError -> Text
formatValidationError (StatusCodeMismatch expected actual) =
  T.pack $ printf "Status code mismatch: expected %d, got %d" expected actual
formatValidationError (FieldNotFound path) =
  "Field not found: " <> path
formatValidationError (FieldValueMismatch path expected actual) =
  T.pack $
    printf
      "Field value mismatch at %s: expected %s, actual %s"
      (T.unpack path)
      (renderValue expected)
      (renderValue actual)
formatValidationError BodyAbsent =
  "Response body absent"
formatValidationError BodyInvalidJSON =
  "Response body is not valid JSON"

renderValue :: Value -> String
renderValue = LBS8.unpack . encode
