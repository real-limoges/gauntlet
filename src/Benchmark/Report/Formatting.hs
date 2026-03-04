{-|
Module      : Benchmark.Report.Formatting
Description : Shared formatters for terminal and markdown reports
-}
module Benchmark.Report.Formatting
  ( formatMWU
  , formatKS
  , formatAD
  , formatValidationError
  ) where

import Benchmark.Types
  ( ADResult (..)
  , KSResult (..)
  , MWUResult (..)
  , ValidationError (..)
  )
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

-- | Format a Mann-Whitney U result as a human-readable string.
formatMWU :: Maybe MWUResult -> String
formatMWU Nothing = "sample too small"
formatMWU (Just r)
  | mwuSignificant r = "Significant (p < 0.05)"
  | otherwise = "Not significant (p >= 0.05)"

-- | Format a Kolmogorov-Smirnov result as a human-readable string.
formatKS :: Maybe KSResult -> String
formatKS Nothing = "sample too small"
formatKS (Just r) =
  printf
    "D = %.3f, p = %.3f (%s)"
    (ksStatistic r)
    (ksPValue r)
    (if ksSignificant r then "significant" else "not significant" :: String)

-- | Format an Anderson-Darling result as a human-readable string.
formatAD :: Maybe ADResult -> String
formatAD Nothing = "sample too small"
formatAD (Just r) =
  printf
    "A² = %.3f, p ≈ %.3f (%s)"
    (adStatistic r)
    (adPValue r)
    (if adSignificant r then "significant" else "not significant" :: String)

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
