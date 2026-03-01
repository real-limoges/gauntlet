module Benchmark.Types.Verify
  ( JsonDiff (..)
  , VerificationResult (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A single differing path between two JSON bodies.
data JsonDiff = JsonDiff
  { jdPath :: Text
  -- ^ Dot-notation path, e.g. @"data.items[0].id"@
  , jdPrimary :: Text
  -- ^ JSON-encoded value from primary response
  , jdCandidate :: Text
  -- ^ JSON-encoded value from candidate response
  }
  deriving stock (Show, Eq, Generic)

data VerificationResult
  = Match
  | StatusMismatch Int Int
  | BodyMismatch [JsonDiff]
  | InvalidJSON String
  | -- | Request-level failure (timeout, connection error) on primary or candidate
    NetworkError String
  deriving stock (Show, Eq, Generic)
