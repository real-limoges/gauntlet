module Benchmark.Types.Units
  ( Nanoseconds (..)
  , Milliseconds (..)
  , nsToMs
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import Text.Printf (PrintfArg (..))

-- | Duration in nanoseconds with type safety.
newtype Nanoseconds = Nanoseconds {unNanoseconds :: Word64}
  deriving newtype (Show, Read, Eq, Ord, Num, Integral, Real, Enum, FromJSON, ToJSON)

-- | Duration in milliseconds with type safety.
newtype Milliseconds = Milliseconds {unMilliseconds :: Double}
  deriving newtype (Show, Eq, Ord, Num, FromJSON, ToJSON, Real, Fractional, RealFrac, Floating)

instance PrintfArg Milliseconds where
  formatArg (Milliseconds x) = formatArg x

-- | Convert nanoseconds to milliseconds.
nsToMs :: Nanoseconds -> Milliseconds
nsToMs (Nanoseconds ns) = Milliseconds (fromIntegral ns / 1_000_000)
