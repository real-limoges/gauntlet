module Benchmark.Types.Internal (dropFieldPrefix) where

import Data.Char (toLower)

-- | Drop a record field prefix and lowercase the first remaining character.
-- Used with aeson's 'fieldLabelModifier' to map Haskell field names to JSON keys.
dropFieldPrefix :: String -> String -> String
dropFieldPrefix prefix s = case drop (length prefix) s of
  (c : cs) -> toLower c : cs
  [] -> s
