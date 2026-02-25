{-|
Module      : Benchmark.Verify
Description : JSON response comparison
Stability   : experimental

Compares responses from primary and candidate endpoints for semantic equality.
-}
module Benchmark.Verify
  ( verify
  )
where

import Benchmark.Types
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy (ByteString)

-- | Compare two responses for semantic equality.
verify :: TestingResponse -> TestingResponse -> VerificationResult
verify respA respB
  | statusCode respA /= statusCode respB =
      StatusMismatch (statusCode respA) (statusCode respB)
  | otherwise = compareBodies (respBody respA) (respBody respB)

compareBodies :: Maybe ByteString -> Maybe ByteString -> VerificationResult
compareBodies Nothing Nothing = Match
compareBodies (Just _) Nothing = InvalidJSON "Response B was Empty"
compareBodies Nothing (Just _) = InvalidJSON "Response A was Empty"
compareBodies (Just bodyA) (Just bodyB) =
  case (decode bodyA :: Maybe Value, decode bodyB :: Maybe Value) of
    (Nothing, _) -> InvalidJSON "Response A was Empty"
    (_, Nothing) -> InvalidJSON "Response B was Empty"
    (Just jsonA, Just jsonB) ->
      if jsonA == jsonB
        then Match
        else BodyMismatch "JSON bodies differ"
