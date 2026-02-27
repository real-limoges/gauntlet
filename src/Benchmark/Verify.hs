{-|
Module      : Benchmark.Verify
Description : JSON response comparison
Stability   : experimental

Compares responses from primary and candidate endpoints for semantic equality.
Supports an optional float tolerance for stochastic data science responses.
-}
module Benchmark.Verify
  ( verify
  , verifyWithNetworkCheck
  )
where

import Benchmark.Types
import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.List (nub)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V

{-| Check for network-level errors before comparing responses.
Returns a 'NetworkError' if either response has an 'errorMessage', otherwise
delegates to 'verify'.
-}
verifyWithNetworkCheck ::
  Double ->
  -- | compareFields whitelist
  Maybe [T.Text] ->
  -- | ignoreFields blacklist
  Maybe [T.Text] ->
  TestingResponse ->
  TestingResponse ->
  VerificationResult
verifyWithNetworkCheck tol mCompare mIgnore respA respB =
  case (errorMessage respA, errorMessage respB) of
    (Just e, _) -> NetworkError ("Primary request failed: " <> e)
    (_, Just e) -> NetworkError ("Candidate request failed: " <> e)
    (Nothing, Nothing) -> verify tol mCompare mIgnore respA respB

{-| Compare two responses for semantic equality.
The 'Double' tolerance is applied to all JSON number comparisons;
pass @0.0@ for exact matching.
When 'compareFields' is provided, only those keys (and their subtrees) are compared.
When 'ignoreFields' is provided, those keys are stripped at every depth before comparison.
-}
verify ::
  Double -> Maybe [T.Text] -> Maybe [T.Text] -> TestingResponse -> TestingResponse -> VerificationResult
verify tol mCompare mIgnore respA respB
  | statusCode respA /= statusCode respB =
      StatusMismatch (statusCode respA) (statusCode respB)
  | otherwise = compareBodies tol mCompare mIgnore (respBody respA) (respBody respB)

compareBodies ::
  Double -> Maybe [T.Text] -> Maybe [T.Text] -> Maybe ByteString -> Maybe ByteString -> VerificationResult
compareBodies _ _ _ Nothing Nothing = Match
compareBodies _ _ _ (Just _) Nothing = InvalidJSON "Response B body absent"
compareBodies _ _ _ Nothing (Just _) = InvalidJSON "Response A body absent"
compareBodies tol mCompare mIgnore (Just bodyA) (Just bodyB) =
  case (decode bodyA :: Maybe Value, decode bodyB :: Maybe Value) of
    (Nothing, _) -> InvalidJSON "Response A is not valid JSON"
    (_, Nothing) -> InvalidJSON "Response B is not valid JSON"
    (Just jsonA, Just jsonB) ->
      let
        -- First strip ignored fields
        (strippedA, strippedB) = case mIgnore of
          Nothing -> (jsonA, jsonB)
          Just ks ->
            let keyset = Set.fromList ks
             in (stripKeys keyset jsonA, stripKeys keyset jsonB)
        -- Then apply compareFields whitelist
        (effA, effB) = case mCompare of
          Nothing -> (strippedA, strippedB)
          Just ks ->
            let keyset = Set.fromList ks
             in (extractByKeys keyset strippedA, extractByKeys keyset strippedB)
        diffs = diffValues tol "" effA effB
       in
        if null diffs then Match else BodyMismatch diffs

{-| Recursively remove all keys matching the given set from a JSON tree.
Applied before 'extractByKeys' so that ignored fields are stripped first.
-}
stripKeys :: Set.Set T.Text -> Value -> Value
stripKeys keys (Object obj) =
  let filtered = KeyMap.filterWithKey (\k _ -> Key.toText k `Set.notMember` keys) obj
   in Object (KeyMap.map (stripKeys keys) filtered)
stripKeys keys (Array arr) = Array (fmap (stripKeys keys) arr)
stripKeys _ v = v

{-| Recursively search a JSON tree and return a flat 'Object' containing
every key in @keys@ paired with its full subtree.
Because all keys are assumed globally unique, there are no collisions.
-}
extractByKeys :: Set.Set T.Text -> Value -> Value
extractByKeys keys v = Object (KeyMap.fromList (go v))
  where
    go (Object obj) =
      concatMap
        ( \(k, val) ->
            let keep = [(k, val) | Key.toText k `Set.member` keys]
             in keep ++ go val
        )
        (KeyMap.toList obj)
    go (Array arr) = concatMap go (V.toList arr)
    go _ = []

-- | Encode a JSON 'Value' as compact 'Text'.
showValue :: Value -> T.Text
showValue = TL.toStrict . TLE.decodeUtf8 . encode

{-| Walk two JSON trees and collect every path where values differ.
@path@ is the dot-notation prefix built up during recursion (empty at root).
-}
diffValues :: Double -> T.Text -> Value -> Value -> [JsonDiff]
diffValues tol path (Object oa) (Object ob) =
  let allKeys = nub (KeyMap.keys oa ++ KeyMap.keys ob)
      childPath k =
        if T.null path
          then Key.toText k
          else path <> "." <> Key.toText k
   in concatMap
        ( \k -> case (KeyMap.lookup k oa, KeyMap.lookup k ob) of
            (Just va, Just vb) -> diffValues tol (childPath k) va vb
            (Just va, Nothing) -> [JsonDiff (childPath k) (showValue va) "<absent>"]
            (Nothing, Just vb) -> [JsonDiff (childPath k) "<absent>" (showValue vb)]
            (Nothing, Nothing) -> []
        )
        allKeys
diffValues tol path (Array as) (Array bs)
  | V.length as /= V.length bs =
      [ JsonDiff
          path
          ("array[" <> T.pack (show (V.length as)) <> "]")
          ("array[" <> T.pack (show (V.length bs)) <> "]")
      ]
  | otherwise =
      concat $
        V.toList $
          V.imap
            ( \i (va, vb) ->
                diffValues tol (path <> "[" <> T.pack (show i) <> "]") va vb
            )
            (V.zip as bs)
diffValues tol path a b
  | approxEqValue tol a b = []
  | otherwise = [JsonDiff path (showValue a) (showValue b)]

{-| Recursively compare two JSON values within a numeric tolerance.
All 'Number' nodes are compared with @abs(a - b) <= tol@.
For arrays, elements are compared pairwise; lengths must match.
For objects, keys must match and corresponding values must be approximately equal.
All other value types (String, Bool, Null) use exact equality.
-}
approxEqValue :: Double -> Value -> Value -> Bool
approxEqValue tol (Number a) (Number b) =
  abs (realToFrac a - realToFrac b :: Double) <= tol
approxEqValue tol (Array as) (Array bs) =
  V.length as == V.length bs && V.and (V.zipWith (approxEqValue tol) as bs)
approxEqValue tol (Object oa) (Object ob) =
  KeyMap.size oa == KeyMap.size ob
    && all
      ( \k -> case (KeyMap.lookup k oa, KeyMap.lookup k ob) of
          (Just va, Just vb) -> approxEqValue tol va vb
          _ -> False
      )
      (KeyMap.keys oa)
approxEqValue _ a b = a == b
