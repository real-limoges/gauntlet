{- |
Module      : Stats.Common
Description : Shared statistical utilities
Stability   : experimental

Common statistical functions used by both benchmark analysis and trace analysis.
-}
module Stats.Common (
    -- * Descriptive Statistics
    percentile,
    percentileList,
    stdDev,
    stdDevList,
    variance,
    varianceList,
) where

import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as V

{- | Linear interpolation for percentile calculation on a sorted vector.
Uses the R-7 method (default in R and NumPy).
-}
percentile :: Double -> V.Vector Double -> Double
percentile p vec
    | V.null vec = 0
    | V.length vec == 1 = V.head vec
    | otherwise =
        let sorted = V.modify VA.sort vec
            n = V.length sorted
            idx = p * fromIntegral (n - 1)
            lower = floor idx
            upper = ceiling idx
            frac = idx - fromIntegral lower
         in if lower == upper
                then sorted V.! lower
                else (sorted V.! lower) * (1 - frac) + (sorted V.! upper) * frac

{- | Linear interpolation for percentile calculation on a sorted list.
Assumes the input list is already sorted.
-}
percentileList :: Double -> [Double] -> Double
percentileList _ [] = 0
percentileList _ [x] = x
percentileList p sorted =
    let n = length sorted
        idx = p * fromIntegral (n - 1)
        lower = floor idx
        upper = ceiling idx
        frac = idx - fromIntegral lower
     in if lower == upper
            then sorted !! lower
            else (sorted !! lower) * (1 - frac) + (sorted !! upper) * frac

-- | Sample standard deviation for a vector.
stdDev :: V.Vector Double -> Double
stdDev vec = sqrt (variance vec)

-- | Sample standard deviation for a list, given the precomputed mean.
stdDevList :: Double -> [Double] -> Double
stdDevList avg xs = sqrt (varianceList avg xs)

-- | Sample variance for a vector.
variance :: V.Vector Double -> Double
variance vec
    | V.length vec <= 1 = 0
    | otherwise =
        let n = V.length vec
            avg = V.sum vec / fromIntegral n
            sumSq = V.sum $ V.map (\x -> (x - avg) ** 2) vec
         in sumSq / fromIntegral (n - 1) -- Sample variance (n-1)

-- | Population variance for a list, given the precomputed mean.
varianceList :: Double -> [Double] -> Double
varianceList _ [] = 0
varianceList _ [_] = 0
varianceList avg xs =
    let n = length xs
        sumSq = sum $ map (\x -> (x - avg) ** 2) xs
     in sumSq / fromIntegral n -- Population variance (n) for consistency with original
