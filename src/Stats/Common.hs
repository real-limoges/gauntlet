{- |
Module      : Stats.Common
Description : Shared statistical utilities
Stability   : experimental

Common statistical functions used by both benchmark analysis and trace analysis.
-}
module Stats.Common (
    -- * Descriptive Statistics
    percentile,
    percentileSorted,
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
    | otherwise = percentileSorted p (V.modify VA.sort vec)

{- | Like 'percentile' but assumes the input vector is already sorted.
Skips the sort step — use this when computing multiple percentiles from
the same pre-sorted vector.
-}
percentileSorted :: Double -> V.Vector Double -> Double
percentileSorted p sorted
    | V.null sorted = 0
    | V.length sorted == 1 = V.head sorted
    | otherwise =
        let n = V.length sorted
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
percentileList p sorted = percentileSorted p (V.fromList sorted)

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

-- | Sample variance for a list, given the precomputed mean.
varianceList :: Double -> [Double] -> Double
varianceList _ [] = 0
varianceList _ [_] = 0
varianceList avg xs =
    let n = length xs
        sumSq = sum $ map (\x -> (x - avg) ** 2) xs
     in sumSq / fromIntegral (n - 1) -- Sample variance (n-1)
