module Stats.Benchmark
  ( -- * Descriptive Statistics
    calculateStats

    -- * Bayesian Comparison
  , compareBayesian

    -- * Distribution Comparison
  , earthMoversDistance
  )
where

import Benchmark.Types
  ( BayesianComparison (..)
  , BenchmarkStats (..)
  , Milliseconds (..)
  , PercentileComparison (..)
  , TestingResponse (..)
  , nsToMs
  )
import Data.Maybe qualified as M
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as V
import Numeric.SpecFunctions (erfc)
import Statistics.Sample (mean, stdDev)
import Stats.Common (percentileSorted)

{-| Compute descriptive statistics from benchmark responses.
Failed responses (with errorMessage) are excluded from latency calculations.
-}
calculateStats :: [TestingResponse] -> BenchmarkStats
calculateStats results =
  let validDurations = M.mapMaybe getDuration results
      vector = V.fromList validDurations
      sorted = V.modify VA.sort vector
      total = Prelude.length results
      success = Prelude.length validDurations
      failures = total - success

      (avg, dev, mn, mx, median, pct95, pct99)
        | success == 0 = (0, 0, 0, 0, 0, 0, 0)
        | V.length sorted == 1 = let val = V.head sorted in (val, 0, val, val, val, val, val)
        | otherwise =
            ( mean vector
            , stdDev vector
            , V.minimum vector
            , V.maximum vector
            , percentileSorted 0.50 sorted
            , percentileSorted 0.95 sorted
            , percentileSorted 0.99 sorted
            )
      es
        | success == 0 = 0
        | otherwise = expectedShortfall sorted
   in BenchmarkStats
        { totalRequests = total
        , countSuccess = success
        , countFailure = failures
        , meanMs = avg
        , stdDevMs = dev
        , minMs = mn
        , maxMs = mx
        , p50Ms = median
        , p95Ms = pct95
        , p99Ms = pct99
        , esMs = es
        }

{-| Expected Shortfall: mean of the worst 1% of observations (E[X | X > p99]).
Input must be sorted ascending. Returns the last value for very small samples.
-}
expectedShortfall :: V.Vector Double -> Double
expectedShortfall sorted
  | V.null sorted = 0
  | otherwise =
      let n = V.length sorted
          threshold = floor (0.99 * (fromIntegral n :: Double))
          tailVals = V.drop threshold sorted
       in if V.null tailVals
            then V.last sorted
            else V.foldl' (+) 0 tailVals / fromIntegral (V.length tailVals)

-- | Extract duration from response (Nothing if error).
getDuration :: TestingResponse -> Maybe Double
getDuration response =
  case errorMessage response of
    Just _ -> Nothing
    Nothing -> Just $ unMilliseconds $ nsToMs (durationNs response)

{-| Bayesian comparison of two benchmark results.

Computes probability that B is faster than A, mean difference with
95% credible interval, Cohen's d effect size, and percentile comparisons.
Positive mean difference indicates A is slower (B is faster).
-}
compareBayesian :: BenchmarkStats -> BenchmarkStats -> BayesianComparison
compareBayesian statsA statsB =
  let muA = meanMs statsA
      muB = meanMs statsB
      sdA = stdDevMs statsA
      sdB = stdDevMs statsB
      varA = sdA ** 2
      varB = sdB ** 2
      countA = fromIntegral (countSuccess statsA)
      countB = fromIntegral (countSuccess statsB)

      muDiff = muA - muB
      sigmaDiff = sqrt ((varA / countA) + (varB / countB))
      probBIsFaster = if sigmaDiff > 0 then standardNormalCDF (muDiff / sigmaDiff) else 0.5
      ciLower = muDiff - (1.96 * sigmaDiff)
      ciUpper = muDiff + (1.96 * sigmaDiff)

      pooledVar =
        if countA + countB > 2
          then ((countA - 1) * varA + (countB - 1) * varB) / (countA + countB - 2)
          else (varA + varB) / 2
      pooledSd = sqrt pooledVar
      cohenD = if pooledSd > 0 then muDiff / pooledSd else 0

      relEffect = if muA > 0 then (muDiff / muA) * 100 else 0

      p95Comp = comparePercentile 0.95 (p95Ms statsA) (p95Ms statsB) sdA sdB countA countB
      p99Comp = comparePercentile 0.99 (p99Ms statsA) (p99Ms statsB) sdA sdB countA countB

      -- P(single request to B is faster than single request to A)
      -- Uses individual observation variance (σ²), not standard error (σ²/n)
      sigmaPred = sqrt (varA + varB)
      probSingle = if sigmaPred > 0 then standardNormalCDF (muDiff / sigmaPred) else 0.5
   in BayesianComparison
        { probBFasterThanA = probBIsFaster
        , probSingleRequestFaster = probSingle
        , meanDifference = muDiff
        , credibleIntervalLower = ciLower
        , credibleIntervalUpper = ciUpper
        , effectSize = cohenD
        , relativeEffect = relEffect
        , p95Comparison = p95Comp
        , p99Comparison = p99Comp
        }

-- | Compare percentiles between primary and candidate with Maritz-Jarrett SE.
comparePercentile ::
  Double -> Double -> Double -> Double -> Double -> Double -> Double -> PercentileComparison
comparePercentile p pctA pctB sdA sdB nA nB =
  let k = percentileSEMultiplier p
      seA = k * sdA / sqrt nA
      seB = k * sdB / sqrt nB
      seDiff = sqrt (seA ** 2 + seB ** 2)
      diff = pctA - pctB
      zPct = if seDiff > 0 then diff / seDiff else 0
      probRegress = 1 - standardNormalCDF zPct
   in PercentileComparison
        { pctDifference = diff
        , pctCredibleLower = diff - 1.96 * seDiff
        , pctCredibleUpper = diff + 1.96 * seDiff
        , probPctRegression = probRegress
        }

-- | Maritz-Jarrett standard error multiplier for percentiles.
percentileSEMultiplier :: Double -> Double
percentileSEMultiplier p =
  let z = inverseNormalCDF p
      phi = exp (-(0.5 * z * z)) / sqrt (2 * pi)
   in if phi > 0 then sqrt (p * (1 - p)) / phi else 1.0

-- | Rational approximation of inverse normal CDF (Abramowitz and Stegun).
inverseNormalCDF :: Double -> Double
inverseNormalCDF p
  | p <= 0 = -10
  | p >= 1 = 10
  | p < 0.5 = negate (approxInvNorm (1 - p))
  | otherwise = approxInvNorm p
  where
    approxInvNorm q =
      let t = sqrt (-(2 * log (1 - q)))
       in t
            - (2.515517 + 0.802853 * t + 0.010328 * t * t)
              / (1 + 1.432788 * t + 0.189269 * t * t + 0.001308 * t * t * t)

-- | Standard normal cumulative distribution function.
standardNormalCDF :: Double -> Double
standardNormalCDF x = 0.5 * erfc (-(x / sqrt 2))

{-| Earth Mover's Distance (1-Wasserstein distance).

Measures the minimum "work" to transform distribution A into B.
For equal sample sizes: EMD = mean(|sorted_A - sorted_B|)
For unequal sizes: uses CDF-based integration.
-}
earthMoversDistance :: V.Vector Double -> V.Vector Double -> Double
earthMoversDistance samplesA samplesB =
  let sortedA = V.toList $ V.modify VA.sort samplesA
      sortedB = V.toList $ V.modify VA.sort samplesB
      nA = length sortedA
      nB = length sortedB
   in if nA == nB
        then sum (zipWith (\a b -> abs (a - b)) sortedA sortedB) / fromIntegral nA
        else computeEMDGeneral samplesA samplesB

-- | General EMD for unequal sample sizes via CDF integration.
computeEMDGeneral :: V.Vector Double -> V.Vector Double -> Double
computeEMDGeneral a b
  | V.null a || V.null b = 0
  | otherwise = integrate 0 0 events
  where
    sortedA = V.toList $ V.modify VA.sort a
    sortedB = V.toList $ V.modify VA.sort b
    nA = fromIntegral (length sortedA) :: Double
    nB = fromIntegral (length sortedB) :: Double

    events =
      merge
        [(x, 1 / nA, 0) | x <- sortedA]
        [(x, 0, 1 / nB) | x <- sortedB]

    merge [] ys = ys
    merge xs [] = xs
    merge (x@(px, _, _) : xs) (y@(py, _, _) : ys)
      | px <= py = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys

    integrate _ _ [] = 0
    integrate _ _ [_] = 0
    integrate cdfA cdfB ((x, dA, dB) : rest@((nextX, _, _) : _)) =
      let cdfA' = cdfA + dA
          cdfB' = cdfB + dB
          width = nextX - x
          area = width * abs (cdfA' - cdfB')
       in area + integrate cdfA' cdfB' rest
