-- | Benchmark statistical analysis: descriptive stats, Bayesian comparison, and earth mover's distance.
module Stats.Benchmark
  ( -- * Descriptive Statistics
    calculateStats

    -- * Bayesian Comparison
  , compareBayesian
  , PercentileInput (..)

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

-- | Z-score for the 95% credible interval (two-tailed).
z95 :: Double
z95 = 1.96

-- | Percentile threshold for Expected Shortfall (ES = E[X | X > p99]).
esPercentile :: Double
esPercentile = 0.99

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
          tailSize = max 1 $ ceiling ((1 - esPercentile) * fromIntegral n) :: Int
          tailVals = V.drop (n - tailSize) sorted
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
   in BayesianComparison
        { probBFasterThanA = probFaster muDiff varA varB countA countB
        , probSingleRequestFaster = probSingleFaster muDiff varA varB
        , probBLessJittery = probJitter sdA sdB varA varB countA countB
        , meanDifference = muDiff
        , credibleIntervalLower = ciLower muDiff varA varB countA countB
        , credibleIntervalUpper = ciUpper muDiff varA varB countA countB
        , effectSize = cohenD muDiff varA varB countA countB
        , relativeEffect = if muA > 0 then (muDiff / muA) * 100 else 0
        , p95Comparison =
            comparePercentile
              PercentileInput
                { piQuantile = 0.95
                , piValueA = p95Ms statsA
                , piValueB = p95Ms statsB
                , piStdDevA = sdA
                , piStdDevB = sdB
                , piCountA = countA
                , piCountB = countB
                }
        , p99Comparison =
            comparePercentile
              PercentileInput
                { piQuantile = 0.99
                , piValueA = p99Ms statsA
                , piValueB = p99Ms statsB
                , piStdDevA = sdA
                , piStdDevB = sdB
                , piCountA = countA
                , piCountB = countB
                }
        }
  where
    -- P(mean_B < mean_A) using population-level standard error σ/√n
    probFaster muDiff varA varB countA countB
      | countA <= 0 || countB <= 0 = 0.5
      | otherwise =
          let sigmaDiff = sqrt ((varA / countA) + (varB / countB))
           in if sigmaDiff > 0 then standardNormalCDF (muDiff / sigmaDiff) else 0.5

    ciLower muDiff varA varB countA countB =
      muDiff - z95 * sqrt ((varA / countA) + (varB / countB))

    ciUpper muDiff varA varB countA countB =
      muDiff + z95 * sqrt ((varA / countA) + (varB / countB))

    -- Cohen's d using pooled standard deviation
    cohenD muDiff varA varB countA countB =
      let pooledVar =
            if countA + countB > 2
              then ((countA - 1) * varA + (countB - 1) * varB) / (countA + countB - 2)
              else (varA + varB) / 2
          pooledSd = sqrt pooledVar
       in if pooledSd > 0 then muDiff / pooledSd else 0

    -- P(X_B < X_A) for individual requests, uses σ not σ/√n
    probSingleFaster muDiff varA varB =
      let sigmaPred = sqrt (varA + varB)
       in if sigmaPred > 0 then standardNormalCDF (muDiff / sigmaPred) else 0.5

    -- P(sigma_B < sigma_A) via log-normal approximation log(s²) ~ N(log(σ²), 2/(n-1))
    probJitter sdA sdB varA varB countA countB
      | sdA <= 0 || sdB <= 0 || countA <= 1 || countB <= 1 = 0.5
      | otherwise =
          let seLogVar = sqrt (2 / (countA - 1) + 2 / (countB - 1))
              zJitter = (log varA - log varB) / seLogVar
           in standardNormalCDF zJitter

-- | Inputs for a percentile comparison.
data PercentileInput = PercentileInput
  { piQuantile :: Double
  , piValueA :: Double
  , piValueB :: Double
  , piStdDevA :: Double
  , piStdDevB :: Double
  , piCountA :: Double
  , piCountB :: Double
  }

-- | Compare percentiles between primary and candidate with Maritz-Jarrett SE.
comparePercentile :: PercentileInput -> PercentileComparison
comparePercentile PercentileInput {..} =
  let k = percentileSEMultiplier piQuantile
      seA = k * piStdDevA / sqrt piCountA
      seB = k * piStdDevB / sqrt piCountB
      seDiff = sqrt (seA ** 2 + seB ** 2)
      diff = piValueA - piValueB
      zPct = if seDiff > 0 then diff / seDiff else 0
      probRegress = 1 - standardNormalCDF zPct
   in PercentileComparison
        { pctDifference = diff
        , pctCredibleLower = diff - z95 * seDiff
        , pctCredibleUpper = diff + z95 * seDiff
        , probPctRegression = probRegress
        }

-- | Maritz-Jarrett standard error multiplier for percentiles.
percentileSEMultiplier :: Double -> Double
percentileSEMultiplier p =
  let z = inverseNormalCDF p
      phi = exp (-(0.5 * z * z)) / sqrt (2 * pi)
   in if phi > 0 then sqrt (p * (1 - p)) / phi else 1.0

{-| Rational approximation of inverse normal CDF.
Abramowitz and Stegun, Handbook of Mathematical Functions (1964), formula 26.2.23.
Coefficients: c0=2.515517, c1=0.802853, c2=0.010328, d1=1.432788, d2=0.189269, d3=0.001308.
-}
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
  let sortedA = V.modify VA.sort samplesA
      sortedB = V.modify VA.sort samplesB
      nA = V.length sortedA
      nB = V.length sortedB
   in if nA == nB
        then V.sum (V.zipWith (\a b -> abs (a - b)) sortedA sortedB) / fromIntegral nA
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

    -- Walk the merged event stream, accumulating CDF area between the two distributions
    integrate _ _ [] = 0
    integrate _ _ [_] = 0
    integrate cdfA cdfB ((x, dA, dB) : rest@((nextX, _, _) : _)) =
      let cdfA' = cdfA + dA
          cdfB' = cdfB + dB
          width = nextX - x
          area = width * abs (cdfA' - cdfB')
       in area + integrate cdfA' cdfB' rest
