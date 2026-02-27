{-|
Module      : Stats.Benchmark
Description : Statistical analysis for benchmark results
Stability   : experimental

Computes descriptive statistics and Bayesian A/B comparison between
benchmark runs. Uses conjugate normal model for mean comparison with
95% credible intervals.
-}
module Stats.Benchmark
  ( -- * Descriptive Statistics
    calculateStats

    -- * Bayesian Comparison
  , compareBayesian

    -- * Frequentist Tests
  , addFrequentistTests

    -- * Distribution Comparison
  , earthMoversDistance
  )
where

import Benchmark.Types
  ( ADResult (..)
  , BayesianComparison (..)
  , BenchmarkStats (..)
  , KSResult (..)
  , MWUResult (..)
  , Milliseconds (..)
  , PercentileComparison (..)
  , TestingResponse (..)
  , nsToMs
  )
import Data.List (sortBy)
import Data.Maybe qualified as M
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as V
import Numeric.SpecFunctions (erfc)
import Statistics.Sample (mean, stdDev)
import Statistics.Test.KolmogorovSmirnov (kolmogorovSmirnovTest2)
import Statistics.Test.MannWhitneyU (mannWhitneyUtest)
import Statistics.Test.Types (PositionTest (..), Test (..), TestResult (..))
import Statistics.Types (mkPValue, pValue)
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
        , mannWhitneyU = Nothing
        , kolmogorovSmirnov = Nothing
        , andersonDarling = Nothing
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

{-| Enrich a 'BayesianComparison' with Mann-Whitney U and KS test results.
Call this after 'compareBayesian' when the raw responses are available.
Both tests require at least 2 successful responses per sample.
-}
addFrequentistTests :: [TestingResponse] -> [TestingResponse] -> BayesianComparison -> BayesianComparison
addFrequentistTests responsesA responsesB bayes =
  let durA = V.fromList $ M.mapMaybe getDuration responsesA
      durB = V.fromList $ M.mapMaybe getDuration responsesB
   in bayes
        { mannWhitneyU = runMWU durA durB
        , kolmogorovSmirnov = runKS durA durB
        , andersonDarling = runAD durA durB
        }

-- | Run Mann-Whitney U test. Returns Nothing if either sample has fewer than 2 observations.
runMWU :: V.Vector Double -> V.Vector Double -> Maybe MWUResult
runMWU a b
  | V.length a < 2 || V.length b < 2 = Nothing
  | otherwise =
      let result = mannWhitneyUtest SamplesDiffer (mkPValue 0.05) a b
       in Just $ MWUResult {mwuSignificant = result == Just Significant}

-- | Run two-sample KS test. Returns Nothing if either sample has fewer than 2 observations.
runKS :: V.Vector Double -> V.Vector Double -> Maybe KSResult
runKS a b
  | V.length a < 2 || V.length b < 2 = Nothing
  | otherwise = case kolmogorovSmirnovTest2 a b of
      Nothing -> Nothing
      Just t ->
        let p = pValue (testSignificance t)
         in Just
              KSResult
                { ksStatistic = testStatistics t
                , ksPValue = p
                , ksSignificant = p < 0.05
                }

-- | Extract duration from response (Nothing if error).
getDuration :: TestingResponse -> Maybe Double
getDuration response =
  case errorMessage response of
    Just _ -> Nothing
    Nothing -> Just $ unMilliseconds $ nsToMs (durationNs response)

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

{-| Run two-sample Anderson-Darling test (Scholz & Stephens 1987).
More sensitive to tail differences than KS. Returns Nothing if either sample < 5.
-}
runAD :: V.Vector Double -> V.Vector Double -> Maybe ADResult
runAD va vb
  | V.length va < 5 || V.length vb < 5 = Nothing
  | otherwise =
      let a = V.toList va
          b = V.toList vb
          bigN = V.length va + V.length vb
          stat = computeADStatistic a b
          -- Scholz-Stephens k=2 variance approximation: σ² ≈ (4·H_{N-1} - 6) / 45
          gN = harmonicNumber (bigN - 1)
          sigma = sqrt ((4 * gN - 6) / 45)
          tStat = if sigma > 0 then (stat - 1) / sigma else 0
          pVal = adPValueFromT tStat
       in Just
            ADResult
              { adStatistic = stat
              , adPValue = pVal
              , adSignificant = pVal < 0.05
              }

{-| Two-sample Anderson-Darling A² statistic (Scholz & Stephens 1987).
A² = (N / (m·n)) · Σ_{i=1}^{N-1} (N·F_i - m·i)² / (i·(N-i))
where F_i is the count of sample-a values at combined rank i.
-}
computeADStatistic :: [Double] -> [Double] -> Double
computeADStatistic a b = (bigN / (m * n)) * total
  where
    m = fromIntegral (length a) :: Double
    n = fromIntegral (length b) :: Double
    bigN = m + n
    tagged = map (,True) a ++ map (,False) b
    combined = sortBy (comparing fst) tagged
    (_, _, total) = foldl step (0.0, 1.0, 0.0) combined
    step (fi, l, acc) (_, isA) =
      let fi' = if isA then fi + 1.0 else fi
          term =
            if l < bigN
              then (bigN * fi' - m * l) ^ (2 :: Int) / (l * (bigN - l))
              else 0.0
       in (fi', l + 1.0, acc + term)

-- | H_{n} = Σ_{i=1}^{n} 1/i (harmonic number).
harmonicNumber :: Int -> Double
harmonicNumber n = sum [1 / fromIntegral i | i <- [1 .. n]]

{-| Approximate p-value for the standardized AD statistic T,
interpolated from Scholz-Stephens (1987) Table 2 asymptotic quantiles.
-}
adPValueFromT :: Double -> Double
adPValueFromT t
  | t < 1.248 = 0.25
  | t < 1.610 = lerp 0.25 0.10 1.248 1.610 t
  | t < 1.859 = lerp 0.10 0.05 1.610 1.859 t
  | t < 2.094 = lerp 0.05 0.025 1.859 2.094 t
  | t < 2.408 = lerp 0.025 0.01 2.094 2.408 t
  | otherwise = 0.01
  where
    lerp p1 p2 t1 t2 x = p1 + (x - t1) / (t2 - t1) * (p2 - p1)

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
