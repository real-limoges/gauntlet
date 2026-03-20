-- | Tests for Benchmark.Execution.RateLimiter.
module RateLimiterSpec (rateLimiterSpec) where

import Benchmark.Execution.RateLimiter (currentTargetRpm, makeLimiter, waitForSlot)
import Benchmark.Types
  ( LoadMode (..)
  , LoadStep (..)
  , RampUpConfig (..)
  , isDurationBased
  , loadModeDurationSecs
  , totalRequestsForMode
  )
import Control.Concurrent.Async (replicateConcurrently_)
import Data.Time (diffUTCTime, getCurrentTime)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

rateLimiterSpec :: TestTree
rateLimiterSpec =
  testGroup
    "Benchmark.RateLimiter"
    [ testGroup
        "makeLimiter"
        [ testCase "returns Nothing for LoadUnthrottled" $ do
            ml <- makeLimiter LoadUnthrottled
            case ml of
              Nothing -> pure ()
              Just _ -> assertFailure "Expected Nothing"
        , testCase "returns Just for LoadConstantRpm" $ do
            ml <- makeLimiter (LoadConstantRpm 6000)
            case ml of
              Just _ -> pure ()
              Nothing -> assertFailure "Expected Just"
        , testCase "returns Just for LoadRampUp" $ do
            ml <- makeLimiter (LoadRampUp (RampUpConfig 600 6000 60))
            case ml of
              Just _ -> pure ()
              Nothing -> assertFailure "Expected Just"
        , testCase "returns Just for LoadStepLoad" $ do
            ml <- makeLimiter (LoadStepLoad [LoadStep 3000 30])
            case ml of
              Just _ -> pure ()
              Nothing -> assertFailure "Expected Just"
        , testCase "returns Just for LoadPoissonRpm" $ do
            ml <- makeLimiter (LoadPoissonRpm 6000)
            case ml of
              Just _ -> pure ()
              Nothing -> assertFailure "Expected Just"
        ]
    , testGroup
        "waitForSlot"
        [ testCase "constant RPM paces requests correctly" $ do
            -- 3000 RPM = 20ms per request; 5 requests should take ~80ms (first fires immediately)
            Just limiter <- makeLimiter (LoadConstantRpm 3000)
            start <- getCurrentTime
            replicateConcurrently_ 5 (waitForSlot limiter)
            end <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime end start) :: Double
            -- Should take at least ~80ms (4 intervals) but less than 300ms
            elapsed `shouldSatisfy` (>= 0.06)
            elapsed `shouldSatisfy` (< 0.3)
        , testCase "concurrent calls are serialized (no duplicate slots)" $ do
            -- At 1200 RPM (50ms interval), 4 concurrent waitForSlot calls
            -- should take ~150ms total (3 intervals after first)
            Just limiter <- makeLimiter (LoadConstantRpm 1200)
            start <- getCurrentTime
            replicateConcurrently_ 4 (waitForSlot limiter)
            end <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime end start) :: Double
            -- 3 intervals at 50ms = 150ms minimum
            elapsed `shouldSatisfy` (>= 0.12)
            elapsed `shouldSatisfy` (< 0.5)
        , testCase "poisson RPM dispatches 5 requests in roughly expected time" $ do
            -- 3000 RPM Poisson: mean inter-arrival = 20ms; 5 requests ~ 4 * 20ms = 80ms
            Just limiter <- makeLimiter (LoadPoissonRpm 3000)
            start <- getCurrentTime
            replicateConcurrently_ 5 (waitForSlot limiter)
            end <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime end start) :: Double
            -- Wide bounds due to randomness: at least 1 interval, less than 10x mean
            elapsed `shouldSatisfy` (>= 0.01)
            elapsed `shouldSatisfy` (< 0.8)
        ]
    , testGroup
        "currentTargetRpm"
        [ testCase "reports correct RPM for constant mode" $ do
            Just limiter <- makeLimiter (LoadConstantRpm 2520.0)
            rpm <- currentTargetRpm limiter
            -- Should be approximately 2520
            rpm `shouldSatisfy` (\r -> abs (r - 2520.0) < 60.0)
        , testCase "reports non-zero RPM for ramp mode" $ do
            Just limiter <- makeLimiter (LoadRampUp (RampUpConfig 600 6000 60))
            rpm <- currentTargetRpm limiter
            rpm `shouldSatisfy` (> 0)
        , testCase "reports exact configured RPM for poisson mode" $ do
            Just limiter <- makeLimiter (LoadPoissonRpm 4500.0)
            rpm <- currentTargetRpm limiter
            rpm `shouldBe` 4500.0
        ]
    , testGroup
        "LoadMode helpers"
        [ testCase "totalRequestsForMode constant uses fallback" $
            totalRequestsForMode (LoadConstantRpm 3000) 1000 `shouldBe` 1000
        , testCase "totalRequestsForMode rampUp computes from schedule" $
            -- (600 + 6000) / 2 / 60 * 60 = 3300
            totalRequestsForMode (LoadRampUp (RampUpConfig 600 6000 60)) 999 `shouldBe` 3300
        , testCase "totalRequestsForMode stepLoad sums steps" $
            -- 1200/60*30 + 3000/60*30 = 600 + 1500 = 2100
            totalRequestsForMode (LoadStepLoad [LoadStep 1200 30, LoadStep 3000 30]) 999 `shouldBe` 2100
        , testCase "isDurationBased is True for rampUp" $
            isDurationBased (LoadRampUp (RampUpConfig 600 6000 60)) `shouldBe` True
        , testCase "isDurationBased is True for stepLoad" $
            isDurationBased (LoadStepLoad [LoadStep 3000 30]) `shouldBe` True
        , testCase "isDurationBased is False for constant" $
            isDurationBased (LoadConstantRpm 3000) `shouldBe` False
        , testCase "isDurationBased is False for poisson" $
            isDurationBased (LoadPoissonRpm 3000) `shouldBe` False
        , testCase "totalRequestsForMode poisson uses fallback" $
            totalRequestsForMode (LoadPoissonRpm 3000) 1000 `shouldBe` 1000
        , testCase "loadModeDurationSecs for rampUp" $
            loadModeDurationSecs (LoadRampUp (RampUpConfig 600 6000 60)) `shouldSatisfy` (\d -> abs (d - 60) < 0.001)
        , testCase "loadModeDurationSecs for stepLoad" $
            loadModeDurationSecs (LoadStepLoad [LoadStep 1200 30, LoadStep 3000 45])
              `shouldSatisfy` (\d -> abs (d - 75) < 0.001)
        ]
    ]
