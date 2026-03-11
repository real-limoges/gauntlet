module RateLimiterSpec (rateLimiterSpec) where

import Benchmark.Execution.RateLimiter (currentTargetRps, makeLimiter, waitForSlot)
import Benchmark.Types
  ( LoadMode (..)
  , LoadStep (..)
  , isDurationBased
  , loadModeDurationSecs
  , totalRequestsForMode
  )
import Control.Concurrent.Async (replicateConcurrently_)
import Data.Time (diffUTCTime, getCurrentTime)
import TastyCompat (shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

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
              Just _ -> error "Expected Nothing"
        , testCase "returns Just for LoadConstantRps" $ do
            ml <- makeLimiter (LoadConstantRps 100)
            case ml of
              Just _ -> pure ()
              Nothing -> error "Expected Just"
        , testCase "returns Just for LoadRampUp" $ do
            ml <- makeLimiter (LoadRampUp 10 100 60)
            case ml of
              Just _ -> pure ()
              Nothing -> error "Expected Just"
        , testCase "returns Just for LoadStepLoad" $ do
            ml <- makeLimiter (LoadStepLoad [LoadStep 50 30])
            case ml of
              Just _ -> pure ()
              Nothing -> error "Expected Just"
        ]
    , testGroup
        "waitForSlot"
        [ testCase "constant RPS paces requests correctly" $ do
            -- 50 RPS = 20ms per request; 5 requests should take ~80ms (first fires immediately)
            Just limiter <- makeLimiter (LoadConstantRps 50)
            start <- getCurrentTime
            replicateConcurrently_ 5 (waitForSlot limiter)
            end <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime end start) :: Double
            -- Should take at least ~80ms (4 intervals) but less than 300ms
            elapsed `shouldSatisfy` (>= 0.06)
            elapsed `shouldSatisfy` (< 0.3)
        , testCase "concurrent calls are serialized (no duplicate slots)" $ do
            -- At 20 RPS (50ms interval), 4 concurrent waitForSlot calls
            -- should take ~150ms total (3 intervals after first)
            Just limiter <- makeLimiter (LoadConstantRps 20)
            start <- getCurrentTime
            replicateConcurrently_ 4 (waitForSlot limiter)
            end <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime end start) :: Double
            -- 3 intervals at 50ms = 150ms minimum
            elapsed `shouldSatisfy` (>= 0.12)
            elapsed `shouldSatisfy` (< 0.5)
        ]
    , testGroup
        "currentTargetRps"
        [ testCase "reports correct RPS for constant mode" $ do
            Just limiter <- makeLimiter (LoadConstantRps 42.0)
            rps <- currentTargetRps limiter
            -- Should be approximately 42
            rps `shouldSatisfy` (\r -> abs (r - 42.0) < 1.0)
        , testCase "reports non-zero RPS for ramp mode" $ do
            Just limiter <- makeLimiter (LoadRampUp 10 100 60)
            rps <- currentTargetRps limiter
            rps `shouldSatisfy` (> 0)
        ]
    , testGroup
        "LoadMode helpers"
        [ testCase "totalRequestsForMode constant uses fallback" $
            totalRequestsForMode (LoadConstantRps 50) 1000 `shouldBe` 1000
        , testCase "totalRequestsForMode rampUp computes from schedule" $
            -- (10 + 100) / 2 * 60 = 3300
            totalRequestsForMode (LoadRampUp 10 100 60) 999 `shouldBe` 3300
        , testCase "totalRequestsForMode stepLoad sums steps" $
            -- 20*30 + 50*30 = 600 + 1500 = 2100
            totalRequestsForMode (LoadStepLoad [LoadStep 20 30, LoadStep 50 30]) 999 `shouldBe` 2100
        , testCase "isDurationBased is True for rampUp" $
            isDurationBased (LoadRampUp 10 100 60) `shouldBe` True
        , testCase "isDurationBased is True for stepLoad" $
            isDurationBased (LoadStepLoad [LoadStep 50 30]) `shouldBe` True
        , testCase "isDurationBased is False for constant" $
            isDurationBased (LoadConstantRps 50) `shouldBe` False
        , testCase "loadModeDurationSecs for rampUp" $
            loadModeDurationSecs (LoadRampUp 10 100 60) `shouldSatisfy` (\d -> abs (d - 60) < 0.001)
        , testCase "loadModeDurationSecs for stepLoad" $
            loadModeDurationSecs (LoadStepLoad [LoadStep 20 30, LoadStep 50 45])
              `shouldSatisfy` (\d -> abs (d - 75) < 0.001)
        ]
    ]
