module ContextSpec (contextSpec) where

import Benchmark.TUI.State (BenchmarkEvent (..))
import Benchmark.Types (Settings (..), TestConfig (..))
import Control.Concurrent.STM (atomically, newTChanIO, readTChan)
import Data.Text qualified as T
import Runner.Context (RunContext (..), emitEvent, initContext)
import System.IO (hClose, hFlush)
import System.IO.Temp (withSystemTempFile)
import TastyCompat (shouldBe, shouldReturn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers (makeValidConfig)

contextSpec :: TestTree
contextSpec =
  testGroup
    "Runner.Context"
    [ testGroup
        "emitEvent"
        [ testCase "Nothing channel is a no-op" $
            emitEvent Nothing (PhaseStarted 1) `shouldReturn` ()
        , testCase "Just channel writes the event" $ do
            chan <- newTChanIO
            emitEvent (Just chan) (PhaseStarted 1)
            event <- atomically $ readTChan chan
            event `shouldBe` PhaseStarted 1
        ]
    , testGroup
        "initContext"
        [ testCase "reads token from secrets file" $
            withTempToken "my-secret-token\n" $ \tokenPath -> do
              let setts = (settings makeValidConfig) {secrets = T.pack tokenPath}
              ctx <- initContext setts "/dev/null" "ts" Nothing
              -- Token should have whitespace trimmed
              T.strip (rcToken ctx) `shouldBe` "my-secret-token"
        , testCase "stores settings verbatim" $
            withTempToken "tok\n" $ \tokenPath -> do
              let setts = (settings makeValidConfig) {secrets = T.pack tokenPath, iterations = 42}
              ctx <- initContext setts "/dev/null" "ts" Nothing
              iterations (rcSettings ctx) `shouldBe` 42
        , testCase "stores csvFile and timestamp" $
            withTempToken "tok\n" $ \tokenPath -> do
              let setts = (settings makeValidConfig) {secrets = T.pack tokenPath}
              ctx <- initContext setts "/tmp/test.csv" "2024-01-01" Nothing
              rcCsvFile ctx `shouldBe` "/tmp/test.csv"
              rcTimestamp ctx `shouldBe` "2024-01-01"
        , testCase "stores Nothing event channel as Nothing" $
            withTempToken "tok\n" $ \tokenPath -> do
              let setts = (settings makeValidConfig) {secrets = T.pack tokenPath}
              ctx <- initContext setts "/dev/null" "ts" Nothing
              case rcEventChan ctx of
                Nothing -> pure ()
                Just _ -> assertFailure "Expected Nothing event channel"
        ]
    ]

-- Helpers

withTempToken :: String -> (FilePath -> IO a) -> IO a
withTempToken content action =
  withSystemTempFile "token" $ \path h -> do
    hFlush h
    hClose h
    writeFile path content
    action path
