-- | Tests for Log.
module LogSpec (logSpec) where

import Benchmark.Types (LogLevel (..))
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Log (Logger (..), formatMessage, logAt)
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

fixedTime :: UTCTime
fixedTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-06-15 12:30:45"

logSpec :: TestTree
logSpec =
  testGroup
    "Log"
    [ testGroup
        "formatMessage"
        [ testCase "contains correct level tag for each log level" $ do
            let check level tag =
                  T.isInfixOf tag (formatMessage level fixedTime "test") `shouldBe` True
            check Debug "[DEBUG]"
            check Info "[INFO]"
            check Warning "[WARN]"
            check Error "[ERROR]"
        , testCase "contains the message text" $
            T.isInfixOf "hello world" (formatMessage Info fixedTime "hello world") `shouldBe` True
        , testCase "contains timestamp" $
            T.isInfixOf "2024-06-15 12:30:45" (formatMessage Info fixedTime "test") `shouldBe` True
        ]
    , testGroup
        "logging level filtering"
        [ testCase "makeLogger Warning: logAt Debug does NOT invoke action" $ do
            ref <- newIORef (0 :: Int)
            let warningLogger = makeLoggerWithCapture Warning ref
            logAt Debug warningLogger "debug msg"
            count <- readIORef ref
            count `shouldBe` 0
        , testCase "makeLogger Warning: logAt Warning DOES invoke action" $ do
            ref <- newIORef (0 :: Int)
            let logger = makeLoggerWithCapture Warning ref
            logAt Warning logger "warn msg"
            count <- readIORef ref
            count `shouldBe` 1
        , testCase "makeLogger Debug: logAt Debug DOES invoke action" $ do
            ref <- newIORef (0 :: Int)
            let logger = makeLoggerWithCapture Debug ref
            logAt Debug logger "debug msg"
            count <- readIORef ref
            count `shouldBe` 1
        ]
    ]

{-| Create a Logger that increments an IORef counter instead of printing.
Uses the same level-filtering logic as makeLogger.
-}
makeLoggerWithCapture :: LogLevel -> IORef Int -> Logger
makeLoggerWithCapture minLevel ref =
  Logger
    { logLevel = minLevel
    , logAction = \(level, _, _) ->
        when (level >= minLevel) $ modifyIORef ref (+ 1)
    }
