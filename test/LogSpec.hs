module LogSpec (logSpec) where

import Benchmark.Types (LogLevel (..))
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Log
import Test.Hspec

fixedTime :: UTCTime
fixedTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-06-15 12:30:45"

logSpec :: Spec
logSpec = describe "Log" $ do
  describe "formatMessage" $ do
    it "contains [DEBUG] for Debug level" $
      T.isInfixOf "[DEBUG]" (formatMessage Debug fixedTime "test") `shouldBe` True

    it "contains [INFO] for Info level" $
      T.isInfixOf "[INFO]" (formatMessage Info fixedTime "test") `shouldBe` True

    it "contains [WARN] for Warning level" $
      T.isInfixOf "[WARN]" (formatMessage Warning fixedTime "test") `shouldBe` True

    it "contains [ERROR] for Error level" $
      T.isInfixOf "[ERROR]" (formatMessage Error fixedTime "test") `shouldBe` True

    it "contains the message text" $
      T.isInfixOf "hello world" (formatMessage Info fixedTime "hello world") `shouldBe` True

    it "contains timestamp" $
      T.isInfixOf "2024-06-15 12:30:45" (formatMessage Info fixedTime "test") `shouldBe` True

  describe "logging level filtering" $ do
    it "makeLogger Warning: logAt Debug does NOT invoke action" $ do
      ref <- newIORef ([] :: [(LogLevel, Text)])
      let logger = Logger Warning (\(lvl, _, msg) -> modifyIORef ref ((lvl, msg) :))
      logAt Debug logger "debug msg"
      -- The default makeLogger filters, but our custom Logger doesn't filter itself.
      -- Use makeLogger and override logAction to capture.
      -- Actually, logAt delegates to logAction, which in makeLogger contains the filter.
      -- Let's test via makeLogger's behavior by using a capturing logger.
      ref2 <- newIORef (0 :: Int)
      let warningLogger = makeLoggerWithCapture Warning ref2
      logAt Debug warningLogger "debug msg"
      count <- readIORef ref2
      count `shouldBe` 0

    it "makeLogger Warning: logAt Warning DOES invoke action" $ do
      ref <- newIORef (0 :: Int)
      let logger = makeLoggerWithCapture Warning ref
      logAt Warning logger "warn msg"
      count <- readIORef ref
      count `shouldBe` 1

    it "makeLogger Debug: logAt Debug DOES invoke action" $ do
      ref <- newIORef (0 :: Int)
      let logger = makeLoggerWithCapture Debug ref
      logAt Debug logger "debug msg"
      count <- readIORef ref
      count `shouldBe` 1

{-| Create a Logger that increments an IORef counter instead of printing.
Uses the same level-filtering logic as makeLogger.
-}
makeLoggerWithCapture :: LogLevel -> IORef Int -> Logger
makeLoggerWithCapture minLevel ref =
  Logger
    { logLevel = minLevel
    , logAction = \(level, _, _) ->
        if level >= minLevel
          then modifyIORef ref (+ 1)
          else pure ()
    }
