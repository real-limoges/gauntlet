module NwayIntegrationSpec (nwayIntegrationSpec) where

import Benchmark.Types
import Data.Text qualified as T
import MockServer (mockJson)
import Runner.Nway (runNway)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)
import System.IO (hClose, hFlush)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.Hspec
import TestHelpers (captureStdout)

nwayIntegrationSpec :: Spec
nwayIntegrationSpec = describe "N-Way Integration" $ do
  describe "runNway" $ do
    it "returns RunSuccess with 2 targets" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let cfg = makeTestNwayConfig tokenPath [port1, port2]
            result <- runNway OutputTerminal cfg
            result `shouldBe` RunSuccess

    it "returns RunSuccess with 3 targets" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 ->
            mockJson "{}" $ \port3 -> do
              let cfg = makeTestNwayConfig tokenPath [port1, port2, port3]
              result <- runNway OutputTerminal cfg
              result `shouldBe` RunSuccess

    it "stdout contains target names" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let cfg = makeTestNwayConfig tokenPath [port1, port2]
            output <- captureStdout $ do
              _ <- runNway OutputTerminal cfg
              pure ()
            output `shouldContain` "target-1"
            output `shouldContain` "target-2"

    it "stdout contains ranking table" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let cfg = makeTestNwayConfig tokenPath [port1, port2]
            output <- captureStdout $ do
              _ <- runNway OutputTerminal cfg
              pure ()
            T.pack output `shouldSatisfy` T.isInfixOf "Ranking"

    it "with OutputMarkdown writes a file" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let mdPath = "nway-report.md"
                cfg = makeTestNwayConfig tokenPath [port1, port2]
            result <- runNway (OutputMarkdown mdPath) cfg
            result `shouldBe` RunSuccess
            exists <- doesFileExist mdPath
            exists `shouldBe` True
            content <- readFile mdPath
            content `shouldContain` "target-1"
            content `shouldContain` "target-2"

    it "creates results directory with CSV output" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let cfg = makeTestNwayConfig tokenPath [port1, port2]
            _ <- runNway OutputTerminal cfg
            exists <- doesDirectoryExist "results"
            exists `shouldBe` True
            files <- listDirectory "results"
            let csvFiles = filter (T.isInfixOf ".csv" . T.pack) files
            csvFiles `shouldSatisfy` (not . null)

    it "handles POST endpoints" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let cfg = makeTestNwayConfigWithMethod tokenPath [port1, port2] "POST"
            result <- runNway OutputTerminal cfg
            result `shouldBe` RunSuccess

    it "with no branches skips git setup" $
      withNwayEnv $ \tokenPath ->
        mockJson "{}" $ \port1 ->
          mockJson "{}" $ \port2 -> do
            let cfg = makeTestNwayConfig tokenPath [port1, port2]
            -- All targets have targetBranch = Nothing, so no git/docker side effects
            result <- runNway OutputTerminal cfg
            result `shouldBe` RunSuccess

-- Helpers

-- | Run an action inside a fresh temp directory with a temp token file.
withNwayEnv :: (FilePath -> IO a) -> IO a
withNwayEnv action =
  withSystemTempDirectory "nway-test" $ \tmpDir ->
    withCurrentDirectory tmpDir $
      withTempToken $ \tokenPath ->
        action tokenPath

withTempToken :: (FilePath -> IO a) -> IO a
withTempToken action =
  withSystemTempFile "token" $ \path h -> do
    hFlush h
    hClose h
    writeFile path "test-token"
    action path

makeTestSettings :: FilePath -> Settings
makeTestSettings tokenPath =
  Settings
    { iterations = 5
    , concurrency = 2
    , secrets = T.pack tokenPath
    , maxConnections = Nothing
    , connIdleTimeout = Nothing
    , requestTimeout = Just 10
    , retry = Nothing
    , warmup = Nothing
    , logLevel = Nothing
    , tempo = Nothing
    , healthCheckPath = Nothing
    , healthCheckTimeout = Nothing
    , floatTolerance = Nothing
    , compareFields = Nothing
    , ignoreFields = Nothing
    , verifyIterations = Nothing
    }

makeTestNwayConfig :: FilePath -> [Int] -> NwayConfig
makeTestNwayConfig tokenPath ports =
  NwayConfig
    { nwayTargets =
        [ NamedTarget
            { targetName = T.pack ("target-" ++ show i)
            , targetUrl = T.pack ("http://127.0.0.1:" ++ show port)
            , targetBranch = Nothing
            }
        | (i, port) <- zip [(1 :: Int) ..] ports
        ]
    , nwaySettings = makeTestSettings tokenPath
    , nwayPayloads = [PayloadSpec "test" "GET" "/" Nothing Nothing Nothing]
    }

makeTestNwayConfigWithMethod :: FilePath -> [Int] -> T.Text -> NwayConfig
makeTestNwayConfigWithMethod tokenPath ports method =
  let cfg = makeTestNwayConfig tokenPath ports
   in cfg {nwayPayloads = [PayloadSpec "test" method "/" Nothing Nothing Nothing]}
