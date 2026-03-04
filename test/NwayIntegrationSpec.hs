module NwayIntegrationSpec (nwayIntegrationSpec) where

import Benchmark.CLI (BaselineMode (..))
import Benchmark.Types
import Data.Text qualified as T
import MockServer (mockJson)
import Runner.Nway (runNway)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)
import System.IO (hClose, hFlush)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import TastyCompat (shouldBe, shouldContain, shouldReturn, shouldSatisfy)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestHelpers (captureStdout)

nwayIntegrationSpec :: TestTree
nwayIntegrationSpec =
  testGroup
    "N-Way Integration"
    [ sequentialTestGroup
        "runNway"
        AllSucceed
        [ testCase "returns RunSuccess with 2 targets" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestNwayConfig tokenPath [port1, port2]
                  result <- runNway NoBaseline OutputTerminal cfg
                  result `shouldBe` RunSuccess
        , testCase "returns RunSuccess with 3 targets" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 ->
                  mockJson "{}" $ \port3 -> do
                    let cfg = makeTestNwayConfig tokenPath [port1, port2, port3]
                    result <- runNway NoBaseline OutputTerminal cfg
                    result `shouldBe` RunSuccess
        , testCase "stdout contains target names" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestNwayConfig tokenPath [port1, port2]
                  output <- captureStdout $ do
                    _ <- runNway NoBaseline OutputTerminal cfg
                    pure ()
                  output `shouldContain` "target-1"
                  output `shouldContain` "target-2"
        , testCase "stdout contains ranking table" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestNwayConfig tokenPath [port1, port2]
                  output <- captureStdout $ do
                    _ <- runNway NoBaseline OutputTerminal cfg
                    pure ()
                  T.pack output `shouldSatisfy` T.isInfixOf "Ranking"
        , testCase "with OutputMarkdown writes a file" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let mdPath = "nway-report.md"
                      cfg = makeTestNwayConfig tokenPath [port1, port2]
                  result <- runNway NoBaseline (OutputMarkdown mdPath) cfg
                  result `shouldBe` RunSuccess
                  exists <- doesFileExist mdPath
                  exists `shouldBe` True
                  content <- readFile mdPath
                  content `shouldContain` "target-1"
                  content `shouldContain` "target-2"
        , testCase "creates results directory with correct CSV output" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestNwayConfig tokenPath [port1, port2]
                  _ <- runNway NoBaseline OutputTerminal cfg
                  exists <- doesDirectoryExist "results"
                  exists `shouldBe` True
                  files <- listDirectory "results"
                  let csvFiles = filter (T.isInfixOf ".csv" . T.pack) files
                  csvFiles `shouldSatisfy` (not . null)
                  case csvFiles of
                    (f : _) -> do
                      csvContents <- T.pack <$> readFile ("results/" ++ f)
                      -- Check header
                      case T.lines csvContents of
                        (hdr : _) -> T.unpack hdr `shouldBe` "target_name,payload_id,url,status_code,latency_ms"
                        [] -> assertFailure "CSV file is empty"
                      -- Check target names in rows
                      T.isInfixOf "target-1" csvContents `shouldBe` True
                      T.isInfixOf "target-2" csvContents `shouldBe` True
                    [] -> assertFailure "No CSV files found"
        , testCase "handles POST endpoints" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestNwayConfigWithMethod tokenPath [port1, port2] "POST"
                  result <- runNway NoBaseline OutputTerminal cfg
                  result `shouldBe` RunSuccess
        , testCase "SaveBaseline creates per-target baseline files" $
            withNwayEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestNwayConfig tokenPath [port1, port2]
                  result <- runNway (SaveBaseline "test") OutputTerminal cfg
                  result `shouldBe` RunSuccess
                  doesFileExist "baselines/test--target-1.json" `shouldReturn` True
                  doesFileExist "baselines/test--target-2.json" `shouldReturn` True
        ]
    ]

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
    , secrets = Just (T.pack tokenPath)
    , maxConnections = Nothing
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
    , loadMode = Nothing
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
