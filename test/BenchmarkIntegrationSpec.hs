-- | Integration tests for benchmark comparison.
module BenchmarkIntegrationSpec (benchmarkIntegrationSpec) where

import Benchmark.Config.CLI (BaselineMode (..))
import Benchmark.Reporter (markdownReporter, noOpReporter)
import Benchmark.Types
import Data.Text qualified as T
import MockServer (mockJson)
import Runner.Benchmark (runBenchmark)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)
import System.IO (hClose, hFlush)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import TastyCompat (shouldBe, shouldContain, shouldReturn, shouldSatisfy)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

benchmarkIntegrationSpec :: TestTree
benchmarkIntegrationSpec =
  testGroup
    "Benchmark Integration"
    [ sequentialTestGroup
        "runBenchmark"
        AllSucceed
        [ testCase "returns RunSuccess with 2 targets" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfig tokenPath [port1, port2]
                  result <- runBenchmark noOpReporter NoBaseline Nothing cfg
                  result `shouldBe` RunSuccess
        , testCase "returns RunSuccess with 3 targets" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 ->
                  mockJson "{}" $ \port3 -> do
                    let cfg = makeTestBenchmarkConfig tokenPath [port1, port2, port3]
                    result <- runBenchmark noOpReporter NoBaseline Nothing cfg
                    result `shouldBe` RunSuccess
        , testCase "report contains target names" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfig tokenPath [port1, port2]
                  _ <- runBenchmark (markdownReporter "endpoint_analysis.md") NoBaseline Nothing cfg
                  content <- readFile "endpoint_analysis.md"
                  content `shouldContain` "target-1"
                  content `shouldContain` "target-2"
        , testCase "report contains ranking table" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfig tokenPath [port1, port2]
                  _ <- runBenchmark (markdownReporter "endpoint_analysis.md") NoBaseline Nothing cfg
                  content <- readFile "endpoint_analysis.md"
                  content `shouldContain` "Ranking"
        , testCase "writes endpoint_analysis.md" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfig tokenPath [port1, port2]
                  result <- runBenchmark (markdownReporter "endpoint_analysis.md") NoBaseline Nothing cfg
                  result `shouldBe` RunSuccess
                  exists <- doesFileExist "endpoint_analysis.md"
                  exists `shouldBe` True
                  content <- readFile "endpoint_analysis.md"
                  content `shouldContain` "target-1"
                  content `shouldContain` "target-2"
        , testCase "creates results directory with correct CSV output" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfig tokenPath [port1, port2]
                  _ <- runBenchmark noOpReporter NoBaseline Nothing cfg
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
                        (hdr : _) -> T.unpack hdr `shouldBe` "target_name,payload_id,url,method,status_code,latency_ms,timestamp_iso"
                        [] -> assertFailure "CSV file is empty"
                      -- Check target names in rows
                      T.isInfixOf "target-1" csvContents `shouldBe` True
                      T.isInfixOf "target-2" csvContents `shouldBe` True
                    [] -> assertFailure "No CSV files found"
        , testCase "handles POST endpoints" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfigWithMethod tokenPath [port1, port2] "POST"
                  result <- runBenchmark noOpReporter NoBaseline Nothing cfg
                  result `shouldBe` RunSuccess
        , testCase "SaveBaseline creates per-target baseline files" $
            withBenchmarkEnv $ \tokenPath ->
              mockJson "{}" $ \port1 ->
                mockJson "{}" $ \port2 -> do
                  let cfg = makeTestBenchmarkConfig tokenPath [port1, port2]
                  result <- runBenchmark noOpReporter (SaveBaseline "test") Nothing cfg
                  result `shouldBe` RunSuccess
                  doesFileExist "baselines/test--target-1.json" `shouldReturn` True
                  doesFileExist "baselines/test--target-2.json" `shouldReturn` True
        ]
    ]

-- Helpers

-- | Run an action inside a fresh temp directory with a temp token file.
withBenchmarkEnv :: (FilePath -> IO a) -> IO a
withBenchmarkEnv action =
  withSystemTempDirectory "benchmark-test" $ \tmpDir ->
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
    , loadMode = Nothing
    }

makeTestBenchmarkConfig :: FilePath -> [Int] -> BenchmarkConfig
makeTestBenchmarkConfig tokenPath ports =
  BenchmarkConfig
    { benchTargets =
        [ NamedTarget
            { targetName = T.pack ("target-" ++ show i)
            , targetUrl = T.pack ("http://127.0.0.1:" ++ show port)
            , targetBranch = Nothing
            , targetLifecycle = Nothing
            }
        | (i, port) <- zip [(1 :: Int) ..] ports
        ]
    , benchSettings = makeTestSettings tokenPath
    , benchPayloads = [PayloadSpec "test" "GET" "/" Nothing Nothing Nothing]
    }

makeTestBenchmarkConfigWithMethod :: FilePath -> [Int] -> T.Text -> BenchmarkConfig
makeTestBenchmarkConfigWithMethod tokenPath ports method =
  let cfg = makeTestBenchmarkConfig tokenPath ports
   in cfg {benchPayloads = [PayloadSpec "test" method "/" Nothing Nothing Nothing]}
