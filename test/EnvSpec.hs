module EnvSpec (envSpec) where

import Benchmark.Config.Env (interpolateEnv, loadEnvVars, parseEnvFile)
import Benchmark.Config.Loader (loadConfig)
import Benchmark.Types
import Data.Map.Strict qualified as Map
import System.Directory (withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import TastyCompat (shouldBe, shouldContain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

envSpec :: TestTree
envSpec =
  testGroup
    "Benchmark.Env"
    [ testGroup "parseEnvFile" parseEnvFileTests
    , testGroup "interpolateEnv" interpolateEnvTests
    , testGroup "loadEnvVars" loadEnvVarsTests
    , testGroup "full pipeline" pipelineTests
    ]

parseEnvFileTests :: [TestTree]
parseEnvFileTests =
  [ testCase "parses simple KEY=VALUE" $ do
      let result = parseEnvFile "FOO=bar"
      Map.lookup "FOO" result `shouldBe` Just "bar"
  , testCase "parses multiple lines" $ do
      let result = parseEnvFile "A=1\nB=2\nC=3"
      Map.lookup "A" result `shouldBe` Just "1"
      Map.lookup "B" result `shouldBe` Just "2"
      Map.lookup "C" result `shouldBe` Just "3"
  , testCase "skips comment lines" $ do
      let result = parseEnvFile "# this is a comment\nFOO=bar"
      Map.size result `shouldBe` 1
      Map.lookup "FOO" result `shouldBe` Just "bar"
  , testCase "skips blank lines" $ do
      let result = parseEnvFile "\n\nFOO=bar\n\n"
      Map.size result `shouldBe` 1
  , testCase "strips export prefix" $ do
      let result = parseEnvFile "export FOO=bar"
      Map.lookup "FOO" result `shouldBe` Just "bar"
  , testCase "strips double-quoted values" $ do
      let result = parseEnvFile "FOO=\"hello world\""
      Map.lookup "FOO" result `shouldBe` Just "hello world"
  , testCase "strips single-quoted values" $ do
      let result = parseEnvFile "FOO='hello world'"
      Map.lookup "FOO" result `shouldBe` Just "hello world"
  , testCase "leaves unquoted values as-is" $ do
      let result = parseEnvFile "FOO=hello"
      Map.lookup "FOO" result `shouldBe` Just "hello"
  , testCase "skips lines without equals sign" $ do
      let result = parseEnvFile "NOEQUALS"
      Map.size result `shouldBe` 0
  , testCase "value may contain equals sign" $ do
      let result = parseEnvFile "FOO=a=b=c"
      Map.lookup "FOO" result `shouldBe` Just "a=b=c"
  ]

interpolateEnvTests :: [TestTree]
interpolateEnvTests =
  [ testCase "substitutes single variable" $ do
      let env = Map.fromList [("HOST", "localhost")]
      interpolateEnv env "${HOST}" `shouldBe` Right "localhost"
  , testCase "substitutes variable in context" $ do
      let env = Map.fromList [("HOST", "localhost"), ("PORT", "8080")]
      interpolateEnv env "http://${HOST}:${PORT}/api" `shouldBe` Right "http://localhost:8080/api"
  , testCase "leaves plain text unchanged" $ do
      interpolateEnv Map.empty "no variables here" `shouldBe` Right "no variables here"
  , testCase "returns Left on undefined variable" $ do
      case interpolateEnv Map.empty "${MISSING}" of
        Left err -> err `shouldContain` "MISSING"
        Right _ -> assertFailure "Expected Left for undefined variable"
  , testCase "substitutes adjacent variables" $ do
      let env = Map.fromList [("A", "foo"), ("B", "bar")]
      interpolateEnv env "${A}${B}" `shouldBe` Right "foobar"
  , testCase "leaves unclosed ${ as literal" $ do
      interpolateEnv Map.empty "prefix ${unclosed" `shouldBe` Right "prefix ${unclosed"
  , testCase "empty input returns empty output" $ do
      interpolateEnv Map.empty "" `shouldBe` Right ""
  ]

loadEnvVarsTests :: [TestTree]
loadEnvVarsTests =
  [ testCase ".env.local overrides .env" $ do
      withSystemTempDirectory "gauntlet-env-test" $ \tmpDir -> do
        writeFile (tmpDir </> ".env") "KEY=from_dotenv\n"
        writeFile (tmpDir </> ".env.local") "KEY=from_local\n"
        result <- withCurrentDirectory tmpDir loadEnvVars
        Map.lookup "KEY" result `shouldBe` Just "from_local"
  , testCase ".env is loaded when .env.local is absent" $ do
      withSystemTempDirectory "gauntlet-env-test" $ \tmpDir -> do
        writeFile (tmpDir </> ".env") "ONLY_IN_DOTENV=yes\n"
        result <- withCurrentDirectory tmpDir loadEnvVars
        Map.lookup "ONLY_IN_DOTENV" result `shouldBe` Just "yes"
  , testCase "missing .env files are silently ignored" $ do
      withSystemTempDirectory "gauntlet-env-test" $ \tmpDir -> do
        result <- withCurrentDirectory tmpDir loadEnvVars
        Map.lookup "__GAUNTLET_NONEXISTENT_12345__" result `shouldBe` Nothing
  ]

pipelineTests :: [TestTree]
pipelineTests =
  [ testCase "loadConfig interpolates ${VAR} from .env" $ do
      withSystemTempDirectory "gauntlet-pipeline-test" $ \tmpDir -> do
        writeFile (tmpDir </> ".env") "PRIMARY_URL=http://localhost:8080\nCANDIDATE_URL=http://localhost:9090\n"
        writeFile (tmpDir </> "config.json") (minimalConfig "${PRIMARY_URL}" "${CANDIDATE_URL}")
        result <- withCurrentDirectory tmpDir $ loadConfig (tmpDir </> "config.json")
        case result of
          Right tc -> do
            primary (targets tc) `shouldBe` "http://localhost:8080"
            candidate (targets tc) `shouldBe` "http://localhost:9090"
          Left err -> assertFailure $ "Expected successful parse: " ++ err
  , testCase "loadConfig returns error for undefined variable" $ do
      withSystemTempDirectory "gauntlet-pipeline-test" $ \tmpDir -> do
        writeFile (tmpDir </> "config.json") (minimalConfig "${UNDEFINED_PRIMARY}" "http://localhost:9090")
        result <- withCurrentDirectory tmpDir $ loadConfig (tmpDir </> "config.json")
        case result of
          Left err -> err `shouldContain` "UNDEFINED_PRIMARY"
          Right _ -> assertFailure "Expected Left for undefined variable"
  ]

minimalConfig :: String -> String -> String
minimalConfig primaryUrl candidateUrl =
  unlines
    [ "{"
    , "  \"targets\": {"
    , "    \"primary\": \"" ++ primaryUrl ++ "\","
    , "    \"candidate\": \"" ++ candidateUrl ++ "\""
    , "  },"
    , "  \"git\": {"
    , "    \"primary\": \"main\","
    , "    \"candidate\": \"feature\""
    , "  },"
    , "  \"settings\": {"
    , "    \"iterations\": 10,"
    , "    \"concurrency\": 2,"
    , "    \"secrets\": \"secrets.txt\""
    , "  },"
    , "  \"payloads\": ["
    , "    { \"name\": \"test\", \"method\": \"GET\", \"path\": \"/health\" }"
    , "  ]"
    , "}"
    ]
