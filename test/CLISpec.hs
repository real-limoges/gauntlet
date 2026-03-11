module CLISpec (cliSpec) where

import Benchmark.Config.CLI
import Benchmark.Types (OutputFormat (..))
import Options.Applicative
import TastyCompat (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- | Parse arguments using the commandParser in pure mode.
parse :: [String] -> Maybe Command
parse args =
  getParseResult $ execParserPure defaultPrefs (info (commandParser <**> helper) mempty) args

cliSpec :: TestTree
cliSpec =
  testGroup
    "Benchmark.CLI"
    [ testGroup
        "commandParser"
        [ testCase "parses benchmark-nway --config foo.json" $ do
            parse ["benchmark-nway", "--config", "foo.json"]
              `shouldBe` Just (BenchmarkNway "foo.json" NoBaseline OutputTerminal)
        , testCase "parses benchmark-single --config foo.json" $ do
            parse ["benchmark-single", "--config", "foo.json"]
              `shouldBe` Just (BenchmarkSingle "foo.json" NoBaseline OutputTerminal)
        , testCase "parses benchmark-single with --save-baseline" $ do
            parse ["benchmark-single", "--config", "f.json", "--save-baseline", "v1"]
              `shouldBe` Just (BenchmarkSingle "f.json" (SaveBaseline "v1") OutputTerminal)
        , testCase "parses benchmark-single with --compare-baseline" $ do
            parse ["benchmark-single", "--config", "f.json", "--compare-baseline", "v1"]
              `shouldBe` Just (BenchmarkSingle "f.json" (CompareBaseline "v1") OutputTerminal)
        , testCase "parses benchmark-single with --save-baseline and --compare-baseline" $ do
            parse ["benchmark-single", "--config", "f.json", "--save-baseline", "v2", "--compare-baseline", "v1"]
              `shouldBe` Just (BenchmarkSingle "f.json" (SaveAndCompare "v2" "v1") OutputTerminal)
        , testCase "parses benchmark-nway with --output markdown" $ do
            parse ["benchmark-nway", "--config", "foo.json", "--output", "markdown"]
              `shouldBe` Just (BenchmarkNway "foo.json" NoBaseline (OutputMarkdown "results/report.md"))
        , testCase "parses --output markdown --report-path custom.md" $ do
            parse ["benchmark-nway", "--config", "foo.json", "--output", "markdown", "--report-path", "custom.md"]
              `shouldBe` Just (BenchmarkNway "foo.json" NoBaseline (OutputMarkdown "custom.md"))
        , testCase "parses benchmark-nway with --save-baseline" $ do
            parse ["benchmark-nway", "--config", "test.json", "--save-baseline", "foo"]
              `shouldBe` Just (BenchmarkNway "test.json" (SaveBaseline "foo") OutputTerminal)
        , testCase "parses benchmark-nway with --compare-baseline" $ do
            parse ["benchmark-nway", "--config", "test.json", "--compare-baseline", "bar"]
              `shouldBe` Just (BenchmarkNway "test.json" (CompareBaseline "bar") OutputTerminal)
        , testCase "parses benchmark-nway with --save-baseline and --compare-baseline" $ do
            parse ["benchmark-nway", "--config", "t.json", "--save-baseline", "v2", "--compare-baseline", "v1"]
              `shouldBe` Just (BenchmarkNway "t.json" (SaveAndCompare "v2" "v1") OutputTerminal)
        , testCase "fails on missing --config" $ do
            parse ["benchmark-nway"] `shouldBe` Nothing
        , testCase "fails on unknown subcommand" $ do
            parse ["unknown-cmd", "--config", "foo.json"] `shouldBe` Nothing
        ]
    , testGroup
        "compare"
        [ testCase "parses compare a.json b.json" $ do
            parse ["compare", "a.json", "b.json"]
              `shouldBe` Just (Compare "a.json" "b.json")
        , testCase "fails on missing arguments" $ do
            parse ["compare"] `shouldBe` Nothing
        ]
    , testGroup
        "validate"
        [ testCase "parses validate --config c.json" $ do
            parse ["validate", "--config", "c.json"]
              `shouldBe` Just (Validate "c.json" False)
        , testCase "parses validate --config c.json --check-endpoints" $ do
            parse ["validate", "--config", "c.json", "--check-endpoints"]
              `shouldBe` Just (Validate "c.json" True)
        ]
    ]
