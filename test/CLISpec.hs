-- | Tests for Benchmark.Config.CLI.
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
        [ testCase "parses benchmark --config foo.json" $ do
            parse ["benchmark", "--config", "foo.json"]
              `shouldBe` Just (Benchmark "foo.json" NoBaseline OutputTerminal Nothing)
        , testCase "parses benchmark with --save-baseline" $ do
            parse ["benchmark", "--config", "f.json", "--save-baseline", "v1"]
              `shouldBe` Just (Benchmark "f.json" (SaveBaseline "v1") OutputTerminal Nothing)
        , testCase "parses benchmark with --compare-baseline" $ do
            parse ["benchmark", "--config", "f.json", "--compare-baseline", "v1"]
              `shouldBe` Just (Benchmark "f.json" (CompareBaseline "v1") OutputTerminal Nothing)
        , testCase "parses benchmark with --save-baseline and --compare-baseline" $ do
            parse ["benchmark", "--config", "f.json", "--save-baseline", "v2", "--compare-baseline", "v1"]
              `shouldBe` Just (Benchmark "f.json" (SaveAndCompare "v2" "v1") OutputTerminal Nothing)
        , testCase "parses benchmark with --output markdown" $ do
            parse ["benchmark", "--config", "foo.json", "--output", "markdown"]
              `shouldBe` Just (Benchmark "foo.json" NoBaseline (OutputMarkdown "results/report.md") Nothing)
        , testCase "parses benchmark with --output markdown --report-path custom.md" $ do
            parse ["benchmark", "--config", "foo.json", "--output", "markdown", "--report-path", "custom.md"]
              `shouldBe` Just (Benchmark "foo.json" NoBaseline (OutputMarkdown "custom.md") Nothing)
        , testCase "fails on missing --config" $ do
            parse ["benchmark"] `shouldBe` Nothing
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
