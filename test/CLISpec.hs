module CLISpec (cliSpec) where

import Benchmark.CLI
import Benchmark.Types (OutputFormat (..))
import Options.Applicative
import Test.Hspec

-- | Parse arguments using the commandParser in pure mode.
parse :: [String] -> Maybe Command
parse args =
  getParseResult $ execParserPure defaultPrefs (info (commandParser <**> helper) mempty) args

cliSpec :: Spec
cliSpec = describe "Benchmark.CLI" $ do
  describe "commandParser" $ do
    it "parses benchmark-nway --config foo.json" $ do
      parse ["benchmark-nway", "--config", "foo.json"]
        `shouldBe` Just (BenchmarkNway "foo.json" OutputTerminal)

    it "parses benchmark-single --config foo.json" $ do
      parse ["benchmark-single", "--config", "foo.json"]
        `shouldBe` Just (BenchmarkSingle "foo.json" NoBaseline OutputTerminal)

    it "parses benchmark-single with --save-baseline" $ do
      parse ["benchmark-single", "--config", "f.json", "--save-baseline", "v1"]
        `shouldBe` Just (BenchmarkSingle "f.json" (SaveBaseline "v1") OutputTerminal)

    it "parses benchmark-single with --compare-baseline" $ do
      parse ["benchmark-single", "--config", "f.json", "--compare-baseline", "v1"]
        `shouldBe` Just (BenchmarkSingle "f.json" (CompareBaseline "v1") OutputTerminal)

    it "parses benchmark-single with --save-baseline and --compare-baseline" $ do
      parse ["benchmark-single", "--config", "f.json", "--save-baseline", "v2", "--compare-baseline", "v1"]
        `shouldBe` Just (BenchmarkSingle "f.json" (SaveAndCompare "v2" "v1") OutputTerminal)

    it "parses verify --config foo.json" $ do
      parse ["verify", "--config", "foo.json"]
        `shouldBe` Just (Verify "foo.json" OutputTerminal)

    it "parses benchmark-nway with --output markdown" $ do
      parse ["benchmark-nway", "--config", "foo.json", "--output", "markdown"]
        `shouldBe` Just (BenchmarkNway "foo.json" (OutputMarkdown "results/report.md"))

    it "parses --output markdown --report-path custom.md" $ do
      parse ["benchmark-nway", "--config", "foo.json", "--output", "markdown", "--report-path", "custom.md"]
        `shouldBe` Just (BenchmarkNway "foo.json" (OutputMarkdown "custom.md"))

    it "fails on missing --config" $ do
      parse ["benchmark-nway"] `shouldBe` Nothing

    it "fails on unknown subcommand" $ do
      parse ["unknown-cmd", "--config", "foo.json"] `shouldBe` Nothing
