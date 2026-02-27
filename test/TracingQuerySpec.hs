module TracingQuerySpec (tracingQuerySpec) where

import Data.Text qualified as T
import Test.Hspec
import Tracing.Query (buildTraceQL)
import Tracing.Types (Nanoseconds (..), TraceQuery (..))

tracingQuerySpec :: Spec
tracingQuerySpec = describe "Tracing.Query.buildTraceQL" $ do
  it "always includes the service name" $ do
    let q = baseQuery {queryService = "my-service"}
    let result = buildTraceQL q
    result `shouldSatisfy` T.isInfixOf "my-service"

  it "wraps output in curly braces" $ do
    let result = buildTraceQL baseQuery
    T.isPrefixOf "{" result `shouldBe` True
    T.isSuffixOf "}" result `shouldBe` True

  it "uses resource.service.name key" $ do
    let result = buildTraceQL baseQuery
    result `shouldSatisfy` T.isInfixOf "resource.service.name"

  it "includes span name when Just" $ do
    let q = baseQuery {querySpanName = Just "GET /api/users"}
    let result = buildTraceQL q
    result `shouldSatisfy` T.isInfixOf "GET /api/users"
    result `shouldSatisfy` T.isInfixOf "name="

  it "excludes span name when Nothing" $ do
    let q = baseQuery {querySpanName = Nothing}
    let result = buildTraceQL q
    -- span name would appear as `name="..."` (no dot-prefix)
    -- service name appears as `resource.service.name="..."` - won't false-match
    result `shouldBe` "{resource.service.name=\"test-service\"}"

  it "includes min duration when Just" $ do
    let q = baseQuery {queryMinDuration = Just "100ms"}
    let result = buildTraceQL q
    result `shouldSatisfy` T.isInfixOf "100ms"
    result `shouldSatisfy` T.isInfixOf "duration>"

  it "excludes min duration when Nothing" $ do
    let q = baseQuery {queryMinDuration = Nothing}
    let result = buildTraceQL q
    result `shouldSatisfy` (not . T.isInfixOf "duration>")

  it "joins multiple conditions with &&" $ do
    let q = baseQuery {querySpanName = Just "op", queryMinDuration = Just "50ms"}
    let result = buildTraceQL q
    result `shouldSatisfy` T.isInfixOf " && "

  it "single condition has no && separator" $ do
    -- Only service name (no span name, no min duration)
    let result = buildTraceQL baseQuery
    result `shouldSatisfy` (not . T.isInfixOf " && ")

  it "two conditions produce one && separator" $ do
    let q = baseQuery {querySpanName = Just "op"}
    let result = buildTraceQL q
    let parts = T.splitOn " && " result
    length parts `shouldBe` 2

  it "three conditions (service + span + duration) produce two && separators" $ do
    let q = baseQuery {querySpanName = Just "op", queryMinDuration = Just "50ms"}
    let result = buildTraceQL q
    let parts = T.splitOn " && " result
    length parts `shouldBe` 3

  it "three conditions include all fields" $ do
    let q = baseQuery {querySpanName = Just "GET /api", queryMinDuration = Just "100ms"}
    let result = buildTraceQL q
    result `shouldSatisfy` T.isInfixOf "resource.service.name=\"test-service\""
    result `shouldSatisfy` T.isInfixOf "name=\"GET /api\""
    result `shouldSatisfy` T.isInfixOf "duration>100ms"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

baseQuery :: TraceQuery
baseQuery =
  TraceQuery
    { queryService = "test-service"
    , querySpanName = Nothing
    , queryStartNs = Nanoseconds 0
    , queryEndNs = Nanoseconds 1_000_000_000
    , queryMinDuration = Nothing
    }
