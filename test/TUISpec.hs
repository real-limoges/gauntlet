module TUISpec (tuiStateSpec, tuiWidgetsSpec) where

import Benchmark.TUI.State
import Benchmark.TUI.Widgets
import Benchmark.Types (Nanoseconds (..))
import Control.Monad (forM_)
import Data.Sequence qualified as Seq
import Data.Time (addUTCTime, getCurrentTime)
import Test.Hspec

tuiStateSpec :: Spec
tuiStateSpec = describe "Benchmark.TUI.State" $ do
    describe "initialState" $ do
        it "creates state with correct target, totals, and zeroed counters" $ do
            let state = initialState "http://test.com" 100 5
            _tsTarget state `shouldBe` "http://test.com"
            _tsIsTotal state `shouldBe` 100
            _tsTotalEndpoints state `shouldBe` 5
            _tsCompleted state `shouldBe` 0
            _tsSuccessCount state `shouldBe` 0
            _tsErrorCount state `shouldBe` 0
            _tsBuckets state `shouldBe` [0, 0, 0, 0, 0, 0]
            _tsFinished state `shouldBe` False

        it "starts with empty rolling stats and durations" $ do
            let state = initialState "http://test.com" 100 5
            _tsRollingStats state `shouldBe` Nothing
            Seq.null (_tsRecentDurations state) `shouldBe` True

    describe "updateState with RequestCompleted" $ do
        it "increments completed count" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 50_000_000) 200
                newState = updateState now event state
            _tsCompleted newState `shouldBe` 1

        forM_ [(200, 1, 0), (301, 1, 0), (404, 0, 1), (500, 0, 1)] $
            \(code, expSuccess, expError) ->
                it ("categorizes status " ++ show (code :: Int) ++ " correctly") $ do
                    now <- getCurrentTime
                    let state = initialState "http://test.com" 100 5
                        event = RequestCompleted (Nanoseconds 50_000_000) code
                        newState = updateState now event state
                    _tsSuccessCount newState `shouldBe` expSuccess
                    _tsErrorCount newState `shouldBe` expError

        it "updates rolling stats" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 50_000_000) 200
                newState = updateState now event state
            _tsRollingStats newState `shouldSatisfy` (/= Nothing)

        it "adds duration to recent durations" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 50_000_000) 200
                newState = updateState now event state
            Seq.length (_tsRecentDurations newState) `shouldBe` 1

        it "sets start time on first request" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 50_000_000) 200
                newState = updateState now event state
            _tsStartTime newState `shouldBe` Just now

        it "does not change start time on subsequent requests" $ do
            now <- getCurrentTime
            let later = addUTCTime 1 now
                state = initialState "http://test.com" 100 5
                event1 = RequestCompleted (Nanoseconds 50_000_000) 200
                event2 = RequestCompleted (Nanoseconds 60_000_000) 200
                state1 = updateState now event1 state
                state2 = updateState later event2 state1
            _tsStartTime state2 `shouldBe` Just now

    describe "updateState with RequestFailed" $ do
        it "increments completed and error count" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestFailed "Connection timeout"
                newState = updateState now event state
            _tsCompleted newState `shouldBe` 1
            _tsErrorCount newState `shouldBe` 1
            _tsSuccessCount newState `shouldBe` 0

        it "adds error to recent errors" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestFailed "Connection timeout"
                newState = updateState now event state
            Seq.length (_tsRecentErrors newState) `shouldBe` 1

        it "keeps only last 5 errors" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                addError s = updateState now (RequestFailed "error") s
                finalState = iterate addError state !! 10
            Seq.length (_tsRecentErrors finalState) `shouldBe` 5

    describe "updateState with EndpointStarted" $ do
        it "updates current endpoint" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = EndpointStarted "/api/users" 2 5
                newState = updateState now event state
            _tsCurrentEndpoint newState `shouldBe` "/api/users"
            _tsEndpointIndex newState `shouldBe` 2
            _tsTotalEndpoints newState `shouldBe` 5

    describe "updateState with BenchmarkFinished" $ do
        it "sets finished flag" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = BenchmarkFinished
                newState = updateState now event state
            _tsFinished newState `shouldBe` True

    describe "bucket updates" $ do
        it "updates bucket 0 for <5s" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 4_000_000_000) 200
                newState = updateState now event state
            _tsBuckets newState `shouldBe` [1, 0, 0, 0, 0, 0]

        it "updates bucket 1 for 5s-7.5s" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 6_000_000_000) 200
                newState = updateState now event state
            _tsBuckets newState `shouldBe` [0, 1, 0, 0, 0, 0]

        it "updates bucket 2 for 7.5s-10s" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 8_000_000_000) 200
                newState = updateState now event state
            _tsBuckets newState `shouldBe` [0, 0, 1, 0, 0, 0]

        it "updates bucket 3 for 10s-12.5s" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 11_000_000_000) 200
                newState = updateState now event state
            _tsBuckets newState `shouldBe` [0, 0, 0, 1, 0, 0]

        it "updates bucket 4 for 12.5s-15s" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 13_000_000_000) 200
                newState = updateState now event state
            _tsBuckets newState `shouldBe` [0, 0, 0, 0, 1, 0]

        it "updates bucket 5 for >15s" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 100 5
                event = RequestCompleted (Nanoseconds 20_000_000_000) 200
                newState = updateState now event state
            _tsBuckets newState `shouldBe` [0, 0, 0, 0, 0, 1]

    describe "rolling window" $ do
        it "rollingWindow is 100" $ do
            rollingWindow `shouldBe` 100

        it "limits durations to rolling window size" $ do
            now <- getCurrentTime
            let state = initialState "http://test.com" 200 1
                addRequest s = updateState now (RequestCompleted (Nanoseconds 50_000_000) 200) s
                finalState = iterate addRequest state !! 150
            Seq.length (_tsRecentDurations finalState) `shouldBe` 100

tuiWidgetsSpec :: Spec
tuiWidgetsSpec = describe "Benchmark.TUI.Widgets" $ do
    describe "formatDuration" $ do
        it "formats microseconds for <1ms" $ do
            formatDuration 0.5 `shouldBe` "500µs"

        it "formats milliseconds for 1-1000ms" $ do
            formatDuration 50 `shouldBe` "50ms"
            formatDuration 999 `shouldBe` "999ms"

        it "formats seconds for >1000ms" $ do
            formatDuration 1500 `shouldBe` "1.5s"
            formatDuration 5000 `shouldBe` "5.0s"

        it "handles zero" $ do
            formatDuration 0 `shouldBe` "0µs"

    describe "formatRPS" $ do
        it "formats low RPS with decimal" $ do
            formatRPS 0.5 `shouldBe` "0.5 rps"

        it "formats very low RPS" $ do
            formatRPS 0.05 `shouldBe` "<0.1 rps"

        it "formats integer RPS" $ do
            formatRPS 42.3 `shouldBe` "42 rps"
            formatRPS 100 `shouldBe` "100 rps"

    describe "formatElapsed" $ do
        it "formats seconds as MM:SS" $ do
            formatElapsed 65 `shouldBe` "01:05"
            formatElapsed 0 `shouldBe` "00:00"
            formatElapsed 59 `shouldBe` "00:59"

        it "formats minutes correctly" $ do
            formatElapsed 125 `shouldBe` "02:05"
            formatElapsed 600 `shouldBe` "10:00"

        it "formats hours as HH:MM:SS" $ do
            formatElapsed 3661 `shouldBe` "01:01:01"
            formatElapsed 7200 `shouldBe` "02:00:00"

        it "handles negative values" $ do
            formatElapsed (-10) `shouldBe` "00:00"
