{-# LANGUAGE TemplateHaskell #-}

module Benchmark.TUI.State (
    -- * Types
    TUIState (..),
    BenchmarkEvent (..),
    RollingStats (..),

    -- * Functions
    initialState,
    updateState,
    rollingWindow,

    -- * TUIState lenses
    tsTarget,
    tsCurrentEndpoint,
    tsEndpointIndex,
    tsTotalEndpoints,
    tsCompleted,
    tsIsTotal,
    tsSuccessCount,
    tsErrorCount,
    tsStartTime,
    tsRecentDurations,
    tsRecentErrors,
    tsRollingStats,
    tsBuckets,
    tsFinished,

    -- * RollingStats lenses
    rsMeanMs,
    rsP50Ms,
    rsP95Ms,
    rsP99Ms,
    rsMinMs,
    rsMaxMs,
) where

import Benchmark.Types (BenchmarkStats (..), Milliseconds (..), Nanoseconds, nsToMs)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Lens.Micro.TH (makeLenses)

-- | Size of rolling window
rollingWindow :: Int
rollingWindow = 100

-- | Events emitted by runner
data BenchmarkEvent
    = RequestCompleted Nanoseconds Int
    | RequestFailed Text
    | EndpointStarted Text Int Int
    | BenchmarkFinished
    deriving (Show, Eq)

data RollingStats = RollingStats
    { _rsMeanMs :: Double
    , _rsP50Ms :: Double
    , _rsP95Ms :: Double
    , _rsP99Ms :: Double
    , _rsMinMs :: Double
    , _rsMaxMs :: Double
    }
    deriving (Show, Eq)

makeLenses ''RollingStats

data TUIState = TUIState
    { _tsTarget :: Text
    , _tsCurrentEndpoint :: Text
    , _tsEndpointIndex :: Int
    , _tsTotalEndpoints :: Int
    , _tsCompleted :: Int
    , _tsIsTotal :: Int
    , _tsSuccessCount :: Int
    , _tsErrorCount :: Int
    , _tsStartTime :: Maybe UTCTime
    , _tsRecentDurations :: Seq Double
    , _tsRecentErrors :: Seq (UTCTime, Text)
    , _tsRollingStats :: Maybe RollingStats
    , _tsBuckets :: [Int]
    , _tsFinished :: Bool
    }
    deriving (Show, Eq)

makeLenses ''TUIState

initialState :: Text -> Int -> Int -> TUIState
initialState target total endpoints =
    TUIState
        { _tsTarget = target
        , _tsCurrentEndpoint = ""
        , _tsEndpointIndex = 0
        , _tsTotalEndpoints = endpoints
        , _tsCompleted = 0
        , _tsIsTotal = total
        , _tsSuccessCount = 0
        , _tsErrorCount = 0
        , _tsStartTime = Nothing
        , _tsRecentDurations = Seq.empty
        , _tsRecentErrors = Seq.empty
        , _tsRollingStats = Nothing
        , _tsBuckets = [0, 0, 0, 0, 0, 0]
        , _tsFinished = False
        }

updateState :: UTCTime -> BenchmarkEvent -> TUIState -> TUIState
updateState now event state = case event of
    RequestCompleted durationNs statusCode ->
        -- this decomposition makes ms a double
        let ms = unMilliseconds $ nsToMs durationNs
            newDurations = addToRolling ms (_tsRecentDurations state)
            newStats = computeRollingStats newDurations
            newBuckets = updateBuckets ms (_tsBuckets state)
            isSuccess = statusCode >= 200 && statusCode < 400
         in state
                { _tsCompleted = _tsCompleted state + 1
                , _tsSuccessCount = _tsSuccessCount state + (if isSuccess then 1 else 0)
                , _tsErrorCount = _tsErrorCount state + (if isSuccess then 0 else 1)
                , _tsRecentDurations = newDurations
                , _tsRollingStats = Just newStats
                , _tsBuckets = newBuckets
                , _tsStartTime = _tsStartTime state <|> Just now
                }
    RequestFailed error ->
        let newErrors = addToRolling (now, error) (_tsRecentErrors state)
         in state
                { _tsCompleted = _tsCompleted state + 1
                , _tsErrorCount = _tsErrorCount state + 1
                , _tsRecentErrors = Seq.take 5 newErrors
                }
    EndpointStarted endpoint index total ->
        state
            { _tsCurrentEndpoint = endpoint
            , _tsEndpointIndex = index
            , _tsTotalEndpoints = total
            }
    BenchmarkFinished ->
        state{_tsFinished = True}
  where
    unMilliseconds (Milliseconds ms) = ms

addToRolling :: a -> Seq a -> Seq a
addToRolling x xs
    | Seq.length xs >= rollingWindow = x Seq.<| Seq.deleteAt (Seq.length xs - 1) xs
    | otherwise = x Seq.<| xs

computeRollingStats :: Seq Double -> RollingStats
computeRollingStats durations
    | Seq.null durations = RollingStats 0 0 0 0 0 0
    | otherwise =
        let sorted = Seq.sort durations
            n = Seq.length sorted
            mean = sum durations / fromIntegral n
            percentile p = Seq.index sorted (min (n - 1) (floor (p * fromIntegral n)))
         in RollingStats
                { _rsMeanMs = mean
                , _rsP50Ms = percentile 0.50
                , _rsP95Ms = percentile 0.95
                , _rsP99Ms = percentile 0.99
                , _rsMinMs = Seq.index sorted 0
                , _rsMaxMs = Seq.index sorted (n - 1)
                }

updateBuckets :: Double -> [Int] -> [Int]
updateBuckets ms [b0, b1, b2, b3, b4, b5]
    | ms < 5_000 = [b0 + 1, b1, b2, b3, b4, b5]
    | ms < 7_500 = [b0, b1 + 1, b2, b3, b4, b5]
    | ms < 10_000 = [b0, b1, b2 + 1, b3, b4, b5]
    | ms < 12_500 = [b0, b1, b2, b3 + 1, b4, b5]
    | ms < 15_000 = [b0, b1, b2, b3, b4 + 1, b5]
    | otherwise = [b0, b1, b2, b3, b4, b5 + 1]

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> x = x
x <|> _ = x
