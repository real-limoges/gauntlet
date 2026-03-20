-- | TUI widget rendering: progress bars, histograms, and formatting helpers.
module Benchmark.TUI.Widgets
  ( progressBar
  , histogram
  , formatDuration
  , formatRPM
  , formatElapsed
  ) where

import Brick (Widget, hBox, txt, vBox)
import Data.Text (Text)
import Data.Text qualified as T

{-| Custom progress bar widget
Usage: progressBar 0.35 "Progress"
-}
progressBar :: Float -> Text -> Widget n
progressBar progress label =
  let pct = max 0 (min 1 progress)
      width = 40 :: Int
      filled = round (pct * fromIntegral width)
      empty = width - filled
      bar = T.replicate filled "█" <> T.replicate empty "░"
      pctText = T.pack $ show (round (pct * 100) :: Int) <> "%"
   in hBox
        [ txt label
        , txt " "
        , txt bar
        , txt " "
        , txt pctText
        ]

{-| Histogram widget
Takes list of (label, count) pairs
-}
histogram :: [(Text, Int)] -> Widget n
histogram buckets =
  let total = sum (map snd buckets)
      barWidth = 30 :: Int
      renderBucket (label, count) =
        let pct = if total > 0 then fromIntegral count / fromIntegral total else 0 :: Float
            filled = round (pct * fromIntegral barWidth * 0.5)
            bar = T.replicate filled "█" <> T.replicate (barWidth - filled) "░"
            pctText = T.pack $ show (round (pct * 100) :: Int) <> "%"
         in hBox
              [ txt (T.justifyRight 10 ' ' label)
              , txt " "
              , txt bar
              , txt " "
              , txt (T.justifyRight 4 ' ' pctText)
              ]
   in vBox (map renderBucket buckets)

-- | Format milliseconds nicely
formatDuration :: Double -> Text
formatDuration ms
  | ms < 1 = T.pack $ show (round (ms * 1000) :: Int) <> "µs"
  | ms < 1000 = T.pack $ show (round ms :: Int) <> "ms"
  | otherwise = T.pack $ show (fromIntegral (round (ms / 100) :: Int) / 10 :: Double) <> "s"

-- | Format requests per minute
formatRPM :: Double -> Text
formatRPM rpm
  | rpm < 1 = "<1 rpm"
  | rpm < 10 = T.pack $ show (fromIntegral (round (rpm * 10) :: Int) / 10 :: Double) <> " rpm"
  | otherwise = T.pack $ show (round rpm :: Int) <> " rpm"

-- | Format elapsed time as HH:MM:SS or MM:SS
formatElapsed :: Double -> Text
formatElapsed seconds
  | seconds < 0 = "00:00"
  | hours > 0 = T.pack $ padZero hours <> ":" <> padZero mins <> ":" <> padZero secs
  | otherwise = T.pack $ padZero mins <> ":" <> padZero secs
  where
    totalSecs = round seconds :: Int
    hours = totalSecs `div` 3600
    mins = (totalSecs `mod` 3600) `div` 60
    secs = totalSecs `mod` 60
    padZero n = if n < 10 then "0" <> show n else show n
