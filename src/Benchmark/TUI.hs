module Benchmark.TUI
  ( runTUI
  , TUIState
  , BenchmarkEvent (..)
  , initialState
  ) where

import Benchmark.TUI.State
import Benchmark.TUI.Widgets
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (hCenter)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, hPutStr, openTempFile, stderr)

data CustomEvent
  = Tick
  | BenchEvent BenchmarkEvent

type Name = ()

-- ── Runner ────────────────────────────────────────────────────────────────────

runTUI :: TChan BenchmarkEvent -> TUIState -> IO TUIState
runTUI eventChan initialSt = do
  brickChan <- newBChan 1000

  bracket
    ( do
        tid1 <- forkIO $ forever $ do
          event <- atomically $ readTChan eventChan
          writeBChan brickChan (BenchEvent event)
        tid2 <- forkIO $ forever $ do
          threadDelay 100_000
          writeBChan brickChan Tick
        return (tid1, tid2)
    )
    (\(tid1, tid2) -> killThread tid1 >> killThread tid2)
    ( \_ -> do
        let buildVty = mkVty V.defaultConfig
        initialVty <- buildVty
        withStderrBuffered $
          customMain initialVty buildVty (Just brickChan) tuiApp initialSt
    )

{-| Redirect stderr to a temp file for the duration of 'action', then replay
any captured output to the real stderr afterwards. Prevents log/error lines
from corrupting the Brick terminal while the TUI is active.
-}
withStderrBuffered :: IO a -> IO a
withStderrBuffered action =
  bracket acquire release (const action)
  where
    acquire = do
      stderrSaved <- hDuplicate stderr
      tmpDir <- getTemporaryDirectory
      (tmpPath, tmpHandle) <- openTempFile tmpDir "gauntlet-stderr.tmp"
      hDuplicateTo tmpHandle stderr
      hClose tmpHandle
      return (stderrSaved, tmpPath)
    release (stderrSaved, tmpPath) = do
      hFlush stderr
      hDuplicateTo stderrSaved stderr
      hClose stderrSaved
      content <- try (readFile tmpPath) :: IO (Either SomeException String)
      case content of
        Right s -> unless (null s) $ hPutStr stderr s
        Left _ -> return ()
      _ <- try (removeFile tmpPath) :: IO (Either SomeException ())
      return ()

tuiApp :: App TUIState CustomEvent Name
tuiApp =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theAttrMap
    }

-- ── Events ────────────────────────────────────────────────────────────────────

handleEvent :: BrickEvent Name CustomEvent -> EventM Name TUIState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
handleEvent (AppEvent Tick) = do
  now <- liftIO getCurrentTime
  modify $ \st -> case tsStartTime st of
    Nothing -> st
    Just start -> st {tsElapsedSecs = realToFrac (diffUTCTime now start)}
handleEvent (AppEvent (BenchEvent event)) = do
  now <- liftIO getCurrentTime
  modify (updateState now event)
  case event of
    BenchmarkFinished -> halt
    BenchmarkFailed _ -> halt
    _ -> return ()
handleEvent _ = return ()

-- ── Attributes ───────────────────────────────────────────────────────────────

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    V.defAttr
    [ (attrName "title", fg V.yellow)
    , (attrName "label", fg V.yellow)
    , (attrName "hi", fg V.cyan)
    , (attrName "stat", V.withStyle V.defAttr V.bold)
    , (attrName "ok", fg V.brightBlue)
    , (attrName "err", fg V.red)
    , (attrName "dim", fg V.brightBlack)
    , (attrName "progress", fg V.blue)
    , (attrName "warn", fg V.brightYellow)
    ]

-- ── Drawing ──────────────────────────────────────────────────────────────────

drawUI :: TUIState -> [Widget Name]
drawUI state = [borderWithLabel title (vBox sections)]
  where
    title = withAttr (attrName "title") (txt " gauntlet ")
    sections =
      [ headerSection state
      , hBorder
      , progressSection state
      , hBorder
      , statsSection state
      , hBorder
      , hBox [histogramSection state, requestTimelineSection state]
      , hBorder
      , errorsSection state
      , hBorder
      , footerSection
      ]

headerSection :: TUIState -> Widget Name
headerSection state =
  padLeftRight 1 $
    vBox
      [ row "Target  " $ withAttr (attrName "hi") $ txt (tsTarget state)
      , row "Phase   " $
          hBox
            [ txt $ tsCurrentEndpoint state
            , withAttr (attrName "dim") $
                txt $
                  "  ["
                    <> T.pack (show (tsEndpointIndex state))
                    <> "/"
                    <> T.pack (show (tsTotalEndpoints state))
                    <> "]"
            ]
      , if T.null (tsStatus state)
          then emptyWidget
          else row "Status  " $ withAttr (attrName "dim") $ txt (tsStatus state)
      ]
  where
    row label w = hBox [withAttr (attrName "label") (txt label), w]

progressSection :: TUIState -> Widget Name
progressSection state =
  padLeftRight 1 $
    vBox
      [ progressBar pct ""
      , hBox
          [ withAttr (attrName "dim") $ txt $ T.pack $ show completed <> "/" <> show total
          , txt $ "  " <> T.pack (show (round (pct * 100) :: Int)) <> "%"
          , if elapsed > 0
              then withAttr (attrName "dim") $ txt $ "   " <> formatElapsed elapsed
              else emptyWidget
          , case eta of
              Just e -> withAttr (attrName "dim") $ txt $ "  ETA " <> formatElapsed e
              Nothing -> emptyWidget
          , rpsWidget state
          ]
      ]
  where
    completed = tsCompleted state
    total = tsIsTotal state
    pct = if total > 0 then fromIntegral completed / fromIntegral total else 0 :: Float
    elapsed = tsElapsedSecs state
    eta =
      if completed > 0 && total > completed
        then Just $ elapsed * fromIntegral (total - completed) / fromIntegral completed
        else Nothing

rpsWidget :: TUIState -> Widget Name
rpsWidget state = case (tsCurrentRps state, tsTargetRps state, tsCurrentStep state) of
  (Just rps, Just target, Just step) ->
    hBox
      [ withAttr (attrName "dim") $ txt "   "
      , withAttr (attrName "hi") $ txt $ formatRPS rps
      , withAttr (attrName "dim") $ txt $ " / " <> formatRPS target
      , withAttr (attrName "dim") $ txt $ "  [step " <> T.pack (show step) <> "]"
      ]
  (Just rps, Just target, Nothing) ->
    hBox
      [ withAttr (attrName "dim") $ txt "   "
      , withAttr (attrName "hi") $ txt $ formatRPS rps
      , withAttr (attrName "dim") $ txt $ " / " <> formatRPS target
      ]
  (Just rps, Nothing, _) ->
    hBox
      [ withAttr (attrName "dim") $ txt "   "
      , withAttr (attrName "hi") $ txt $ formatRPS rps
      ]
  _ -> emptyWidget

statsSection :: TUIState -> Widget Name
statsSection state =
  padLeftRight 1 $ case tsRollingStats state of
    Nothing ->
      withAttr (attrName "dim") $ txt $ "Waiting for data...  (rolling last " <> T.pack (show rollingWindow) <> ")"
    Just stats ->
      vBox
        [ withAttr (attrName "dim") $ txt $ "Latency  (rolling last " <> T.pack (show rollingWindow) <> ")"
        , txt " "
        , hBox
            [ statBox "stat" "Mean" (formatDuration $ rsMeanMs stats)
            , statBox "stat" "P50 " (formatDuration $ rsP50Ms stats)
            , statBox (p95Attr stats) "P95 " (formatDuration $ rsP95Ms stats)
            , statBox (p99Attr stats) "P99 " (formatDuration $ rsP99Ms stats)
            , statBox "stat" "Min " (formatDuration $ rsMinMs stats)
            , statBox "stat" "Max " (formatDuration $ rsMaxMs stats)
            ]
        ]
  where
    statBox valueAttr label value =
      padRight (Pad 4) $
        vBox
          [ withAttr (attrName "label") $ txt label
          , withAttr (attrName valueAttr) $ txt value
          ]
    p99Attr stats
      | rsMeanMs stats > 0 && rsP99Ms stats > 2 * rsMeanMs stats = "err"
      | rsMeanMs stats > 0 && rsP99Ms stats > 1.5 * rsMeanMs stats = "warn"
      | otherwise = "stat"
    p95Attr stats
      | rsMeanMs stats > 0 && rsP95Ms stats > 2 * rsMeanMs stats = "err"
      | rsMeanMs stats > 0 && rsP95Ms stats > 1.5 * rsMeanMs stats = "warn"
      | otherwise = "stat"

histogramSection :: TUIState -> Widget Name
histogramSection state =
  borderWithLabel (withAttr (attrName "label") $ txt " Distribution ") $
    if null durations
      then withAttr (attrName "dim") $ txt "Waiting for data..."
      else histogram (zip labels counts)
  where
    durations = toList (tsRecentDurations state)
    lo = minimum durations
    hi = maximum durations
    nBuckets = 5 :: Int
    bucketWidth = if hi > lo then (hi - lo) / fromIntegral nBuckets else 1
    bucketIndex v = min (nBuckets - 1) (floor ((v - lo) / bucketWidth))
    counts = [length (filter (\v -> bucketIndex v == i) durations) | i <- [0 .. nBuckets - 1]]
    labels =
      [ formatDuration (lo + fromIntegral i * bucketWidth)
      | i <- [0 .. nBuckets - 1]
      ]

requestTimelineSection :: TUIState -> Widget Name
requestTimelineSection state =
  borderWithLabel (withAttr (attrName "label") $ txt " Requests ") $
    padLeftRight 1 $
      if Seq.null (tsRecentRequests state)
        then withAttr (attrName "dim") $ txt "Waiting..."
        else vBox [renderRow r | r <- rows]
  where
    cols = 10
    reqs = toList (tsRecentRequests state)
    padded = replicate (cols * 8 - length reqs) Nothing ++ map Just reqs
    rows = chunksOf cols padded
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    renderRow bs = hBox (map renderBlock bs)
    renderBlock Nothing = withAttr (attrName "dim") $ txt "·"
    renderBlock (Just True) = withAttr (attrName "ok") $ txt "■"
    renderBlock (Just False) = withAttr (attrName "err") $ txt "■"

errorsSection :: TUIState -> Widget Name
errorsSection state =
  padLeftRight 1 $
    vBox
      [ hBox
          [ withAttr (attrName "ok") $ txt $ "✓ " <> T.pack (show (tsSuccessCount state)) <> " ok"
          , txt "   "
          , withAttr (attrName "err") $ txt $ "✗ " <> T.pack (show (tsErrorCount state)) <> " errors"
          , if errorCount > 0 && total > 0
              then withAttr (attrName errPctAttr) $ txt $ "  (" <> T.pack (show pct) <> "%)"
              else emptyWidget
          ]
      , if Seq.null (tsRecentErrors state)
          then emptyWidget
          else vBox $ map renderErr $ foldr (:) [] $ Seq.take 3 (tsRecentErrors state)
      ]
  where
    errorCount = tsErrorCount state
    total = errorCount + tsSuccessCount state
    pct = if total > 0 then (errorCount * 100) `div` total else 0 :: Int
    errPctAttr
      | total > 0 && errorCount * 100 > total = "err"
      | total > 0 && errorCount * 1000 > total = "warn"
      | otherwise = "dim"
    renderErr (_, msg) = withAttr (attrName "err") $ txt $ "  " <> msg

footerSection :: Widget Name
footerSection = withAttr (attrName "dim") $ hCenter $ txt "q  quit    ctrl+c  quit"
