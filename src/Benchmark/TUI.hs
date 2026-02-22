module Benchmark.TUI (
    runTUI,
    TUIState,
    BenchmarkEvent (..),
    initialState,
) where

import Benchmark.TUI.State (RollingStats (..))
import Benchmark.TUI.State hiding (RollingStats (..))
import Benchmark.TUI.Widgets
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel, hBorder)
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
import Lens.Micro ((^.))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, hPutStr, openTempFile, stderr)

data CustomEvent
    = Tick
    | BenchEvent BenchmarkEvent

type Name = ()

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
        , hBox [histogramSection state, latencyChartSection state]
        , hBorder
        , errorsSection state
        , hBorder
        , footerSection
        ]

headerSection :: TUIState -> Widget Name
headerSection state =
    padLeftRight 1 $
        vBox
            [ row "Target  " $ withAttr (attrName "hi") $ txt (state ^. tsTarget)
            , row "Phase   " $
                hBox
                    [ txt $ state ^. tsCurrentEndpoint
                    , withAttr (attrName "dim") $
                        txt $
                            "  ["
                                <> T.pack (show (state ^. tsEndpointIndex))
                                <> "/"
                                <> T.pack (show (state ^. tsTotalEndpoints))
                                <> "]"
                    ]
            , if T.null (state ^. tsStatus)
                then emptyWidget
                else row "Status  " $ withAttr (attrName "dim") $ txt (state ^. tsStatus)
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
                ]
            ]
  where
    completed = state ^. tsCompleted
    total = state ^. tsIsTotal
    pct = if total > 0 then fromIntegral completed / fromIntegral total else 0 :: Float
    elapsed = state ^. tsElapsedSecs

statsSection :: TUIState -> Widget Name
statsSection state =
    padLeftRight 1 $ case state ^. tsRollingStats of
        Nothing -> withAttr (attrName "dim") $ txt $ "Waiting for data...  (rolling last " <> T.pack (show rollingWindow) <> ")"
        Just stats ->
            vBox
                [ withAttr (attrName "dim") $ txt $ "Latency  (rolling last " <> T.pack (show rollingWindow) <> ")"
                , txt " "
                , hBox
                    [ statBox "Mean" (formatDuration $ stats ^. rsMeanMs)
                    , statBox "P50 " (formatDuration $ stats ^. rsP50Ms)
                    , statBox "P95 " (formatDuration $ stats ^. rsP95Ms)
                    , statBox "P99 " (formatDuration $ stats ^. rsP99Ms)
                    , statBox "Min " (formatDuration $ stats ^. rsMinMs)
                    , statBox "Max " (formatDuration $ stats ^. rsMaxMs)
                    ]
                ]
  where
    statBox label value =
        padRight (Pad 4) $
            vBox
                [ withAttr (attrName "label") $ txt label
                , withAttr (attrName "stat") $ txt value
                ]

histogramSection :: TUIState -> Widget Name
histogramSection state =
    borderWithLabel (withAttr (attrName "label") $ txt " Distribution ") $
        histogram (zip labels (state ^. tsBuckets))
  where
    labels = ["<5s", "<7.5s", "<10s", "<12.5s", "<15s", ">15s"]

latencyChartSection :: TUIState -> Widget Name
latencyChartSection state =
    borderWithLabel (withAttr (attrName "label") $ txt " Latency Trend ") $
        padLeftRight 1 $
            if Seq.null (state ^. tsLatencyHistory)
                then withAttr (attrName "dim") $ txt "Waiting for data..."
                else
                    let history = reverse $ toList (state ^. tsLatencyHistory)
                        p99s = map _rsP99Ms history
                        p50s = map _rsP50Ms history
                        maxVal = maximum (p99s ++ p50s)
                     in vBox
                            [ sparkRow "P99 " (attrName "err") maxVal p99s
                            , sparkRow "P50 " (attrName "hi") maxVal p50s
                            ]
  where
    sparkRow label attr maxVal values =
        hBox
            [ withAttr (attrName "label") $ txt (label <> " ")
            , withAttr attr $ txt (mkSparkline maxVal values)
            ]
    mkSparkline maxVal values =
        let blocks = "▁▂▃▄▅▆▇█"
            nLevels = 7
            toChar v
                | maxVal <= 0 = '▁'
                | otherwise = T.index blocks $ min nLevels $ round (v / maxVal * fromIntegral nLevels)
         in T.pack $ map toChar values

errorsSection :: TUIState -> Widget Name
errorsSection state =
    padLeftRight 1 $
        vBox
            [ hBox
                [ withAttr (attrName "ok") $ txt $ "✓ " <> T.pack (show (state ^. tsSuccessCount)) <> " ok"
                , txt "   "
                , withAttr (attrName "err") $ txt $ "✗ " <> T.pack (show (state ^. tsErrorCount)) <> " errors"
                , if errorCount > 0 && total > 0
                    then withAttr (attrName "dim") $ txt $ "  (" <> T.pack (show pct) <> "%)"
                    else emptyWidget
                ]
            , if Seq.null (state ^. tsRecentErrors)
                then emptyWidget
                else vBox $ map renderErr $ foldr (:) [] $ Seq.take 3 (state ^. tsRecentErrors)
            ]
  where
    errorCount = state ^. tsErrorCount
    total = errorCount + state ^. tsSuccessCount
    pct = if total > 0 then (errorCount * 100) `div` total else 0 :: Int
    renderErr (_, msg) = withAttr (attrName "err") $ txt $ "  " <> msg

footerSection :: Widget Name
footerSection = withAttr (attrName "dim") $ hCenter $ txt "q  quit    ctrl+c  quit"

-- ── Events ────────────────────────────────────────────────────────────────────

handleEvent :: BrickEvent Name CustomEvent -> EventM Name TUIState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
handleEvent (AppEvent Tick) = do
    now <- liftIO getCurrentTime
    modify $ \st -> case _tsStartTime st of
        Nothing -> st
        Just start -> st{_tsElapsedSecs = realToFrac (diffUTCTime now start)}
handleEvent (AppEvent (BenchEvent event)) = do
    now <- liftIO getCurrentTime
    modify (updateState now event)
    case event of
        BenchmarkFinished -> halt
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
        , (attrName "ok", fg V.green)
        , (attrName "err", fg V.red)
        , (attrName "dim", fg V.brightBlack)
        , (attrName "progress", fg V.blue)
        ]

tuiApp :: App TUIState CustomEvent Name
tuiApp =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theAttrMap
        }

-- ── Runner ────────────────────────────────────────────────────────────────────

{- | Redirect stderr to a temp file for the duration of 'action', then replay
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

runTUI :: TChan BenchmarkEvent -> TUIState -> IO TUIState
runTUI eventChan initialSt = do
    brickChan <- newBChan 1000

    tid1 <- forkIO $ forever $ do
        event <- atomically $ readTChan eventChan
        writeBChan brickChan (BenchEvent event)

    tid2 <- forkIO $ forever $ do
        threadDelay 100_000
        writeBChan brickChan Tick

    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    result <-
        withStderrBuffered $
            customMain initialVty buildVty (Just brickChan) tuiApp initialSt
    killThread tid1
    killThread tid2
    return result
