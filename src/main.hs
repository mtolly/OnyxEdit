module Main where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Sound.ALUT

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (void, forM_, zipWithM_, when, unless)
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Paths_rhythmal

data Note
  = Kick
  | Snare
  | HihatF -- ^ Foot
  | HihatC -- ^ Closed
  | HihatO -- ^ Open
  | RideB -- ^ Blue
  | RideG -- ^ Green
  | CrashY -- ^ Yellow
  | CrashB -- ^ Blue
  | CrashG -- ^ Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

noteSprite :: Note -> (Int, Int)
noteSprite n = case n of
  Kick -> (0, 0)
  HihatF -> (120, 60)
  Snare -> (0, 30)
  HihatC -> (60, 60)
  HihatO -> (90, 60)
  RideB -> (60, 90)
  RideG -> (60, 120)
  CrashY -> (30, 60)
  CrashB -> (30, 90)
  CrashG -> (30, 120)

type Seconds = Rational
type Beats   = Rational
type BPS     = Rational

data Program = Program
  { vSurfaces   :: Surfaces
  , vSources    :: Sources
  , vDrumChart  :: Map.Map Seconds (Set.Set Note)
  , vPosition   :: Seconds
  , vResolution :: Int
  -- ^ Zoom level, in pixels (width) per second of time
  , vPlaying    :: Bool
  -- ^ Is audio currently playing?
  , vPlaySpeed  :: Rational
  , vTempos     :: Map.Map Seconds (Beats, BPS)
  , vTemposRev  :: Map.Map Beats (Seconds, BPS)
  , vMeasures   :: Map.Map Beats (Int, Beats)
  , vLines      :: Map.Map Seconds Line
  } deriving (Eq, Ord, Show)

secondsToBeats :: Seconds -> Prog Beats
secondsToBeats secs = do
  tmps <- gets vTempos
  return $ case Map.splitLookup secs tmps of
    (_, Just (bts, _), _) -> bts
    (lt, Nothing, _) -> case Map.maxViewWithKey lt of
      Nothing -> error $
        "secondsToBeats: no tempo event before " ++ show secs ++ " seconds"
      Just ((secs', (bts, bps)), _) ->
        bts + (secs - secs') * bps

beatsToSeconds :: Beats -> Prog Seconds
beatsToSeconds bts = do
  tmps <- gets vTemposRev
  return $ case Map.splitLookup bts tmps of
    (_, Just (secs, _), _) -> secs
    (lt, Nothing, _) -> case Map.maxViewWithKey lt of
      Nothing -> error $
        "beatsToSeconds: no tempo event before " ++ show bts ++ " beats"
      Just ((bts', (secs, bps)), _) ->
        secs + (bts - bts') / bps

-- TODO: add Beat and SubBeat lines
makeLines :: Prog ()
makeLines = do
  bts <- fmap Map.keys $ gets vMeasures
  secs <- mapM beatsToSeconds bts
  modify $ \prog ->
    prog { vLines = Map.fromDistinctAscList $ zip secs $ repeat Measure }

data Surfaces = Surfaces
  { vScreen      :: Surface
  , vNoteSheet   :: Surface
  , vBackground  :: Surface
  , vStaff       :: Surface
  , vMeasureLine :: Surface
  , vBeatLine    :: Surface
  , vSubBeatLine :: Surface
  } deriving (Eq, Ord, Show)

data Sources = Sources
  { vAudioStart :: Float
  , vDrumAudio  :: (Source, Source)
  , vSongAudio  :: (Source, Source)
  } deriving (Eq, Ord, Show)

data Line = Measure | Beat | SubBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Prog = StateT Program IO

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormatAlpha

apply :: Int -> Int -> Surface -> Surface -> IO Bool
apply x y src dst = blitSurface src Nothing dst (Just offset)
  where offset = Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applyCenter :: Int -> Int -> Surface -> Surface -> IO Bool
applyCenter x y src dst = do
  Rect _ _ w h <- getClipRect src
  apply (x - div w 2) (y - div h 2) src dst

playUpdate :: Prog Bool
playUpdate = gets vPlaying >>= \b -> if b
  then do
    (dl, _) <- gets $ vDrumAudio . vSources
    t <- liftIO $ Sound.ALUT.get $ secOffset dl
    a <- gets $ vAudioStart . vSources
    modify $ \prog -> prog { vPosition = realToFrac $ t - a }
    return True
  else return False

timeToX :: Seconds -> Prog Int
timeToX pos = do
  now <- gets vPosition
  pps <- gets vResolution
  return $ 150 + floor ((pos - now) * fromIntegral pps)

noteToY :: Note -> Int
noteToY n = 290 - 25 * case n of
  Kick -> 0
  HihatF -> 0
  Snare -> 1
  HihatC -> 2
  HihatO -> 2
  RideB -> 3
  RideG -> 4
  CrashY -> 2
  CrashB -> 3
  CrashG -> 4

drawLine :: Seconds -> Line -> Prog ()
drawLine pos l = void $ do
  x <- fmap (subtract 1) $ timeToX pos
  scrn <- gets $ vScreen . vSurfaces
  case l of
    Measure -> do
      s <- gets $ vMeasureLine . vSurfaces
      liftIO $ apply x 189 s scrn
    Beat -> do
      s <- gets $ vBeatLine . vSurfaces
      liftIO $ apply x 189 s scrn
    SubBeat -> do
      s <- gets $ vSubBeatLine . vSurfaces
      liftIO $ apply x 214 s scrn

drawNote :: Seconds -> Note -> Prog ()
drawNote pos note = void $ do
  surf <- gets $ vNoteSheet . vSurfaces
  scrn <- gets $ vScreen . vSurfaces
  x <- timeToX pos
  let (clipX, clipY) = noteSprite note
      clip = Just $ Rect clipX clipY 30 30
      drawAt = Just $ Rect (x - 15) (noteToY note - 15) 0 0
  liftIO $ blitSurface surf clip scrn drawAt

drawNotes :: Prog ()
drawNotes = do
  notes <- gets vDrumChart
  now <- gets vPosition
  case Map.splitLookup now notes of
    (lt, eq, gt) -> do
      drawLess lt
      maybe (return ()) (mapM_ (drawNote now) . Set.toList) eq
      drawMore gt
  where
    drawLess mp = mapM_ (\(pos, nts) -> mapM_ (drawNote pos) $ Set.toList nts) $ Map.toList mp
    drawMore = drawLess

kitchen :: Map.Map Seconds (Set.Set Note)
kitchen = Map.fromList $ map (\(sec, st) -> (sec / 2, st))
  [ (0  , Set.fromList [Kick, CrashG])
  , (0.5, Set.fromList [HihatF])
  , (1  , Set.fromList [RideB])
  , (1.5, Set.fromList [Snare])
  , (2  , Set.fromList [RideB])
  , (2.5, Set.fromList [HihatO])
  , (3  , Set.fromList [Kick, HihatF, RideB])
  , (3.5, Set.fromList [HihatC])
  , (4  , Set.fromList [RideB])
  , (4.5, Set.fromList [Snare])
  , (5  , Set.fromList [RideB])
  , (5.5, Set.fromList [HihatO])
  ]

drawBG :: Prog ()
drawBG = void $ do
  scrn <- gets $ vScreen . vSurfaces
  bg   <- gets $ vBackground . vSurfaces
  liftIO $ apply 0 0 bg scrn

drawLines :: Prog ()
drawLines = gets vLines >>= mapM_ (uncurry drawLine) . Map.toList

drawStaff :: Prog ()
drawStaff = void $ do
  scrn <- gets $ vScreen . vSurfaces
  stf  <- gets $ vStaff . vSurfaces
  liftIO $ apply 0 0 stf scrn

draw :: Prog ()
draw = do
  drawBG >> drawLines >> drawStaff >> drawNotes
  gets (vScreen . vSurfaces) >>= liftIO . Graphics.UI.SDL.flip

loadSource :: FilePath -> Source -> IO ()
loadSource f src = createBuffer (File f) >>= \buf -> buffer src $= Just buf

main :: IO ()
main = withInit [InitTimer, InitVideo] $ withProgNameAndArgs runALUT $ \_ args -> do

  -- Get screen, load sprites
  scrn       <- setVideoMode 1000 480 32 [SWSurface]
  gemSheet   <- getDataFileName "gems.png" >>= loadImage
  bgImage    <- getDataFileName "bg.png" >>= loadImage
  staffImage <- getDataFileName "staff.png" >>= loadImage
  measure    <- getDataFileName "measure.png" >>= loadImage
  beat       <- getDataFileName "beat.png" >>= loadImage
  subbeat    <- getDataFileName "subbeat.png" >>= loadImage

  -- Load audio
  srcs@[srcDrumL, srcDrumR, srcSongL, srcSongR] <- genObjectNames 4
  zipWithM_ loadSource args srcs
  forM_ [srcDrumL, srcSongL] $ \src ->
    liftIO $ sourcePosition src $= Vertex3 (-1) 0 0
  forM_ [srcDrumR, srcSongR] $ \src ->
    liftIO $ sourcePosition src $= Vertex3 1 0 0

  let surfaces = Surfaces
        { vScreen = scrn
        , vNoteSheet = gemSheet
        , vBackground = bgImage
        , vStaff = staffImage
        , vMeasureLine = measure
        , vBeatLine = beat
        , vSubBeatLine = subbeat
        }
      sources = Sources
        { vAudioStart = 39.726
        , vDrumAudio = (srcDrumL, srcDrumR)
        , vSongAudio = (srcSongL, srcSongR)
        }
      prog = Program
        { vSurfaces = surfaces
        , vSources = sources
        , vDrumChart = kitchen
        , vPosition = 0
        , vResolution = 200
        , vPlaying = False
        , vPlaySpeed = 1
        , vTempos = Map.fromList [(0, (0, 2))]
        , vTemposRev = Map.fromList [(0, (0, 2))]
        , vMeasures = Map.fromList [(0, (3, 1)), (3, (3, 1))]
        , vLines = undefined
        }
  evalStateT (makeLines >> draw >> inputLoop) prog

inputLoop :: Prog ()
inputLoop = do
  liftIO $ delay 5
  playUpdate >>= \b -> when b draw >> inputLoop' b

updateSpeed :: Prog ()
updateSpeed = do
  spd <- gets vPlaySpeed
  (dl, dr) <- gets $ vDrumAudio . vSources
  (sl, sr) <- gets $ vSongAudio . vSources
  liftIO $ forM_ [dl, dr, sl, sr] $ \src ->
    pitch src $= realToFrac spd

inputLoop' :: Bool -> Prog ()
inputLoop' b = liftIO pollEvent >>= \evt -> case evt of
  Quit -> return ()
  KeyDown (Keysym SDLK_UP _ _) -> do
    modify $ \prog -> prog { vResolution = vResolution prog + 20 }
    draw
    inputLoop
  KeyDown (Keysym SDLK_DOWN _ _) -> do
    modify $ \prog -> prog { vResolution = max 20 $ vResolution prog - 20 }
    draw
    inputLoop
  KeyDown (Keysym SDLK_LEFT _ _) -> do
    modify $ \prog -> prog { vPlaySpeed = max 0.1 $ vPlaySpeed prog - 0.1 }
    updateSpeed
    inputLoop
  KeyDown (Keysym SDLK_RIGHT _ _) -> do
    modify $ \prog -> prog { vPlaySpeed = min 2 $ vPlaySpeed prog + 0.1 }
    updateSpeed
    inputLoop
  MouseButtonDown _ _ ButtonWheelDown -> if b then inputLoop else do
    modify $ \prog -> prog { vPosition = fromInteger (floor ((vPosition prog * 4) + 1)) / 4 }
    draw
    inputLoop
  MouseButtonDown _ _ ButtonWheelUp -> if b then inputLoop else do
    modify $ \prog -> prog { vPosition = max 0 $ fromInteger (ceiling ((vPosition prog * 4) - 1)) / 4 }
    draw
    inputLoop
  KeyDown (Keysym SDLK_1 _ _) -> unless b (toggleDrum Kick >> draw) >> inputLoop
  KeyDown (Keysym SDLK_2 _ _) -> unless b (toggleDrum Snare >> draw) >> inputLoop
  KeyDown (Keysym SDLK_SPACE _ _) -> if b
    then do
      (srcDrumL, srcDrumR) <- gets $ vDrumAudio . vSources
      (srcSongL, srcSongR) <- gets $ vSongAudio . vSources
      modify $ \prog -> prog { vPlaying = False }
      liftIO $ pause [srcDrumL, srcDrumR, srcSongL, srcSongR]
      inputLoop
    else do
      now <- gets vPosition
      (srcDrumL, srcDrumR) <- gets $ vDrumAudio . vSources
      (srcSongL, srcSongR) <- gets $ vSongAudio . vSources
      strt <- gets $ vAudioStart . vSources
      modify $ \prog -> prog { vPlaying = True }
      forM_ [srcDrumL, srcDrumR, srcSongL, srcSongR] $ \src ->
        liftIO $ secOffset src $= strt + realToFrac now
      liftIO $ play [srcDrumL, srcDrumR, srcSongL, srcSongR]
      inputLoop
  KeyDown (Keysym SDLK_d _ _) -> do
    (srcDrumL, srcDrumR) <- gets $ vDrumAudio . vSources
    g <- liftIO $ Sound.ALUT.get $ sourceGain srcDrumL
    forM_ [srcDrumL, srcDrumR] $ \src ->
      liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    inputLoop
  KeyDown (Keysym SDLK_s _ _) -> do
    (srcSongL, srcSongR) <- gets $ vSongAudio . vSources
    g <- liftIO $ Sound.ALUT.get $ sourceGain srcSongL
    forM_ [srcSongL, srcSongR] $ \src ->
      liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    inputLoop
  KeyDown (Keysym SDLK_z _ _) -> if b then inputLoop else do
    modify $ \prog -> prog { vPosition = 0 }
    draw
    inputLoop
  _    -> inputLoop

toggleDrum :: Note -> Prog ()
toggleDrum n = do
  now <- gets vPosition
  modify $ \prog -> prog { vDrumChart = Map.alter f now $ vDrumChart prog }
  where f Nothing = Just $ Set.singleton n
        f (Just notes) = case (Set.lookupIndex n notes, Set.size notes) of
          (Just 0, 0) -> Nothing
          (Just i, _) -> Just $ Set.deleteAt i notes
          (Nothing, _) -> Just $ Set.insert n notes
