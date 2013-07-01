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

data Program = Program
  { vSurfaces :: Surfaces
  , vDrumChart :: Map.Map Seconds (Set.Set Note)
  , vPosition :: Seconds
  , vResolution :: Int
  , vPlaying :: Bool
  , vDrumAudio :: (Source, Source)
  , vSongAudio :: (Source, Source)
  , vAudioStart :: Float
  , vPlaySpeed :: Rational
  , vLines :: Map.Map Seconds Line
  } deriving (Eq, Ord, Show)

data Surfaces = Surfaces
  { vScreen :: Surface
  , vNoteSheet :: Surface
  , vBackground :: Surface
  , vStaff :: Surface
  , vMeasureLine :: Surface
  , vBeatLine :: Surface
  , vSubBeatLine :: Surface
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
    (dl, _) <- gets vDrumAudio
    t <- liftIO $ Sound.ALUT.get $ secOffset dl
    a <- gets vAudioStart
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

kitchenLines :: Map.Map Seconds Line
kitchenLines = Map.fromList $ map (\(sec, st) -> (sec / 2, st))
  [ (0  , Measure)
  , (0.5, SubBeat)
  , (1  , Beat)
  , (1.5, SubBeat)
  , (2  , Beat)
  , (2.5, SubBeat)
  , (3  , Measure)
  , (3.5, SubBeat)
  , (4  , Beat)
  , (4.5, SubBeat)
  , (5  , Beat)
  , (5.5, SubBeat)
  , (6  , Measure)
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
      prog = Program
        { vSurfaces = surfaces
        , vDrumChart = kitchen
        , vPosition = 0
        , vResolution = 200
        , vPlaying = False
        , vDrumAudio = (srcDrumL, srcDrumR)
        , vSongAudio = (srcSongL, srcSongR)
        , vAudioStart = 39.726
        , vPlaySpeed = 1
        , vLines = kitchenLines
        }
  evalStateT (draw >> inputLoop) prog

inputLoop :: Prog ()
inputLoop = do
  liftIO $ delay 5
  playUpdate >>= \b -> when b draw >> inputLoop' b

updateSpeed :: Prog ()
updateSpeed = do
  spd <- gets vPlaySpeed
  (dl, dr) <- gets vDrumAudio
  (sl, sr) <- gets vSongAudio
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
      (srcDrumL, srcDrumR) <- gets vDrumAudio
      (srcSongL, srcSongR) <- gets vSongAudio
      modify $ \prog -> prog { vPlaying = False }
      liftIO $ pause [srcDrumL, srcDrumR, srcSongL, srcSongR]
      inputLoop
    else do
      now <- gets vPosition
      (srcDrumL, srcDrumR) <- gets vDrumAudio
      (srcSongL, srcSongR) <- gets vSongAudio
      strt <- gets vAudioStart
      modify $ \prog -> prog { vPlaying = True }
      forM_ [srcDrumL, srcDrumR, srcSongL, srcSongR] $ \src ->
        liftIO $ secOffset src $= strt + realToFrac now
      liftIO $ play [srcDrumL, srcDrumR, srcSongL, srcSongR]
      inputLoop
  KeyDown (Keysym SDLK_d _ _) -> do
    (srcDrumL, srcDrumR) <- gets vDrumAudio
    g <- liftIO $ Sound.ALUT.get $ sourceGain srcDrumL
    forM_ [srcDrumL, srcDrumR] $ \src ->
      liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    inputLoop
  KeyDown (Keysym SDLK_s _ _) -> do
    (srcSongL, srcSongR) <- gets vSongAudio
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
