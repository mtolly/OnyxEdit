{-# LANGUAGE TupleSections #-}
module Main (main) where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Sound.ALUT

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Ratio

import Control.Monad (void, forM_, zipWithM_, when, unless)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Arrow

import System.Exit

import Paths_OnyxEdit

data Note
  = Kick
  | Snare
  | SnareFlam -- ^ Red/yellow, a double hit on snare
  | TomY -- ^ Yellow
  | TomB -- ^ Blue
  | TomG -- ^ Green
  | HighFlamY -- ^ Yellow/blue, a double hit on high tom
  | HighFlamB -- ^ Yellow/blue, a double hit on mid tom
  | LowFlamB  -- ^ Blue/green, a double hit on mid tom
  | LowFlamG  -- ^ Blue/green, a double hit on low tom
  | HihatF -- ^ Foot
  | HihatC -- ^ Closed
  | HihatO -- ^ Open
  | RideB  -- ^ Blue
  | RideG  -- ^ Green
  | CrashY -- ^ Yellow
  | CrashB -- ^ Blue
  | CrashG -- ^ Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

noteSprite :: Note -> (Int, Int)
noteSprite n = (30 * x, 0) where
  x = case n of
    Kick -> 0
    Snare -> 1
    SnareFlam -> 20
    TomY -> 2
    TomB -> 3 
    TomG -> 4
    HighFlamY -> 21
    HighFlamB -> 22
    LowFlamB -> 23
    LowFlamG -> 24
    HihatF -> 19
    HihatC -> 8
    HihatO -> 11
    RideB -> 9
    RideG -> 10
    CrashY -> 5
    CrashB -> 6
    CrashG -> 7

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
  -- ^ Tempo events to convert from seconds to beats.
  , vTemposRev  :: Map.Map Beats (Seconds, BPS)
  -- ^ Tempo events to convert from beats to seconds.
  , vMeasures   :: Map.Map Beats (Int, Beats)
  , vDivision   :: Beats
  -- ^ The fraction of a beat that creates sub-beat lines.
  , vLines      :: Map.Map Seconds Line
  , vMetronome  :: Bool
  } deriving (Eq, Ord, Show)

secondsToBeats :: Seconds -> Prog Beats
secondsToBeats secs = do
  tmps <- gets vTempos
  return $ case Map.splitLookup secs tmps of
    (_, Just (bts, _), _) -> bts
    (lt, Nothing, _) -> case Map.maxViewWithKey lt of
      Nothing -> error $
        "secondsToBeats: no tempo event before " ++ show secs ++ " seconds"
      Just ((secs', (bts, bps)), _) -> bts + (secs - secs') * bps

beatsToSeconds :: Beats -> Prog Seconds
beatsToSeconds bts = do
  tmps <- gets vTemposRev
  return $ case Map.splitLookup bts tmps of
    (_, Just (secs, _), _) -> secs
    (lt, Nothing, _) -> case Map.maxViewWithKey lt of
      Nothing -> error $
        "beatsToSeconds: no tempo event before " ++ show bts ++ " beats"
      Just ((bts', (secs, bps)), _) -> secs + (bts - bts') / bps

makeMeasure :: Beats -> Beats -> (Int, Beats) -> Map.Map Beats Line
makeMeasure dvn start (mult, unit) = let
  len = fromIntegral mult * unit
  end = start + len
  subbeats = Map.fromDistinctAscList $ map (, SubBeat) $
    takeWhile (< end) [start, start + dvn ..]
  beats    = Map.fromDistinctAscList $ map (, Beat) $
    takeWhile (< end) [start, start + unit ..]
  measure = Map.singleton start Measure
  in measure `Map.union` beats `Map.union` subbeats

makeLines :: Prog ()
makeLines = do
  msrs <- fmap Map.toAscList $ gets vMeasures
  dvn <- gets vDivision
  let btLns = concatMap (Map.toAscList . uncurry (makeMeasure dvn)) msrs
  secLns <- mapM (runKleisli $ first $ Kleisli beatsToSeconds) btLns
  modify $ \prog -> prog { vLines = Map.fromDistinctAscList secLns }

data Surfaces = Surfaces
  { vScreen     :: Surface
  , vNoteSheet  :: Surface
  , vBackground :: Surface
  , vStaff      :: Surface
  , vBeatLines  :: Surface
  , vNowLine    :: Surface
  } deriving (Eq, Ord, Show)

data Sources = Sources
  { vAudioStart :: Float
  , vDrumAudio  :: (Source, Source)
  , vSongAudio  :: (Source, Source)
  , vClick      :: Source
  } deriving (Eq, Ord, Show)

data Line = Measure | Beat | SubBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Prog = StateT Program IO

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormatAlpha

apply :: Int -> Int -> Surface -> Surface -> IO Bool
apply x y src dst = blitSurface src Nothing dst (Just offset)
  where offset = Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

playUpdate :: Prog ()
playUpdate = gets vPlaying >>= \b -> when b $ do
  pos <- gets vPosition
  lns <- gets vLines
  (dl, _) <- gets $ vDrumAudio . vSources
  t <- liftIO $ Sound.ALUT.get $ secOffset dl
  a <- gets $ vAudioStart . vSources
  let pos' = realToFrac $ t - a
  modify $ \prog -> prog { vPosition = pos' }
  met <- gets vMetronome
  when (pos' > pos && met) $ case Map.splitLookup pos lns of
    (_, eq, gt) -> case Map.splitLookup pos' gt of
      (lt, _, _) -> let
        startBeat = elem eq [Just Measure, Just Beat]
        passedBeat = any (/= SubBeat) $ Map.elems lt
        in when (startBeat || passedBeat) $ do
          clk <- gets $ vClick . vSources
          liftIO $ stop [clk]
          liftIO $ secOffset clk $= 0
          liftIO $ play [clk]

timeToX :: Seconds -> Prog Int
timeToX pos = do
  now <- gets vPosition
  pps <- gets vResolution
  return $ 150 + floor ((pos - now) * fromIntegral pps)

drawLine :: Seconds -> Line -> Prog ()
drawLine pos l = void $ do
  x <- timeToX pos
  scrn <- gets $ vScreen . vSurfaces
  surf <- gets $ vBeatLines . vSurfaces
  let clip = Just $ case l of
        Measure -> Rect 0 0 30 125
        Beat    -> Rect 30 0 30 125
        SubBeat -> Rect 60 0 30 125
      drawAt = Just $ Rect (x - 15) 100 0 0
  liftIO $ blitSurface surf clip scrn drawAt

drawNote :: Seconds -> Note -> Prog ()
drawNote pos note = void $ do
  surf <- gets $ vNoteSheet . vSurfaces
  scrn <- gets $ vScreen . vSurfaces
  x <- timeToX pos
  let (clipX, clipY) = noteSprite note
      clip = Just $ Rect clipX clipY 30 125
      drawAt = Just $ Rect (x - 15) 100 0 0
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
    drawLess mp = mapM_ (\(pos, nts) -> mapM_ (drawNote pos) $ Set.toList nts) $
      Map.toList mp
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
  , (6  , Set.fromList [Kick, HihatF, RideB])
  , (6.5, Set.fromList [HihatC])
  , (7  , Set.fromList [RideB])
  , (7.5, Set.fromList [Snare])
  , (8  , Set.fromList [RideB])
  , (8.5, Set.fromList [HihatO])
  , (9  , Set.fromList [Kick, HihatF, RideB])
  , (9.5, Set.fromList [HihatC])
  , (10  , Set.fromList [RideB])
  , (10.5, Set.fromList [Kick])
  , (11  , Set.fromList [Snare])
  , (11.5, Set.fromList [Snare])
  , (12  , Set.fromList [Kick, CrashG])
  ]

drawBG :: Prog ()
drawBG = void $ do
  scrn <- gets $ vScreen . vSurfaces
  bg   <- gets $ vBackground . vSurfaces
  liftIO $ apply 0 0 bg scrn

drawLines :: Prog ()
drawLines = gets vLines >>= mapM_ (uncurry drawLine) . Map.toList

drawStaff :: Prog ()
drawStaff = do
  scrn <- gets $ vScreen . vSurfaces
  now <- gets $ vNowLine . vSurfaces
  stf  <- gets $ vStaff . vSurfaces
  void $ liftIO $ apply 0 100 stf scrn
  void $ liftIO $ apply (150 - 15) 0 now scrn

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
  gemSheet   <- getDataFileName "gems.png"  >>= loadImage
  bgImage    <- getDataFileName "bg.png"    >>= loadImage
  staffImage <- getDataFileName "staff.png" >>= loadImage
  beat       <- getDataFileName "beat.png"  >>= loadImage
  now        <- getDataFileName "now.png"   >>= loadImage

  -- Load audio
  srcs@[srcDrumL, srcDrumR, srcSongL, srcSongR] <- genObjectNames 4
  zipWithM_ loadSource args srcs
  forM_ [srcDrumL, srcSongL] $ \src ->
    liftIO $ sourcePosition src $= Vertex3 (-1) 0 0
  forM_ [srcDrumR, srcSongR] $ \src ->
    liftIO $ sourcePosition src $= Vertex3 1 0 0
  [srcClick] <- genObjectNames 1
  clk <- getDataFileName "click.wav"
  loadSource clk srcClick

  let surfaces = Surfaces
        { vScreen = scrn
        , vNoteSheet = gemSheet
        , vBackground = bgImage
        , vStaff = staffImage
        , vBeatLines = beat
        , vNowLine = now
        }
      sources = Sources
        { vAudioStart = 39.726
        , vDrumAudio = (srcDrumL, srcDrumR)
        , vSongAudio = (srcSongL, srcSongR)
        , vClick = srcClick
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
        , vMeasures = Map.fromList
          [(0, (3, 1)), (3, (3, 1)), (6, (3, 1)), (9, (3, 1)), (12, (3, 1))]
        , vDivision = 1/4
        , vLines = undefined -- generated with makeLines
        , vMetronome = False
        }
  evalStateT (setPosition 0 >> makeLines >> draw >> inputLoop) prog

pauseAndEdit :: ([Source] -> Prog ()) -> Prog ()
pauseAndEdit f = do
  (dl, dr) <- gets $ vDrumAudio . vSources
  (sl, sr) <- gets $ vSongAudio . vSources
  let srcs = [dl, dr, sl, sr]
  b <- gets vPlaying
  when b $ liftIO $ pause srcs
  f [dl, dr, sl, sr]
  when b $ liftIO $ play srcs

setSpeed :: Rational -> Prog ()
setSpeed spd = pauseAndEdit $ \srcs -> do
  modify $ \prog -> prog { vPlaySpeed = spd }
  liftIO $ forM_ srcs $ \src ->
    pitch src $= realToFrac spd

setPosition :: Seconds -> Prog ()
setPosition pos = do
  strt <- gets $ vAudioStart . vSources
  let pos' = strt + realToFrac pos
  pauseAndEdit $ \srcs -> do
    modify $ \prog -> prog { vPosition = pos }
    liftIO $ forM_ srcs $ \src ->
      secOffset src $= pos'

inputLoop :: Prog ()
inputLoop = do
  liftIO $ delay 5
  playUpdate
  draw
  b <- gets vPlaying
  evt <- liftIO pollEvent
  case evt of
    Quit -> liftIO exitSuccess
    KeyDown (Keysym SDLK_UP _ _) ->
      modify $ \prog -> prog { vResolution = vResolution prog + 20 }
    KeyDown (Keysym SDLK_DOWN _ _) ->
      modify $ \prog -> prog { vResolution = max 20 $ vResolution prog - 20 }
    KeyDown (Keysym SDLK_LEFT _ _) -> do
      spd <- gets vPlaySpeed
      setSpeed $ max 0.1 $ spd - 0.1
    KeyDown (Keysym SDLK_RIGHT _ _) -> do
      spd <- gets vPlaySpeed
      setSpeed $ min 2 $ spd + 0.1
    MouseButtonDown _ _ ButtonWheelDown -> do
      pos <- gets vPosition
      lns <- gets vLines
      case Map.splitLookup pos lns of
        (_, _, gt) -> if b
          then case reverse $ take 2 $ Map.toAscList gt of
            (k, _) : _ -> setPosition k
            []         -> return ()
          else case Map.minViewWithKey gt of
            Just ((k, _), _) -> setPosition k
            Nothing          -> return ()
    MouseButtonDown _ _ ButtonWheelUp -> do
      pos <- gets vPosition
      lns <- gets vLines
      case Map.splitLookup pos lns of
        (lt, _, _) -> if b
          then case reverse $ take 3 $ Map.toDescList lt of
            (k, _) : _ -> setPosition k
            []         -> return ()
          else case Map.maxViewWithKey lt of
            Just ((k, _), _) -> setPosition k
            Nothing          -> return ()
    KeyDown (Keysym SDLK_1 _ _) -> unless b $ toggleDrum Kick
    KeyDown (Keysym SDLK_2 _ _) -> unless b $ toggleDrum Snare
    KeyDown (Keysym SDLK_SPACE _ _) -> do
      (srcDrumL, srcDrumR) <- gets $ vDrumAudio . vSources
      (srcSongL, srcSongR) <- gets $ vSongAudio . vSources
      modify $ \prog -> prog { vPlaying = not b }
      liftIO $ (if b then pause else play)
        [srcDrumL, srcDrumR, srcSongL, srcSongR]
    KeyDown (Keysym SDLK_d _ _) -> do
      (srcDrumL, srcDrumR) <- gets $ vDrumAudio . vSources
      g <- liftIO $ Sound.ALUT.get $ sourceGain srcDrumL
      forM_ [srcDrumL, srcDrumR] $ \src ->
        liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    KeyDown (Keysym SDLK_s _ _) -> do
      (srcSongL, srcSongR) <- gets $ vSongAudio . vSources
      g <- liftIO $ Sound.ALUT.get $ sourceGain srcSongL
      forM_ [srcSongL, srcSongR] $ \src ->
        liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    KeyDown (Keysym SDLK_z _ _) -> setPosition 0
    KeyDown (Keysym SDLK_m _ _) ->
      modify $ \prog -> prog { vMetronome = not $ vMetronome prog }
    KeyDown (Keysym SDLK_q _ _) -> do
      dvn <- gets vDivision
      case (numerator dvn, denominator dvn) of
        (1, d) -> do
          modify $ \prog -> prog { vDivision = 1 % (d + 1) }
          makeLines
        _      -> return ()
    KeyDown (Keysym SDLK_a _ _) -> do
      dvn <- gets vDivision
      case (numerator dvn, denominator dvn) of
        (1, d) | d >= 2 -> do
          modify $ \prog -> prog { vDivision = 1 % (d - 1) }
          makeLines
        _               -> return ()
    _    -> return ()
  inputLoop

toggleDrum :: Note -> Prog ()
toggleDrum n = do
  now <- gets vPosition
  modify $ \prog -> prog { vDrumChart = Map.alter f now $ vDrumChart prog }
  where f Nothing = Just $ Set.singleton n
        f (Just notes) = case (Set.lookupIndex n notes, Set.size notes) of
          (Just 0, 0) -> Nothing
          (Just i, _) -> Just $ Set.deleteAt i notes
          (Nothing, _) -> Just $ Set.insert n notes
