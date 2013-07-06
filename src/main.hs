{-# LANGUAGE TupleSections #-}
module Main (main) where

import Graphics.UI.SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

import Sound.ALUT

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Ratio

import qualified Sound.MIDI.File as File
import qualified Data.Rhythm.MIDI as MIDI
import qualified Data.Rhythm.Status as Status
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

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

-- | A position expressed in either real time or musical time.
data Position
  = Both Seconds Beats
  | Seconds Seconds
  | Beats Beats
  deriving (Show, Read)

-- | Comparing two positions will either compare their Seconds values, or their
-- Beats values, depending on which is present. Comparing a Seconds to a Beats
-- will raise an error.
instance Ord Position where
  compare (Both  s _) (Both  s' _) = compare s s' -- arbitrary
  compare (Both  s _) (Seconds s') = compare s s'
  compare (Both  _ b) (Beats   b') = compare b b'
  compare (Seconds s) (Both  s' _) = compare s s'
  compare (Seconds s) (Seconds s') = compare s s'
  compare (Seconds _) (Beats    _) = error "compare: can't compare Seconds and Beats"
  compare (Beats   b) (Both  _ b') = compare b b'
  compare (Beats   _) (Seconds  _) = error "compare: can't compare Seconds and Beats"
  compare (Beats   b) (Beats   b') = compare b b'

instance Eq Position where
  x == y = compare x y == EQ

toSeconds :: Position -> Seconds
toSeconds (Both s _) = s
toSeconds (Seconds s) = s
toSeconds (Beats _) = error "toSeconds: got Beats value"

toBeats :: Position -> Seconds
toBeats (Both _ b) = b
toBeats (Beats b) = b
toBeats (Seconds _) = error "toBeats: got Seconds value"

positionTempos :: Map.Map Beats BPS -> Map.Map Position BPS
positionTempos = Map.fromDistinctAscList . f 0 0 2 . Map.toAscList where
  f :: Beats -> Seconds -> BPS -> [(Beats, BPS)] -> [(Position, BPS)]
  f bts secs bps xs = case xs of
    [] -> []
    (bts', bps') : xs' -> let
      secs' = secs + (bts' - bts) / bps
      in (Both secs' bts', bts') : f bts' secs' bps' xs'

positionTrack :: Map.Map Position BPS -> Map.Map Beats a -> Map.Map Position a
positionTrack tmps = Map.mapKeysMonotonic $
  \bts -> Both (beatsToSeconds' tmps bts) bts

secondsToBeats' :: Map.Map Position BPS -> Seconds -> Beats
secondsToBeats' tmps secs = case Map.lookupLE (Seconds secs) tmps of
  Nothing -> error "secondsToBeats: missing tempo"
  Just (Both secs' bts, bps) -> bts + (secs - secs') * bps
  Just _ -> error "secondsToBeats: invalidly stored tempo"

secondsToBeats :: Seconds -> Prog Beats
secondsToBeats secs = do
  tmps <- gets $ vTempos . vTracks
  return $ secondsToBeats' tmps secs

beatsToSeconds' :: Map.Map Position BPS -> Beats -> Seconds
beatsToSeconds' tmps bts = case Map.lookupLE (Beats bts) tmps of
  Nothing -> error "beatsToSeconds: missing tempo"
  Just (Both secs bts', bps) -> secs + (bts - bts') / bps
  Just _ -> error "beatsToSeconds: invalidly stored tempo"

beatsToSeconds :: Beats -> Prog Seconds
beatsToSeconds bts = do
  tmps <- gets $ vTempos . vTracks
  return $ beatsToSeconds' tmps bts

data Tracks = Tracks
  { vTempos   :: Map.Map Position BPS
  , vDrums    :: Map.Map Position (Set.Set Note)
  , vMeasures :: Map.Map Position (Int, Beats)
  , vLines    :: Map.Map Position Line
  } deriving (Eq, Ord, Show, Read)

data Program = Program
  { vSurfaces   :: Surfaces
  , vSources    :: Sources
  , vTracks     :: Tracks
  , vPosition   :: Position
  , vResolution :: Int -- ^ Zoom level, in pixels (width) per second of time
  , vPlaying    :: Bool -- ^ Is audio currently playing?
  , vPlaySpeed  :: Rational
  , vDivision   :: Beats -- ^ The beat fraction that creates sub-beat lines.
  , vMetronome  :: Bool
  } deriving (Eq, Ord, Show)

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
makeLines = return ()
{-
makeLines = do
  msrs <- fmap Map.toAscList $ gets vMeasures
  dvn <- gets vDivision
  let btLns = concatMap (Map.toAscList . uncurry (makeMeasure dvn)) msrs
  secLns <- mapM (runKleisli $ first $ Kleisli beatsToSeconds) btLns
  modify $ \prog -> prog { vLines = Map.fromDistinctAscList secLns }
-}

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
  lns <- gets $ vLines . vTracks
  (dl, _) <- gets $ vDrumAudio . vSources
  t <- liftIO $ Sound.ALUT.get $ secOffset dl
  a <- gets $ vAudioStart . vSources
  let pos' = realToFrac $ t - a
  bts <- secondsToBeats pos' 
  modify $ \prog -> prog { vPosition = Both pos' bts }
  met <- gets vMetronome
  when (pos' > toSeconds pos && met) $ case Map.splitLookup pos lns of
    (_, eq, gt) -> case Map.splitLookup (Seconds pos') gt of
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
  return $ 150 + floor ((pos - toSeconds now) * fromIntegral pps)

drawLine :: Position -> Line -> Prog ()
drawLine pos l = void $ do
  x <- timeToX $ toSeconds pos
  scrn <- gets $ vScreen . vSurfaces
  surf <- gets $ vBeatLines . vSurfaces
  let clip = Just $ case l of
        Measure -> Rect 0 0 30 125
        Beat    -> Rect 30 0 30 125
        SubBeat -> Rect 60 0 30 125
      drawAt = Just $ Rect (x - 15) 100 0 0
  liftIO $ blitSurface surf clip scrn drawAt

drawNote :: Position -> Note -> Prog ()
drawNote pos note = void $ do
  surf <- gets $ vNoteSheet . vSurfaces
  scrn <- gets $ vScreen . vSurfaces
  x <- timeToX $ toSeconds pos
  let (clipX, clipY) = noteSprite note
      clip = Just $ Rect clipX clipY 30 125
      drawAt = Just $ Rect (x - 15) 100 0 0
  liftIO $ blitSurface surf clip scrn drawAt

-- TODO: Limit drawing to notes that are actually visible
drawNotes :: Prog ()
drawNotes = do
  notes <- gets $ vDrums . vTracks
  now <- gets vPosition
  case Map.splitLookup now notes of
    (lt, eq, gt) -> do
      drawLess lt
      maybe (return ()) (mapM_ (drawNote now) . Set.toList) eq
      drawMore gt
  where
    drawLess mp = forM_ (Map.toList mp) $ \(pos, nts) ->
      forM_ (Set.toList nts) $ drawNote pos
    drawMore = drawLess

kitchen :: Map.Map Beats (Set.Set Note)
kitchen = Map.fromList
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

kitchenMeasures :: Map.Map Beats (Int, Beats)
kitchenMeasures = Map.fromList $ map (, (3, 1)) [0, 3, 6, 9, 12]

drawBG :: Prog ()
drawBG = void $ do
  scrn <- gets $ vScreen . vSurfaces
  bg   <- gets $ vBackground . vSurfaces
  liftIO $ apply 0 0 bg scrn

drawLines :: Prog ()
drawLines = gets (vLines . vTracks) >>= mapM_ (uncurry drawLine) . Map.toList

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
  gets (vScreen . vSurfaces) >>= liftIO . SDL.flip

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
      tmps = Map.singleton (Both 0 0) 2
      tracks = Tracks
        { vTempos = tmps
        , vDrums = positionTrack tmps kitchen
        , vMeasures = positionTrack tmps kitchenMeasures
        , vLines = Map.empty -- generated with makeLines
        }
      prog = Program
        { vSurfaces = surfaces
        , vSources = sources
        , vTracks = tracks
        , vPosition = Both 0 0
        , vResolution = 200
        , vPlaying = False
        , vPlaySpeed = 1
        , vDivision = 1/4
        , vMetronome = False
        }
  evalStateT (setPosition (Both 0 0) >> makeLines >> draw >> inputLoop) prog

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

setPosition :: Position -> Prog ()
setPosition pos = do
  strt <- gets $ vAudioStart . vSources
  let pos' = strt + realToFrac (toSeconds pos)
  pauseAndEdit $ \srcs -> do
    modify $ \prog -> prog { vPosition = pos }
    liftIO $ forM_ srcs $ \src ->
      secOffset src $= pos'

setSeconds :: Seconds -> Prog ()
setSeconds secs = do
  bts <- secondsToBeats secs
  setPosition $ Both secs bts

setBeats :: Seconds -> Prog ()
setBeats bts = do
  secs <- beatsToSeconds bts
  setPosition $ Both secs bts

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
      lns <- gets $ vLines . vTracks
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
      lns <- gets $ vLines . vTracks
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
    KeyDown (Keysym SDLK_z _ _) -> setPosition (Both 0 0)
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
toggleDrum _ = return ()
{-
toggleDrum n = do
  now <- gets vPosition
  modify $ \prog -> prog { vDrumChart = Map.alter f now $ vDrumChart prog }
  where f Nothing = Just $ Set.singleton n
        f (Just notes) = if Set.member n notes
          then if Set.size notes == 1
            then Nothing
            else Just $ Set.delete n notes
          else Just $ Set.insert n notes
-}

{-
loadMIDI :: File.T -> Prog ()
loadMIDI f = case MIDI.readFile f of
  Nothing -> error "Invalid MIDI file"
  Just fTicks -> let
    fBeats = MIDI.fromTickFile fTicks
    rtbTempos = Status.toRTB' $ MIDI.tempoTrack fBeats
    mapTempos = fmap realToFrac $ Map.mapKeysMonotonic realToFrac $
      Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 rtbTempos
    in undefined
-}
