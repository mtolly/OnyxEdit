{-# LANGUAGE TupleSections #-}
module OnyxEdit.Program where

import Graphics.UI.SDL hiding (flip)

import Sound.OpenAL hiding (get)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Arrow

import OnyxEdit.Types

type Prog = StateT Program IO

data Program = Program
  { vSurfaces   :: Surfaces
  , vSources    :: Sources
  , vTracks     :: Tracks
  , vResolution :: Int -- ^ Zoom level, in pixels (width) per second of time
  , vPlaySpeed  :: Rational -- ^ 1 = normal speed
  , vDivision   :: Beats -- ^ Sub-beat lines appear at this interval
  , vMetronome  :: Bool
  } deriving (Eq, Ord, Show)

data Tracks = Tracks
  { vPosition :: Position
  , vEnd      :: Position
  , vTempos   :: Map.Map Position BPS
  , vDrums    :: Map.Map Position (Set.Set Note)
  , vTimeSigs :: Map.Map Position (Int, Beats)
  , vLines    :: Map.Map Position Line
  } deriving (Eq, Ord, Show, Read)

fromSeconds :: Seconds -> Prog Position
fromSeconds secs = do
  tmps <- gets $ vTempos . vTracks
  return $ Both secs $ secondsToBeats tmps secs

fromBeats :: Beats -> Prog Position
fromBeats bts = do
  tmps <- gets $ vTempos . vTracks
  return $ Both (beatsToSeconds tmps bts) bts

positionBoth :: Position -> Prog Position
positionBoth b@(Both _ _) = return b
positionBoth (Seconds  s) = fromSeconds s
positionBoth (Beats    b) = fromBeats b

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

allSources :: Prog [Source]
allSources = do
  (dl, dr) <- gets $ vDrumAudio . vSources
  (sl, sr) <- gets $ vSongAudio . vSources
  return [dl, dr, sl, sr]

emptyTracks :: Tracks
emptyTracks = Tracks
  { vTempos   = Map.singleton (Both 0 0) 2
  , vDrums    = Map.empty
  , vTimeSigs = Map.singleton (Both 0 0) (4, 1)
  , vLines    = Map.empty
  , vPosition = Both 0 0
  , vEnd      = Both 0 0
  }

clearAll :: Prog ()
clearAll = modify $ \prog -> prog { vTracks = emptyTracks }

loadDrums :: Map.Map Beats (Set.Set Note) -> Prog ()
loadDrums drms = do
  tmps <- gets $ vTempos . vTracks
  let drms' = positionTrack tmps drms
  modify $ \prog -> prog { vTracks = (vTracks prog) { vDrums = drms' } }

loadTempos :: Map.Map Beats BPS -> Prog ()
loadTempos tmps = let
  tmps' = positionTempos tmps
  toBoth pos = let bts = toBeats pos in Both (beatsToSeconds tmps' bts) bts
  in modify $ \prog -> let
    trks = vTracks prog
    in prog
      { vTracks = trks
        { vPosition = toBoth $ vPosition trks
        , vEnd      = toBoth $ vEnd trks
        , vTempos   = tmps'
        , vDrums    = Map.mapKeysMonotonic toBoth $ vDrums trks
        , vTimeSigs = Map.mapKeysMonotonic toBoth $ vTimeSigs trks
        , vLines    = Map.mapKeysMonotonic toBoth $ vLines trks
        }
      }

loadTimeSigs :: Map.Map Beats (Int, Beats) -> Prog ()
loadTimeSigs sigs = do
  tmps <- gets $ vTempos . vTracks
  let sigs' = positionTrack tmps sigs
  modify $ \prog -> prog { vTracks = (vTracks prog) { vTimeSigs = sigs' } }
  makeLines

makeLines' :: Beats -> [(Beats, (Int, Beats))] -> Beats -> [(Beats, Line)]
makeLines' dvn sigs end = case sigs of
  [] -> []
  (bts, sig@(i, b)) : sigs' -> if bts >= end
    then []
    else let
      bts' = bts + fromIntegral i * b
      measure = Map.toAscList $ makeMeasure dvn bts sig
      in measure ++ case sigs' of
        (btsNext, _) : _ | bts' >= btsNext -> makeLines' dvn sigs' end
        _ -> makeLines' dvn ((bts', sig) : sigs') end

makeLines :: Prog ()
makeLines = do
  sigs <- fmap Map.toAscList $ gets $ vTimeSigs . vTracks
  dvn <- gets vDivision
  end <- gets $ vEnd . vTracks
  let btLns = makeLines' dvn (map (first toBeats) sigs) (toBeats end)
  posLns <- forM btLns $ runKleisli $ first $ Kleisli $ positionBoth . Beats
  modify $ \prog ->
    prog { vTracks = (vTracks prog) { vLines = Map.fromList posLns } }

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

setSpeed :: Rational -> Prog ()
setSpeed spd = do
  modify $ \prog -> prog { vPlaySpeed = spd }
  srcs <- allSources
  liftIO $ forM_ srcs $ \src -> pitch src $= realToFrac spd

setPosition :: Position -> Prog ()
setPosition pos = do
  strt <- gets $ vAudioStart . vSources
  let pos' = strt + realToFrac (toSeconds pos)
  modify $ \prog -> prog { vTracks = (vTracks prog) { vPosition = pos } }
  srcs <- allSources
  liftIO $ forM_ srcs $ \src -> secOffset src $= pos'

setEnd :: Position -> Prog ()
setEnd end = do
  pos <- gets $ vPosition . vTracks
  modify $ \prog -> prog
    { vTracks = (vTracks prog)
      { vPosition = min pos end
      , vEnd      = end
      }
    }

setResolution :: Int -> Prog ()
setResolution res = modify $ \prog -> prog { vResolution = res }

modifyResolution :: (Int -> Int) -> Prog ()
modifyResolution f = gets vResolution >>= setResolution . f

modifySpeed :: (Rational -> Rational) -> Prog ()
modifySpeed f = gets vPlaySpeed >>= setSpeed . f
