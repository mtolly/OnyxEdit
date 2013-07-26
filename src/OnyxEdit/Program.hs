{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module OnyxEdit.Program where

import Prelude hiding ((.), id)
import Control.Category

import Graphics.UI.SDL hiding (flip)

import Sound.OpenAL hiding (get)
import qualified Sound.OpenAL as OpenAL

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Arrow

import Data.Accessor
import qualified Data.Accessor.Monad.Trans.State as A

import Data.Word

import OnyxEdit.Types

type Prog = StateT Program IO

data Program = Program
  { vSurfaces_   :: Surfaces
  , vSources_    :: Sources
  , vTracks_     :: Tracks
  , vResolution_ :: Int -- ^ Zoom level, in pixels (width) per second of time
  , vPlaySpeed_  :: Rational -- ^ 1 = normal speed
  , vDivision_   :: Beats -- ^ Sub-beat lines appear at this interval
  , vMetronome_  :: Bool
  , vReference_  :: (Word32, Seconds)
  } deriving (Eq, Ord, Show)

vSurfaces   = accessor vSurfaces_   $ \x s -> s { vSurfaces_   = x }
vSources    = accessor vSources_    $ \x s -> s { vSources_    = x }
vTracks     = accessor vTracks_     $ \x s -> s { vTracks_     = x }
vResolution = accessor vResolution_ $ \x s -> s { vResolution_ = x }
vPlaySpeed  = accessor vPlaySpeed_  $ \x s -> s { vPlaySpeed_  = x }
vDivision   = accessor vDivision_   $ \x s -> s { vDivision_   = x }
vMetronome  = accessor vMetronome_  $ \x s -> s { vMetronome_  = x }
vReference  = accessor vReference_  $ \x s -> s { vReference_  = x }

data Tracks = Tracks
  { vPosition_ :: Position
  , vEnd_      :: Position
  , vTempos_   :: Map.Map Position BPS
  , vDrums_    :: Map.Map Position (Set.Set DrumEvent)
  , vTimeSigs_ :: Map.Map Position (Int, Beats)
  , vLines_    :: Map.Map Position Line
  } deriving (Eq, Ord, Show, Read)

vPosition = accessor vPosition_ $ \x s -> s { vPosition_ = x }
vEnd      = accessor vEnd_      $ \x s -> s { vEnd_      = x }
vTempos   = accessor vTempos_   $ \x s -> s { vTempos_   = x }
vDrums    = accessor vDrums_    $ \x s -> s { vDrums_    = x }
vTimeSigs = accessor vTimeSigs_ $ \x s -> s { vTimeSigs_ = x }
vLines    = accessor vLines_    $ \x s -> s { vLines_    = x }

fromSeconds :: Seconds -> Prog Position
fromSeconds secs = do
  tmps <- A.get $ vTempos . vTracks
  return $ Both secs $ secondsToBeats tmps secs

fromBeats :: Beats -> Prog Position
fromBeats bts = do
  tmps <- A.get $ vTempos . vTracks
  return $ Both (beatsToSeconds tmps bts) bts

positionBoth :: Position -> Prog Position
positionBoth b@(Both _ _) = return b
positionBoth (Seconds  s) = fromSeconds s
positionBoth (Beats    b) = fromBeats b

data Surfaces = Surfaces
  { vScreen_     :: Surface
  , vNoteSheet_  :: Surface
  , vBackground_ :: Surface
  , vStaff_      :: Surface
  , vNowLine_    :: Surface
  } deriving (Eq, Ord, Show)

vScreen     = accessor vScreen_     $ \x s -> s { vScreen_     = x }
vNoteSheet  = accessor vNoteSheet_  $ \x s -> s { vNoteSheet_  = x }
vBackground = accessor vBackground_ $ \x s -> s { vBackground_ = x }
vStaff      = accessor vStaff_      $ \x s -> s { vStaff_      = x }
vNowLine    = accessor vNowLine_    $ \x s -> s { vNowLine_    = x }

data Sources = Sources
  { vAudioStart_ :: Float
  , vDrumAudio_  :: (Source, Source)
  , vSongAudio_  :: (Source, Source)
  , vClick_      :: Source
  } deriving (Eq, Ord, Show)

vAudioStart = accessor vAudioStart_ $ \x s -> s { vAudioStart_ = x }
vDrumAudio  = accessor vDrumAudio_  $ \x s -> s { vDrumAudio_  = x }
vSongAudio  = accessor vSongAudio_  $ \x s -> s { vSongAudio_  = x }
vClick      = accessor vClick_      $ \x s -> s { vClick_      = x }

-- | Gets all the music audio sources referenced by the program state.
-- This does not include sound effects such as the metronome.
allSources :: Prog [Source]
allSources = do
  (dl, dr) <- A.get $ vDrumAudio . vSources
  (sl, sr) <- A.get $ vSongAudio . vSources
  return [dl, dr, sl, sr]

-- | A collection of empty tracks. The only events are a 120 BPM tempo and a
-- 4/4 time signature at position zero.
emptyTracks :: Tracks
emptyTracks = Tracks
  { vTempos_   = Map.singleton (Both 0 0) 2
  , vDrums_    = Map.empty
  , vTimeSigs_ = Map.singleton (Both 0 0) (4, 1)
  , vLines_    = Map.empty
  , vPosition_ = Both 0 0
  , vEnd_      = Both 0 0
  }

clearAll :: Prog ()
clearAll = A.set vTracks emptyTracks

-- | Loads a drum note track. The time of all positions is calculated from the
-- current tempo map.
loadDrums :: Map.Map Beats (Set.Set DrumEvent) -> Prog ()
loadDrums drms = do
  tmps <- A.get $ vTempos . vTracks
  let drms' = positionTrack tmps drms
  A.set (vDrums . vTracks) drms'

-- | Loads a new tempo map. The time of any existing positions is recalculated.
loadTempos :: Map.Map Beats BPS -> Prog ()
loadTempos tmps = let
  tmps' = positionTempos tmps
  toBoth pos = let bts = toBeats pos in Both (beatsToSeconds tmps' bts) bts
  in A.modify vTracks $ foldr (.) id
    [ vPosition ^: toBoth
    , vEnd      ^: toBoth
    , vTempos   ^= tmps'
    , vDrums    ^: Map.mapKeysMonotonic toBoth
    , vTimeSigs ^: Map.mapKeysMonotonic toBoth
    , vLines    ^: Map.mapKeysMonotonic toBoth
    ]

-- | Loads a new set of time signatures, and recalculates measure/beat lines.
loadTimeSigs :: Map.Map Beats (Int, Beats) -> Prog ()
loadTimeSigs sigs = do
  tmps <- A.get $ vTempos . vTracks
  let sigs' = positionTrack tmps sigs
  A.set (vTimeSigs . vTracks) sigs'
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

-- | Uses the current time signature map and division setting to calculate where
-- measure, beat, and sub-beat lines should go.
makeLines :: Prog ()
makeLines = do
  sigs <- fmap Map.toAscList $ A.get $ vTimeSigs . vTracks
  dvn <- A.get vDivision
  end <- A.get $ vEnd . vTracks
  let btLns = makeLines' dvn (map (first toBeats) sigs) (toBeats end)
  posLns <- forM btLns $ runKleisli $ first $ Kleisli $ positionBoth . Beats
  A.set (vLines . vTracks) $ Map.fromList posLns

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

-- | Sets the play speed, including the speed of all audio sources.
-- To avoid audio sources becoming desynchronized, this should be done paused.
setSpeed :: Rational -> Prog ()
setSpeed spd = do
  A.set vPlaySpeed spd
  srcs <- allSources
  liftIO $ forM_ srcs $ \src -> pitch src $= realToFrac spd

-- | Sets the time offset, including the offset of all audio sources.
-- To avoid audio sources becoming desynchronized, this should be done paused.
setPosition :: Position -> Prog ()
setPosition pos = do
  strt <- A.get $ vAudioStart . vSources
  both <- positionBoth pos
  let pos' = strt + realToFrac (toSeconds both)
  A.set (vPosition . vTracks) both
  srcs <- allSources
  liftIO $ forM_ srcs $ \src -> secOffset src $= pos'

-- | Sets the end of the track. Also sets the current position if it is beyond
-- the new end point.
setEnd :: Position -> Prog ()
setEnd end = do
  A.set    (vEnd      . vTracks) end
  A.modify (vPosition . vTracks) $ min end

-- | Changes the play speed, including the speed of all audio sources.
-- To avoid audio sources becoming desynchronized, this should be done paused.
modifySpeed :: (Rational -> Rational) -> Prog ()
modifySpeed f = A.get vPlaySpeed >>= setSpeed . f

whilePaused :: Prog () -> Prog ()
whilePaused act = do
  srcs <- allSources >>= filterM isPlaying
  liftIO $ pause srcs
  act
  liftIO $ play srcs
  where isPlaying src = do
          st <- liftIO $ OpenAL.get $ sourceState src
          return $ st == Playing
