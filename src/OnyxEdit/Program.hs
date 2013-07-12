{-# LANGUAGE TupleSections #-}
module OnyxEdit.Program where

import Graphics.UI.SDL hiding (flip)

import Sound.ALUT hiding (get)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Trans.State

import OnyxEdit.Types

type Prog = StateT Program IO

data Program = Program
  { vSurfaces   :: Surfaces
  , vSources    :: Sources
  , vTracks     :: Tracks
  , vPosition   :: Position
  , vEnd        :: Position
  , vResolution :: Int -- ^ Zoom level, in pixels (width) per second of time
  , vPlaying    :: Bool -- ^ Is audio currently playing?
  , vPlaySpeed  :: Rational
  , vDivision   :: Beats -- ^ The beat fraction that creates sub-beat lines.
  , vMetronome  :: Bool
  } deriving (Eq, Ord, Show)

data Tracks = Tracks
  { vTempos   :: Map.Map Position BPS
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
  }

clearAll :: Prog ()
clearAll = modify $ \prog -> prog
  { vTracks   = emptyTracks
  , vPosition = Both 0 0
  , vEnd      = Both 0 0
  }
