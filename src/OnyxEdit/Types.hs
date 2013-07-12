module OnyxEdit.Types where

import qualified Data.Map as Map

data Note
  = Kick Hit
  | Snare Hit
  | SnareFlam -- ^ Red/yellow, a double hit on snare
  | Tom YBG Hit
  | HihatF     -- ^ Foot
  | HihatC YBG -- ^ Closed
  | HihatO YBG -- ^ Open
  | Ride   YBG
  | Crash  YBG
  deriving (Eq, Ord, Show, Read)

data Hit = Normal | Ghost
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data YBG = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  compare (Seconds _) (Beats    _) =
    error "compare: can't compare Seconds and Beats"
  compare (Beats   b) (Both  _ b') = compare b b'
  compare (Beats   _) (Seconds  _) =
    error "compare: can't compare Seconds and Beats"
  compare (Beats   b) (Beats   b') = compare b b'

instance Eq Position where
  x == y = compare x y == EQ

toSeconds :: Position -> Seconds
toSeconds (Both s _) = s
toSeconds (Seconds s) = s
toSeconds (Beats _) = error "toSeconds: got Beats value"

toBeats :: Position -> Beats
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
      in (Both secs' bts', bps') : f bts' secs' bps' xs'

positionTrack :: Map.Map Position BPS -> Map.Map Beats a -> Map.Map Position a
positionTrack tmps = Map.mapKeysMonotonic $
  \bts -> Both (beatsToSeconds tmps bts) bts

secondsToBeats :: Map.Map Position BPS -> Seconds -> Beats
secondsToBeats tmps secs = case Map.lookupLE (Seconds secs) tmps of
  Nothing -> error "secondsToBeats: missing tempo"
  Just (Both secs' bts, bps) -> bts + (secs - secs') * bps
  Just _ -> error "secondsToBeats: invalidly stored tempo"

beatsToSeconds :: Map.Map Position BPS -> Beats -> Seconds
beatsToSeconds tmps bts = case Map.lookupLE (Beats bts) tmps of
  Nothing -> error "beatsToSeconds: missing tempo"
  Just (Both secs bts', bps) -> secs + (bts - bts') / bps
  Just _ -> error "beatsToSeconds: invalidly stored tempo"

data Line = Measure | Beat | SubBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
