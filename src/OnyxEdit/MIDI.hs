module OnyxEdit.MIDI
( loadMIDI
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (stripPrefix)

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

import Control.Monad
import Data.Maybe

import OnyxEdit.Types
import OnyxEdit.Program

import Control.Monad.Trans.State
import Data.Traversable (traverse)

data RB3Drums
  = GemKick
  | GemRed
  | GemYellow
  | GemBlue
  | GemGreen
  | Toms YBG Bool
  | Disco Bool
  | OpenHihat Bool
  deriving (Eq, Ord, Show, Read)

midiToRB3Drums :: E.T -> Maybe RB3Drums
midiToRB3Drums evt = case evt of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p _))) -> case V.fromPitch p of
    25  -> Just $ OpenHihat True
    96  -> Just GemKick
    97  -> Just GemRed
    98  -> Just GemYellow
    99  -> Just GemBlue
    100 -> Just GemGreen
    110 -> Just $ Toms Yellow True
    111 -> Just $ Toms Blue   True
    112 -> Just $ Toms Green  True
    _   -> Nothing
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _))) -> case V.fromPitch p of
    25  -> Just $ OpenHihat   False
    110 -> Just $ Toms Yellow False
    111 -> Just $ Toms Blue   False
    112 -> Just $ Toms Green  False
    _   -> Nothing
  E.MetaEvent (M.TextEvent str) -> case stripPrefix "[mix 3 drums" str of
    Nothing   -> Nothing
    Just str' -> Just $ Disco $ drop 1 str' == "d]"
  _ -> Nothing

data RB3DrumState = DS
  { tomYellow :: Bool
  , tomBlue   :: Bool
  , tomGreen  :: Bool
  , discobeat :: Bool
  , openHihat :: Bool
  } deriving (Eq, Ord, Show, Read)

fromRB3Drums :: Map.Map a [RB3Drums] -> Map.Map a (Set.Set DrumEvent)
fromRB3Drums mp = evalState (traverse go mp) initialState where
  initialState = DS False False False False False
  go :: [RB3Drums] -> State RB3DrumState (Set.Set DrumEvent)
  go evts = do
    forM_ evts $ \evt -> case evt of
      Toms Yellow b -> modify $ \s -> s { tomYellow = b }
      Toms Blue   b -> modify $ \s -> s { tomBlue   = b }
      Toms Green  b -> modify $ \s -> s { tomGreen  = b }
      Disco       b -> modify $ \s -> s { discobeat = b }
      OpenHihat   b -> modify $ \s -> s { openHihat = b }
      _             -> return ()
    ds <- get
    let -- gems
        hasKick   = elem GemKick           evts
        hasRed    = elem GemRed            evts
        hasYellow = elem GemYellow         evts
        hasBlue   = elem GemBlue           evts
        hasGreen  = elem GemGreen          evts
        hasClose  = elem (OpenHihat False) evts
        -- notes
        kick = do
          guard hasKick
          Just $ Kick Normal
        snare = do
          guard $ hasRed || (hasYellow && discobeat ds)
          Just $ Snare Normal
        yellowHH = do
          guard $ or
            [ and [hasYellow, not $ discobeat ds, not $ tomYellow ds]
            , and [hasRed, discobeat ds]
            ]
          Just $ (if openHihat ds then HihatO else HihatC) Yellow
        yellowTom = do
          guard $ and [hasYellow, not $ discobeat ds, tomYellow ds]
          Just $ if hasRed
            then SnareFlam
            else Tom Yellow Normal
        blueGem = do
          guard hasBlue
          Just $ if tomBlue ds
            then Tom Blue Normal
            else Ride Blue
        greenGem = do
          guard hasGreen
          Just $ if tomGreen ds
            then Tom Green Normal
            else Crash Green
        closeHH = do
          guard $ and [hasClose, isNothing yellowHH]
          Just HihatF
        allGems = catMaybes
          [ kick, snare, yellowHH, yellowTom
          , blueGem, greenGem, closeHH
          ]
    return $ Set.fromList allGems

trackToMap :: F.Tempo -> RTB.T F.ElapsedTime a -> Map.Map Beats [a]
trackToMap res = let res' = fromIntegral res
  in Map.mapKeysMonotonic (\tks -> fromIntegral tks / res')
  . Map.fromDistinctAscList . ATB.toPairList . RTB.toAbsoluteEventList 0
  . RTB.collectCoincident

trackName :: Map.Map Beats [E.T] -> Maybe String
trackName trk = Map.lookup 0 trk >>= listToMaybe . mapMaybe isName where
  isName :: E.T -> Maybe String
  isName (E.MetaEvent (M.TrackName str)) = Just str
  isName _                               = Nothing

midiDrums :: Map.Map Beats [E.T] -> Prog ()
midiDrums = loadDrums . fromRB3Drums . fmap (mapMaybe midiToRB3Drums)

midiTempos :: Map.Map Beats [E.T] -> Prog ()
midiTempos trk = let
  getTempo e = case e of
    -- (1000000 microsec/sec) / (x microsec/beat) = (1000000 / x) (beat/sec)
    E.MetaEvent (M.SetTempo mspb) -> Just $ 1000000 / realToFrac mspb
    _                             -> Nothing
  in loadTempos $ Map.mapMaybe (listToMaybe . mapMaybe getTempo) trk

midiTimeSigs :: Map.Map Beats [E.T] -> Prog ()
midiTimeSigs trk = let
  getTimeSig e = case e of
    E.MetaEvent (M.TimeSig n d _ _) -> Just (n, (2 ^^ (-d)) * 4)
    _                               -> Nothing
  in loadTimeSigs $ Map.mapMaybe (listToMaybe . mapMaybe getTimeSig) trk

loadMIDI :: F.T -> Prog ()
loadMIDI f = case F.explicitNoteOff f of
  F.Cons F.Parallel (F.Ticks res) trks -> let
    beatTrks = map (trackToMap res) trks
    trkNames = map trackName beatTrks
    drumTrk  = lookup (Just "PART DRUMS") $ zip trkNames beatTrks
    firstTrk = listToMaybe beatTrks
    lastPos  = maximum $ 0 : map trkLast beatTrks
    trkLast trk = case Map.maxViewWithKey trk of
      Just ((k, _), _) -> k
      Nothing          -> 0
    in do
      clearAll
      end <- positionBoth $ Beats $ lastPos + 4
      setEnd end
      maybe (return ()) midiTempos firstTrk
      maybe (return ()) midiTimeSigs firstTrk
      maybe (return ()) midiDrums drumTrk
      setPosition $ Both 0 0
  _ -> error "loadMIDI: Not a parallel ticks-based MIDI file"
