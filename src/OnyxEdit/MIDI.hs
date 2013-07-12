module OnyxEdit.MIDI (loadMIDI) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe

import OnyxEdit.Types
import OnyxEdit.Program

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
midiDrums trk = let
  pitchToNote (c, p, v) = case V.fromPitch p of
    35 -> Just $ Kick hit -- acoustic bass drum
    36 -> Just $ Kick hit -- bass drum 1
    38 -> Just $ Snare hit -- acoustic snare
    40 -> Just SnareFlam -- electric snare
    41 -> Just $ Tom (fromMaybe Green ybg) hit
    42 -> Just $ HihatC $ fromMaybe Yellow ybg
    43 -> Just $ Tom (fromMaybe Green ybg) hit
    44 -> Just HihatF
    45 -> Just $ Tom (fromMaybe Blue ybg) hit
    46 -> Just $ HihatO $ fromMaybe Yellow ybg
    47 -> Just $ Tom (fromMaybe Blue ybg) hit
    48 -> Just $ Tom (fromMaybe Yellow ybg) hit
    49 -> Just $ Crash $ fromMaybe Green ybg -- crash 1
    50 -> Just $ Tom (fromMaybe Yellow ybg) hit
    51 -> Just $ Ride $ fromMaybe Blue ybg -- ride 1
    52 -> Just $ Crash $ fromMaybe Yellow ybg -- china
    53 -> Just $ Ride $ fromMaybe Blue ybg -- ride bell
    55 -> Just $ Crash $ fromMaybe Yellow ybg -- splash
    57 -> Just $ Crash $ fromMaybe Blue ybg -- crash 2
    59 -> Just $ Ride $ fromMaybe Green ybg -- ride 2
    _ -> Nothing
    where ybg = case C.fromChannel c of
            1 -> Just Yellow
            2 -> Just Blue
            3 -> Just Green
            _ -> Nothing
          hit = if V.fromVelocity v >= 64 then Normal else Ghost
  getNoteOn e = case e of
    E.MIDIEvent (C.Cons c (C.Voice (V.NoteOn p v)))
      | V.fromVelocity v /= 0 -> Just (c, p, v)
    _                         -> Nothing
  listToSet es = let
    st = Set.fromList $ mapMaybe (getNoteOn >=> pitchToNote) es
    in guard (not $ Set.null st) >> Just st
  in loadDrums $ Map.mapMaybe listToSet trk

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
loadMIDI f = case f of
  F.Cons F.Parallel (F.Ticks res) trks -> let
    beatTrks = map (trackToMap res) trks
    trkNames = map trackName beatTrks
    drumTrk  = lookup (Just "onyx_drums") $ zip trkNames beatTrks
    firstTrk = listToMaybe beatTrks
    lastPos  = maximum $ 0 : map trkLast beatTrks
    trkLast trk = case Map.maxViewWithKey trk of
      Just ((k, _), _) -> k
      Nothing          -> 0
    in do
      clearAll
      end <- positionBoth $ Beats $ lastPos + 4
      modify $ \prog -> prog { vEnd = end }
      maybe (return ()) midiTempos firstTrk
      maybe (return ()) midiTimeSigs firstTrk
      maybe (return ()) midiDrums drumTrk
      setPosition $ Both 0 0
  _ -> error "loadMIDI: Not a parallel ticks-based MIDI file"
