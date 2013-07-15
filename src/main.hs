module Main (main) where

import Prelude hiding ((.), id)
import Control.Category

import Graphics.UI.SDL hiding (flip)
import Graphics.UI.SDL.Image

import Sound.OpenAL hiding (get)
import qualified Sound.OpenAL as OpenAL

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intersect)

import Data.Ratio

import qualified Sound.MIDI.File.Load as Load

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import System.Exit
import System.Environment (getArgs)

--import Data.Accessor
import qualified Data.Accessor.Monad.Trans.State as A

import Paths_OnyxEdit
import OnyxEdit.Types
import OnyxEdit.Program
import OnyxEdit.MIDI
import OnyxEdit.Draw
import OnyxEdit.Audio

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormatAlpha

main :: IO ()
main = withInit [InitTimer, InitVideo] $ do

  -- Initialize OpenAL
  beginContext

  -- Get screen, load sprites
  scrn       <- setVideoMode 1000 480 32 [SWSurface]
  gemSheet   <- getDataFileName "gems.png"  >>= loadImage
  bgImage    <- getDataFileName "bg.png"    >>= loadImage
  staffImage <- getDataFileName "staff.png" >>= loadImage
  beat       <- getDataFileName "beat.png"  >>= loadImage
  now        <- getDataFileName "now.png"   >>= loadImage

  -- Load audio
  [midPath, drumPath, songPath] <- getArgs
  (srcDrumL, srcDrumR) <- loadStereo16WAV drumPath
  (srcSongL, srcSongR) <- loadStereo16WAV songPath
  srcClick <- getDataFileName "click.wav" >>= loadMono16WAV

  -- Load MIDI
  mid <- Load.fromFile midPath

  let surfaces = Surfaces
        { vScreen_     = scrn
        , vNoteSheet_  = gemSheet
        , vBackground_ = bgImage
        , vStaff_      = staffImage
        , vBeatLines_  = beat
        , vNowLine_    = now
        }
      sources = Sources
        { vAudioStart_ = 39.726
        , vDrumAudio_  = (srcDrumL, srcDrumR)
        , vSongAudio_  = (srcSongL, srcSongR)
        , vClick_      = srcClick
        }
      prog = Program
        { vSurfaces_   = surfaces
        , vSources_    = sources
        , vTracks_     = undefined
        , vResolution_ = 200
        , vPlaySpeed_  = 1
        , vDivision_   = 1/4
        , vMetronome_  = False
        }

  evalStateT (clearAll >> loadMIDI mid >> draw >> loopPaused) prog

toggleSource :: Source -> Prog ()
toggleSource src = liftIO $ do
  g <- OpenAL.get $ sourceGain src
  sourceGain src $= if g > 0.5 then 0 else 1

staffLines :: Note -> [Int]
staffLines n = case n of
  Kick Normal -> [0]
  Kick Ghost  -> [0, -1]
  Snare _     -> [1]
  SnareFlam   -> [1, 2]
  Tom  ybg _  -> [2 + fromEnum ybg]
  HihatF      -> [-1]
  HihatC ybg  -> [2 + fromEnum ybg]
  HihatO ybg  -> [2 + fromEnum ybg]
  Ride   ybg  -> [2 + fromEnum ybg]
  Crash  ybg  -> [2 + fromEnum ybg]

toggleNote :: Note -> Position -> Prog ()
toggleNote n pos = A.modify (vDrums . vTracks) $ Map.alter f pos where
  f Nothing = Just $ Set.singleton n
  f (Just notes) = if Set.member n notes
    then if Set.size notes == 1
      then Nothing
      else Just $ Set.delete n notes
    else let
      occupied = staffLines n
      in Just $ Set.insert n $
        Set.filter (null . intersect occupied . staffLines) notes

toggleNow :: Note -> Prog ()
toggleNow n = A.get (vPosition . vTracks) >>= toggleNote n

toggleNearest :: Note -> Prog ()
toggleNearest n = do
  pos <- A.get $ vPosition . vTracks
  lns <- A.get $ vLines    . vTracks
  case (Map.lookupLE pos lns, Map.lookupGT pos lns) of
    (Just (k1, _), Just (k2, _)) -> toggleNote n $ let
      [secNow, sec1, sec2] = map toSeconds [pos, k1, k2]
      secNow' = secNow - 0.05 -- approx. audio delay
      in if secNow' - sec1 > sec2 - secNow' then k2 else k1
    (Just (k, _), Nothing) -> toggleNote n k
    (Nothing, Just (k, _)) -> toggleNote n k
    (Nothing, Nothing) -> return ()

-- | The loop for a state that isn't in playing mode. We don't have to draw;
-- just handle the next event.
loopPaused :: Prog ()
loopPaused = do
  liftIO $ delay 1
  evt <- liftIO pollEvent
  case evt of
    Quit -> liftIO $ endContext >> exitSuccess
    KeyDown (Keysym k mods _) -> let
      hihat = if KeyModLeftShift `elem` mods then HihatC else HihatO
      hit   = if KeyModLeftCtrl  `elem` mods then Ghost  else Normal
      in case k of
      SDLK_UP -> A.modify vResolution (+ 20) >> draw >> loopPaused
      SDLK_DOWN -> A.modify vResolution (\r -> max 0 $ r - 20) >> draw >> loopPaused
      SDLK_LEFT -> modifySpeed (\spd -> max 0.1 $ spd - 0.1) >> loopPaused
      SDLK_RIGHT -> modifySpeed (\spd -> min 2 $ spd + 0.1) >> loopPaused
      SDLK_1 -> do
        (srcDrumL, srcDrumR) <- A.get $ vDrumAudio . vSources
        forM_ [srcDrumL, srcDrumR] toggleSource
        loopPaused
      SDLK_BACKQUOTE -> do
        (srcSongL, srcSongR) <- A.get $ vSongAudio . vSources
        forM_ [srcSongL, srcSongR] toggleSource
        loopPaused
      SDLK_BACKSPACE -> setPosition (Both 0 0) >> draw >> loopPaused
      SDLK_TAB -> do
        A.modify vMetronome not
        loopPaused
      SDLK_q -> do
        dvn <- A.get vDivision
        case (numerator dvn, denominator dvn) of
          (1, d) -> do
            A.set vDivision $ 1 % (d + 1)
            makeLines
          _      -> return ()
        draw
        loopPaused
      SDLK_a -> do
        dvn <- A.get vDivision
        case (numerator dvn, denominator dvn) of
          (1, d) | d >= 2 -> do
            A.set vDivision $ 1 % (d - 1)
            makeLines
          _               -> return ()
        draw
        loopPaused
      SDLK_z     -> toggleNow HihatF              >> draw >> loopPaused
      SDLK_SPACE -> toggleNow (Kick          hit) >> draw >> loopPaused
      SDLK_v     -> toggleNow (Snare         hit) >> draw >> loopPaused
      SDLK_d     -> toggleNow SnareFlam           >> draw >> loopPaused
      SDLK_b     -> toggleNow (Tom   Yellow  hit) >> draw >> loopPaused
      SDLK_k     -> toggleNow (Tom   Blue    hit) >> draw >> loopPaused
      SDLK_m     -> toggleNow (Tom   Green   hit) >> draw >> loopPaused
      SDLK_c     -> toggleNow (hihat Yellow     ) >> draw >> loopPaused
      SDLK_t     -> toggleNow (hihat Blue       ) >> draw >> loopPaused
      SDLK_g     -> toggleNow (hihat Green      ) >> draw >> loopPaused
      SDLK_j     -> toggleNow (Ride  Yellow     ) >> draw >> loopPaused
      SDLK_h     -> toggleNow (Ride  Blue       ) >> draw >> loopPaused
      SDLK_l     -> toggleNow (Ride  Green      ) >> draw >> loopPaused
      SDLK_n     -> toggleNow (Crash Yellow     ) >> draw >> loopPaused
      SDLK_e     -> toggleNow (Crash Blue       ) >> draw >> loopPaused
      SDLK_COMMA -> toggleNow (Crash Green      ) >> draw >> loopPaused
      _ -> loopPaused
    MouseButtonDown _ _ btn -> case btn of
      ButtonWheelDown -> do
        pos <- A.get $ vPosition . vTracks
        lns <- A.get $ vLines    . vTracks
        maybe (return ()) (setPosition . fst) $ Map.lookupGT pos lns
        draw
        loopPaused
      ButtonWheelUp -> do
        pos <- A.get $ vPosition . vTracks
        lns <- A.get $ vLines    . vTracks
        maybe (return ()) (setPosition . fst) $ Map.lookupLT pos lns
        draw
        loopPaused
      ButtonMiddle -> playAll >> loopPlaying
      _ -> loopPaused
    _ -> loopPaused

-- | The loop for a state that is playing currently. We must start by updating
-- our position, and drawing the board.
loopPlaying :: Prog ()
loopPlaying = do
  liftIO $ delay 1
  updatePlaying
  draw
  evt <- liftIO pollEvent
  case evt of
    Quit -> liftIO $ endContext >> exitSuccess
    KeyDown (Keysym k mods _) -> let
      hihat = if KeyModLeftShift `elem` mods then HihatC else HihatO
      hit   = if KeyModLeftCtrl  `elem` mods then Ghost  else Normal
      in case k of
      SDLK_UP -> A.modify vResolution (+ 20) >> draw >> loopPlaying
      SDLK_DOWN -> A.modify vResolution (\r -> max 0 $ r - 20) >> draw >> loopPlaying
      SDLK_LEFT -> do
        whilePaused $ modifySpeed $ \spd -> max 0.1 $ spd - 0.1
        loopPlaying
      SDLK_RIGHT -> do
        whilePaused $ modifySpeed $ \spd -> min 2 $ spd + 0.1
        loopPlaying
      SDLK_1 -> do
        (srcDrumL, srcDrumR) <- A.get $ vDrumAudio . vSources
        forM_ [srcDrumL, srcDrumR] toggleSource
        loopPlaying
      SDLK_BACKQUOTE -> do
        (srcSongL, srcSongR) <- A.get $ vSongAudio . vSources
        forM_ [srcSongL, srcSongR] toggleSource
        loopPlaying
      SDLK_BACKSPACE -> whilePaused (setPosition $ Both 0 0) >> loopPlaying
      SDLK_TAB -> do
        A.modify vMetronome not
        loopPlaying
      SDLK_q -> do
        dvn <- A.get vDivision
        case (numerator dvn, denominator dvn) of
          (1, d) -> do
            A.set vDivision $ 1 % (d + 1)
            makeLines
          _      -> return ()
        loopPlaying
      SDLK_a -> do
        dvn <- A.get vDivision
        case (numerator dvn, denominator dvn) of
          (1, d) | d >= 2 -> do
            A.set vDivision $ 1 % (d - 1)
            makeLines
          _               -> return ()
        loopPlaying
      SDLK_z     -> toggleNearest HihatF              >> draw >> loopPlaying
      SDLK_SPACE -> toggleNearest (Kick          hit) >> draw >> loopPlaying
      SDLK_v     -> toggleNearest (Snare         hit) >> draw >> loopPlaying
      SDLK_d     -> toggleNearest SnareFlam           >> draw >> loopPlaying
      SDLK_b     -> toggleNearest (Tom   Yellow  hit) >> draw >> loopPlaying
      SDLK_k     -> toggleNearest (Tom   Blue    hit) >> draw >> loopPlaying
      SDLK_m     -> toggleNearest (Tom   Green   hit) >> draw >> loopPlaying
      SDLK_c     -> toggleNearest (hihat Yellow     ) >> draw >> loopPlaying
      SDLK_t     -> toggleNearest (hihat Blue       ) >> draw >> loopPlaying
      SDLK_g     -> toggleNearest (hihat Green      ) >> draw >> loopPlaying
      SDLK_j     -> toggleNearest (Ride  Yellow     ) >> draw >> loopPlaying
      SDLK_h     -> toggleNearest (Ride  Blue       ) >> draw >> loopPlaying
      SDLK_l     -> toggleNearest (Ride  Green      ) >> draw >> loopPlaying
      SDLK_n     -> toggleNearest (Crash Yellow     ) >> draw >> loopPlaying
      SDLK_e     -> toggleNearest (Crash Blue       ) >> draw >> loopPlaying
      SDLK_COMMA -> toggleNearest (Crash Green      ) >> draw >> loopPlaying
      _ -> loopPlaying
    MouseButtonDown _ _ btn -> case btn of
      ButtonWheelDown -> do
        pos <- A.get $ vPosition . vTracks
        lns <- A.get $ vLines . vTracks
        case Map.splitLookup pos lns of
          (_, _, gt) -> case reverse $ take 2 $ Map.toAscList gt of
            (k, _) : _ -> setPosition k
            []         -> return ()
        loopPlaying
      ButtonWheelUp -> do
        pos <- A.get $ vPosition . vTracks
        lns <- A.get $ vLines . vTracks
        case Map.splitLookup pos lns of
          (lt, _, _) -> case reverse $ take 3 $ Map.toDescList lt of
            (k, _) : _ -> setPosition k
            []         -> return ()
        loopPlaying
      ButtonMiddle -> pauseAll >> loopPaused
      _ -> loopPlaying
    _ -> loopPlaying

pauseAll, playAll :: Prog ()
pauseAll = allSources >>= liftIO . pause
playAll  = allSources >>= liftIO . play

-- | Uses an audio source (or SDL's timer) to bump our position forward.
-- Also triggers metronome sounds, if we passed a bar line.
updatePlaying :: Prog ()
updatePlaying = do
  posOld <- A.get $ vPosition . vTracks
  lns <- A.get $ vLines . vTracks
  srcs <- allSources
  secNew <- case srcs of
    -- If there is an audio source: get our current position by copying the
    -- source's position. TODO: make sure the audio hasn't ended?
    src : _ -> do
      t <- liftIO $ OpenAL.get $ secOffset src
      a <- A.get $ vAudioStart . vSources
      return $ max 0 $ realToFrac $ t - a
    -- If there is no audio source: TODO: get our current position by finding
    -- the difference in SDL ticks from the place where we last recorded a
    -- ticks/position pair.
    []      -> undefined
  posNew <- positionBoth $ Seconds secNew
  A.set (vPosition . vTracks) posNew
  met <- A.get vMetronome
  -- Search the space in [posOld, posNew) for a Measure/Beat line.
  -- If so, trigger a metronome sound if the metronome is on.
  when (met && posNew > posOld) $ case Map.splitLookup posOld lns of
    (_, eq, gt) -> case Map.splitLookup posNew gt of
      (lt, _, _) -> let
        search = maybe id (:) eq $ Map.elems lt
        in when (any (`elem` [Measure, Beat]) search) $ do
          clk <- A.get $ vClick . vSources
          liftIO $ stop [clk]
          liftIO $ secOffset clk $= 0
          liftIO $ play [clk]
