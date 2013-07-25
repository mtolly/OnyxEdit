module Main (main) where

import Prelude hiding ((.), id, lex)
import Control.Category

import Graphics.UI.SDL hiding (flip)
import Graphics.UI.SDL.Image

import Sound.OpenAL hiding (get)
import qualified Sound.OpenAL as OpenAL

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intersect)

import Data.Ratio

import Text.Read.Lex
import Text.ParserCombinators.ReadP

import qualified Sound.MIDI.File.Load as Load

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import System.Exit
import System.Environment (getArgs)
import System.IO

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

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

main :: IO ()
main = withInit [InitTimer, InitVideo] $ do

  -- Initialize OpenAL
  beginContext

  -- Get screen, load sprites
  putErrLn "Loading images..."
  scrn       <- setVideoMode 1000 480 32 [SWSurface]
  gemSheet   <- getDataFileName "gems.png"  >>= loadImage
  bgImage    <- getDataFileName "bg.png"    >>= loadImage
  staffImage <- getDataFileName "staff.png" >>= loadImage
  now        <- getDataFileName "now.png"   >>= loadImage

  -- Load audio
  midPath : drumPath : songPath : restArgs <- getArgs
  let startTime = case restArgs of
        []     -> 0
        st : _ -> read st
  putErrLn "Loading drums audio..."
  (srcDrumL, srcDrumR) <- loadStereo16WAV drumPath
  putErrLn "Loading backing audio..."
  (srcSongL, srcSongR) <- loadStereo16WAV songPath
  putErrLn "Loading misc. sounds..."
  srcClick <- getDataFileName "click.wav" >>= loadMono16WAV

  -- Load MIDI
  mid <- Load.fromFile midPath

  let surfaces = Surfaces
        { vScreen_     = scrn
        , vNoteSheet_  = gemSheet
        , vBackground_ = bgImage
        , vStaff_      = staffImage
        , vNowLine_    = now
        }
      sources = Sources
        { vAudioStart_ = startTime
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

  flip evalStateT prog $ do
    clearAll
    liftIO $ putErrLn "Loading MIDI..."
    loadMIDI mid
    draw
    liftIO $ putErrLn "Done!"
    loopPaused

toggleSource :: Source -> Prog ()
toggleSource src = liftIO $ do
  g <- OpenAL.get $ sourceGain src
  sourceGain src $= if g > 0.5 then 0 else 1

staffLines :: DrumEvent -> [Int]
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

toggleNote :: DrumEvent -> Position -> Prog ()
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

toggleNow :: DrumEvent -> Prog ()
toggleNow n = A.get (vPosition . vTracks) >>= toggleNote n

toggleNearest :: DrumEvent -> Prog ()
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

sharedKeys :: Bool -> Event -> Maybe (Prog ())
sharedKeys isPlaying evt = case evt of
  Quit -> Just $ liftIO $ endContext >> exitSuccess
  KeyDown (Keysym k mods _) -> let
    hihat  = if KeyModLeftShift `elem` mods then HihatC else HihatO
    hit    = if KeyModLeftCtrl  `elem` mods then Ghost  else Normal
    toggle = if isPlaying then toggleNearest else toggleNow
    in case k of
      SDLK_UP -> Just $ A.modify vResolution (+ 20)
      SDLK_DOWN -> Just $ A.modify vResolution $ \r -> max 0 $ r - 20
      SDLK_LEFT -> Just $ modifySpeed $ \spd -> max 0.1 $ spd - 0.1
      SDLK_RIGHT -> Just $ modifySpeed $ \spd -> min 2 $ spd + 0.1
      SDLK_1 -> Just $ do
        (srcDrumL, srcDrumR) <- A.get $ vDrumAudio . vSources
        forM_ [srcDrumL, srcDrumR] toggleSource
      SDLK_BACKQUOTE -> Just $ do
        (srcSongL, srcSongR) <- A.get $ vSongAudio . vSources
        forM_ [srcSongL, srcSongR] toggleSource
      SDLK_BACKSPACE -> Just $ setPosition $ Both 0 0
      SDLK_TAB -> Just $ A.modify vMetronome not
      SDLK_q -> Just $ do
        dvn <- A.get vDivision
        case (numerator dvn, denominator dvn) of
          (1, d) -> do
            A.set vDivision $ 1 % (d + 1)
            makeLines
          _      -> return ()
      SDLK_a -> Just $ do
        dvn <- A.get vDivision
        case (numerator dvn, denominator dvn) of
          (1, d) | d >= 2 -> do
            A.set vDivision $ 1 % (d - 1)
            makeLines
          _               -> return ()
      SDLK_z     -> Just $ toggle HihatF
      SDLK_SPACE -> Just $ toggle $ Kick          hit
      SDLK_v     -> Just $ toggle $ Snare         hit
      SDLK_d     -> Just $ toggle SnareFlam
      SDLK_b     -> Just $ toggle $ Tom   Yellow  hit
      SDLK_k     -> Just $ toggle $ Tom   Blue    hit
      SDLK_m     -> Just $ toggle $ Tom   Green   hit
      SDLK_c     -> Just $ toggle $ hihat Yellow
      SDLK_t     -> Just $ toggle $ hihat Blue
      SDLK_g     -> Just $ toggle $ hihat Green
      SDLK_j     -> Just $ toggle $ Ride  Yellow
      SDLK_h     -> Just $ toggle $ Ride  Blue
      SDLK_l     -> Just $ toggle $ Ride  Green
      SDLK_n     -> Just $ toggle $ Crash Yellow
      SDLK_e     -> Just $ toggle $ Crash Blue
      SDLK_COMMA -> Just $ toggle $ Crash Green
      _ -> Nothing
  MouseButtonDown _ _ btn -> case btn of
    ButtonWheelDown -> Just $ do
      pos <- A.get $ vPosition . vTracks
      lns <- A.get $ vLines    . vTracks
      if isPlaying
        then case Map.splitLookup pos lns of
          (_, _, gt) -> case reverse $ take 2 $ Map.toAscList gt of
            (k, _) : _ -> setPosition k
            []         -> return ()
        else maybe (return ()) (setPosition . fst) $ Map.lookupGT pos lns
    ButtonWheelUp -> Just $ do
      pos <- A.get $ vPosition . vTracks
      lns <- A.get $ vLines    . vTracks
      if isPlaying
        then case Map.splitLookup pos lns of
          (lt, _, _) -> case reverse $ take 3 $ Map.toDescList lt of
            (k, _) : _ -> setPosition k
            []         -> return ()
        else maybe (return ()) (setPosition . fst) $ Map.lookupLT pos lns
    _ -> Nothing
  _ -> Nothing

data Quantity
  = Raw Rational
  | Secs Seconds
  | Bts Beats
  | Msr Int Beats
  | BPM BPM
  deriving (Eq, Ord, Show, Read)

getQuantity :: String -> Maybe Quantity
getQuantity w = let
  lex' = do
    lxm <- lex
    case lxm of
      EOF -> pfail
      _   -> return lxm
  in case filter (null . snd) $ readP_to_S (many lex') w of
    [(lxms, _)] -> case lxms of
      [Number s, Ident "s"] -> Just $ Secs $ numberToRational s
      [Number b, Ident "b"] -> Just $ Bts $ numberToRational b
      [Number m, Ident "m", Number b, Ident "b"] ->
        flip fmap (numberToInteger m) $ \i ->
          Msr (fromIntegral i) $ numberToRational b
      [Number bpm, Ident "bpm"] -> Just $ BPM $ numberToRational bpm
      [Number n] -> Just $ Raw $ numberToRational n
      _ -> Nothing
    _ -> Nothing

-- | The loop for a state that isn't in playing mode. We don't have to draw;
-- just handle the next event.
loopPaused :: Prog ()
loopPaused = do
  liftIO $ delay 1
  evt <- liftIO pollEvent
  case sharedKeys False evt of
    Just act -> act >> draw >> loopPaused
    Nothing  -> case evt of
      MouseButtonDown _ _ ButtonMiddle -> playAll >> loopPlaying
      _ -> liftIO (hReady stdin) >>= \ready -> if ready
        then liftIO getLine >>= \ln -> let
          unrec = liftIO (putStrLn "Unrecognized command.") >> loopPaused
          in case words ln of
            ["now"] -> do
              A.get (vPosition . vTracks) >>= liftIO . print
              loopPaused
            [w] -> case getQuantity w of
              Just (Secs s) -> do
                setPosition $ Seconds s
                draw
                loopPaused
              Just (Bts b) -> do
                setPosition $ Beats b
                draw
                loopPaused
              Just (Msr _ _) -> undefined
              Just (BPM bpm) -> do
                now <- A.get $ vPosition . vTracks
                tmps <- A.get $ vTempos . vTracks
                loadTempos $ Map.mapKeys toBeats $
                  Map.insert now (bpm / 60) tmps
                draw
                loopPaused
              Just (Raw _) -> do
                liftIO $ putStrLn "Number requires a unit."
                loopPaused
              _ -> unrec
            _ -> unrec
        else loopPaused

-- | The loop for a state that is playing currently. We must start by updating
-- our position, and drawing the board.
loopPlaying :: Prog ()
loopPlaying = do
  liftIO $ delay 1
  updatePlaying
  draw
  evt <- liftIO pollEvent
  case sharedKeys True evt of
    Just act -> act >> loopPlaying
    Nothing  -> case evt of
      MouseButtonDown _ _ ButtonMiddle -> pauseAll >> loopPaused
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
