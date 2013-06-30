{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Sound.ALUT

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (void, forM_)
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Data.Word

import Paths_rhythmal

data Note
  = Kick
  | FootHihat
  | Snare
  | CloseHihat
  | OpenHihat
  | RideB
  | RideG
  | CrashY
  | CrashB
  | CrashG
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

noteSprite :: Note -> (Int, Int)
noteSprite = \case
  Kick -> (0, 0)
  FootHihat -> (120, 60)
  Snare -> (0, 30)
  CloseHihat -> (60, 60)
  OpenHihat -> (90, 60)
  RideB -> (60, 90)
  RideG -> (60, 120)
  CrashY -> (30, 60)
  CrashB -> (30, 90)
  CrashG -> (30, 120)

data Program = Program
  { vScreen :: Surface
  , vNoteSprites :: Surface
  , vBackground :: Surface
  , vChart :: Map.Map Rational (Set.Set Note)
  , vPosition :: Rational
  , vResolution :: Int
  , vPlayFrom :: Maybe (Word32, Rational)
  , vDrumAudio :: (Source, Source)
  , vSongAudio :: (Source, Source)
  , vAudioStart :: Float
  }

type Prog = StateT Program IO

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormatAlpha

apply :: Int -> Int -> Surface -> Surface -> IO Bool
apply x y src dst = blitSurface src Nothing dst (Just offset)
  where offset = Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applyCenter :: Int -> Int -> Surface -> Surface -> IO Bool
applyCenter x y src dst = do
  Rect _ _ w h <- getClipRect src
  apply (x - div w 2) (y - div h 2) src dst

playUpdate :: Prog Bool
playUpdate = gets vPlayFrom >>= \case
  Nothing     -> return False
  Just (w, r) -> do
    w' <- liftIO getTicks
    modify $ \prog -> prog { vPosition = r + fromIntegral (w' - w) / 500 }
    return True

timeToX :: Rational -> Prog Int
timeToX pos = do
  now <- gets vPosition
  pps <- gets vResolution
  return $ 150 + floor ((pos - now) * fromIntegral pps)

noteToY :: Note -> Int
noteToY n = 290 - 25 * case n of
  Kick -> 0
  FootHihat -> 0
  Snare -> 1
  CloseHihat -> 2
  OpenHihat -> 2
  RideB -> 3
  RideG -> 4
  CrashY -> 2
  CrashB -> 3
  CrashG -> 4

drawNote :: Rational -> Note -> Prog ()
drawNote pos note = void $ do
  surf <- gets vNoteSprites
  scrn <- gets vScreen
  x <- timeToX pos
  let (clipX, clipY) = noteSprite note
      clip = Just $ Rect clipX clipY 30 30
      drawAt = Just $ Rect (x - 15) (noteToY note - 15) 0 0
  liftIO $ blitSurface surf clip scrn drawAt

drawNotes :: Prog ()
drawNotes = do
  notes <- gets vChart
  now <- gets vPosition
  case Map.splitLookup now notes of
    (lt, eq, gt) -> do
      drawLess lt
      maybe (return ()) (mapM_ (drawNote now) . Set.toList) eq
      drawMore gt
  where
    drawLess mp = mapM_ (\(pos, nts) -> mapM_ (drawNote pos) $ Set.toList nts) $ Map.toList mp
    drawMore = drawLess

kitchen :: Map.Map Rational (Set.Set Note)
kitchen = Map.fromList
  [ (0, Set.fromList [Kick, CrashG])
  , (1, Set.fromList [RideB])
  , (1.5, Set.fromList [Snare])
  , (2, Set.fromList [RideB])
  , (2.5, Set.fromList [OpenHihat])
  , (3, Set.fromList [Kick, FootHihat, RideB])
  , (3.5, Set.fromList [CloseHihat])
  , (4, Set.fromList [RideB])
  , (4.5, Set.fromList [Snare])
  , (5, Set.fromList [RideB])
  , (5.5, Set.fromList [OpenHihat])
  ]

drawBG :: Prog ()
drawBG = void $ do
  scrn <- gets vScreen
  bg <- gets vBackground
  liftIO $ apply 0 0 bg scrn

draw :: Prog ()
draw = drawBG >> drawNotes >> gets vScreen >>= liftIO . Graphics.UI.SDL.flip

loadSource :: FilePath -> Source -> IO ()
loadSource f src = createBuffer (File f) >>= \buf -> buffer src $= Just buf

main :: IO ()
main = withInit [InitTimer, InitVideo] $ withProgNameAndArgs runALUT $ \_ args -> do

  -- Get screen, load sprites
  scrn <- setVideoMode 1000 480 32 [SWSurface]
  gemFile <- getDataFileName "gems.png"
  gemSheet <- loadImage gemFile
  bgFile <- getDataFileName "bg.png"
  bgImage <- loadImage bgFile

  -- Load audio
  [srcDrumL, srcDrumR, srcSongL, srcSongR] <- genObjectNames 4
  loadSource (args !! 0) srcDrumL
  loadSource (args !! 1) srcDrumR
  loadSource (args !! 2) srcSongL
  loadSource (args !! 3) srcSongR
  forM_ [srcDrumL, srcSongL] $ \src ->
    liftIO $ sourcePosition src $= Vertex3 (-1) 0 0
  forM_ [srcDrumR, srcSongR] $ \src ->
    liftIO $ sourcePosition src $= Vertex3 1 0 0

  let prog = Program
        { vScreen = scrn
        , vNoteSprites = gemSheet
        , vBackground = bgImage
        , vChart = kitchen
        , vPosition = 0
        , vResolution = 100
        , vPlayFrom = Nothing
        , vDrumAudio = (srcDrumL, srcDrumR)
        , vSongAudio = (srcSongL, srcSongR)
        , vAudioStart = 39.726
        }
  evalStateT (draw >> inputLoop) prog

inputLoop :: Prog ()
inputLoop = playUpdate >>= \b -> (if b then draw else return ()) >> inputLoop' b

inputLoop' :: Bool -> Prog ()
inputLoop' b = liftIO pollEvent >>= \case
  Quit -> return ()
  KeyDown (Keysym SDLK_UP _ _) -> do
    modify $ \prog -> prog { vResolution = vResolution prog + 20 }
    draw
    inputLoop
  KeyDown (Keysym SDLK_DOWN _ _) -> do
    modify $ \prog -> prog { vResolution = max 20 $ vResolution prog - 20 }
    draw
    inputLoop
  MouseButtonDown _ _ ButtonWheelDown -> if b then inputLoop' b else do
    modify $ \prog -> prog { vPosition = vPosition prog + 0.5 }
    draw
    inputLoop
  MouseButtonDown _ _ ButtonWheelUp -> if b then inputLoop' b else do
    modify $ \prog -> prog { vPosition = vPosition prog - 0.5 }
    draw
    inputLoop
  KeyDown (Keysym SDLK_1 _ _) -> do
    now <- gets vPosition
    modify $ \prog -> prog { vChart = Map.alter f now $ vChart prog }
    draw
    inputLoop
    where f Nothing = Just $ Set.singleton Kick
          f (Just notes) = case (Set.lookupIndex Kick notes, Set.size notes) of
            (Just 0, 0) -> Nothing
            (Just i, _) -> Just $ Set.deleteAt i notes
            (Nothing, _) -> Just $ Set.insert Kick notes
  KeyDown (Keysym SDLK_SPACE _ _) -> do
    gets vPlayFrom >>= \case
      Nothing -> do
        tks <- liftIO getTicks
        now <- gets vPosition
        (srcDrumL, srcDrumR) <- gets vDrumAudio
        (srcSongL, srcSongR) <- gets vSongAudio
        strt <- gets vAudioStart
        modify $ \prog -> prog { vPlayFrom = Just (tks, now) }
        forM_ [srcDrumL, srcDrumR, srcSongL, srcSongR] $ \src ->
          liftIO $ secOffset src $= strt + realToFrac now / 2
        liftIO $ play [srcDrumL, srcDrumR, srcSongL, srcSongR]
        inputLoop
      Just _ -> do
        (srcDrumL, srcDrumR) <- gets vDrumAudio
        (srcSongL, srcSongR) <- gets vSongAudio
        modify $ \prog -> prog { vPlayFrom = Nothing }
        liftIO $ pause [srcDrumL, srcDrumR, srcSongL, srcSongR]
        inputLoop
  KeyDown (Keysym SDLK_d _ _) -> do
    (srcDrumL, srcDrumR) <- gets vDrumAudio
    g <- liftIO $ Sound.ALUT.get $ sourceGain srcDrumL
    forM_ [srcDrumL, srcDrumR] $ \src ->
      liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    inputLoop
  KeyDown (Keysym SDLK_s _ _) -> do
    (srcSongL, srcSongR) <- gets vSongAudio
    g <- liftIO $ Sound.ALUT.get $ sourceGain srcSongL
    forM_ [srcSongL, srcSongR] $ \src ->
      liftIO $ sourceGain src $= if g > 0.5 then 0 else 1
    inputLoop
  _    -> inputLoop
