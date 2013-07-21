{-# LANGUAGE TupleSections #-}
module OnyxEdit.Draw
( draw
) where

import Prelude hiding ((.), id)
import Control.Category

import Graphics.UI.SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.IO.Class

--import Data.Accessor
import qualified Data.Accessor.Monad.Trans.State as A

import OnyxEdit.Types
import OnyxEdit.Program

noteSprite :: DrumEvent -> (Int, Int)
noteSprite n = (30 * x, 0) where
  x = case n of
    Kick Normal -> 13
    Snare Normal -> 14
    SnareFlam -> 18
    Tom ybg Normal -> 15 + fromEnum ybg
    HihatF -> 33
    HihatC ybg -> 27 + fromEnum ybg
    HihatO ybg -> 30 + fromEnum ybg
    Ride ybg -> 34 + fromEnum ybg
    Crash ybg -> 24 + fromEnum ybg
    Kick Ghost -> 19
    Snare Ghost -> 20
    Tom ybg Ghost -> 21 + fromEnum ybg

apply :: Int -> Int -> Surface -> Surface -> IO Bool
apply x y src dst = blitSurface src Nothing dst (Just offset)
  where offset = Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

timeToX :: Seconds -> Prog Int
timeToX pos = do
  now <- A.get $ vPosition . vTracks
  pps <- A.get vResolution
  return $ 150 + floor ((pos - toSeconds now) * fromIntegral pps)

-- | Returns the X position that the drawn line was centered at.
drawLine :: Position -> Line -> Prog Int
drawLine pos l = do
  x <- timeToX $ toSeconds pos
  scrn <- A.get $ vScreen    . vSurfaces
  surf <- A.get $ vNoteSheet . vSurfaces
  let clip = Just $ case l of
        Measure -> Rect 0  0 30 (26 * 5)
        Beat    -> Rect 30 0 30 (26 * 5)
        SubBeat -> Rect 60 0 30 (26 * 5)
      drawAt = Just $ Rect (x - 15) 100 0 0
  void $ liftIO $ blitSurface surf clip scrn drawAt
  return x

-- | Returns the X position that the drawn note was centered at.
drawNote :: Position -> DrumEvent -> Prog Int
drawNote pos note = do
  x <- timeToX $ toSeconds pos
  scrn <- A.get $ vScreen    . vSurfaces
  surf <- A.get $ vNoteSheet . vSurfaces
  let (clipX, clipY) = noteSprite note
      clip = Just $ Rect clipX clipY 30 (26 * 5)
      drawAt = Just $ Rect (x - 15) 100 0 0
  void $ liftIO $ blitSurface surf clip scrn drawAt
  return x

drawVisible :: (Position -> a -> Prog Int) -> Map.Map Position (Set.Set a) -> Prog ()
drawVisible drawFn mp = do
  now <- A.get $ vPosition . vTracks
  case Map.splitLookup now mp of
    (lt, eq, gt) -> do
      go $ expandSets $ Map.toDescList lt
      maybe (return ()) (mapM_ (drawFn now) . Set.toList) eq
      go $ expandSets $ Map.toAscList gt
  where
    expandSets :: [(a, Set.Set b)] -> [(a, b)]
    expandSets = concatMap $ \(x, sy) -> map (x,) $ Set.toList sy
    -- Draws events until it finds one that is not visible.
    go [] = return ()
    go ((pos, evt) : xs) = do
      x <- drawFn pos evt
      when (-100 < x && x < 1100) $ go xs

drawNotes :: Prog ()
drawNotes = A.get (vDrums . vTracks) >>= drawVisible drawNote

drawBG :: Prog ()
drawBG = void $ do
  scrn <- A.get $ vScreen     . vSurfaces
  bg   <- A.get $ vBackground . vSurfaces
  liftIO $ apply 0 0 bg scrn

drawLines :: Prog ()
drawLines = A.get (vLines . vTracks) >>=
  drawVisible drawLine . fmap Set.singleton

drawStaff :: Prog ()
drawStaff = do
  scrn <- A.get $ vScreen  . vSurfaces
  now  <- A.get $ vNowLine . vSurfaces
  stf  <- A.get $ vStaff   . vSurfaces
  void $ liftIO $ apply 0          100 stf scrn
  void $ liftIO $ apply (150 - 15) 0   now scrn

draw :: Prog ()
draw = do
  drawBG >> drawLines >> drawStaff >> drawNotes
  A.get (vScreen . vSurfaces) >>= liftIO . SDL.flip
