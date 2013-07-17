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

noteSprite :: Note -> (Int, Int)
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

drawLine :: Position -> Line -> Prog ()
drawLine pos l = void $ do
  x <- timeToX $ toSeconds pos
  scrn <- A.get $ vScreen    . vSurfaces
  surf <- A.get $ vNoteSheet . vSurfaces
  let clip = Just $ case l of
        Measure -> Rect 0  0 30 (26 * 5)
        Beat    -> Rect 30 0 30 (26 * 5)
        SubBeat -> Rect 60 0 30 (26 * 5)
      drawAt = Just $ Rect (x - 15) 100 0 0
  liftIO $ blitSurface surf clip scrn drawAt

drawNote :: Position -> Note -> Prog Int
drawNote pos note = do
  surf <- A.get $ vNoteSheet . vSurfaces
  scrn <- A.get $ vScreen . vSurfaces
  x <- timeToX $ toSeconds pos
  let (clipX, clipY) = noteSprite note
      clip = Just $ Rect clipX clipY 30 (26 * 5)
      drawAt = Just $ Rect (x - 15) 100 0 0
  void $ liftIO $ blitSurface surf clip scrn drawAt
  return x

-- | Draws notes until it finds one that is definitely not visible.
drawVisibleNotes :: [(Position, Note)] -> Prog ()
drawVisibleNotes [] = return ()
drawVisibleNotes ((pos, note) : pns) = do
  x <- drawNote pos note
  when (-100 < x && x < 1100) $ drawVisibleNotes pns

drawNotes :: Prog ()
drawNotes = do
  notes <- A.get $ vDrums    . vTracks
  now   <- A.get $ vPosition . vTracks
  case Map.splitLookup now notes of
    (lt, eq, gt) -> do
      drawLess lt
      maybe (return ()) (mapM_ (drawNote now) . Set.toList) eq
      drawMore gt
  where
    drawLess = drawVisibleNotes . expandSets . Map.toDescList
    drawMore = drawVisibleNotes . expandSets . Map.toAscList
    expandSets :: [(a, Set.Set b)] -> [(a, b)]
    expandSets = concatMap $ \(x, sy) -> map (x,) $ Set.toList sy

drawBG :: Prog ()
drawBG = void $ do
  scrn <- A.get $ vScreen     . vSurfaces
  bg   <- A.get $ vBackground . vSurfaces
  liftIO $ apply 0 0 bg scrn

drawLines :: Prog ()
drawLines = A.get (vLines . vTracks) >>= mapM_ (uncurry drawLine) . Map.toList

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
