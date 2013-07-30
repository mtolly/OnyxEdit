module OnyxEdit.Shell (interpret, Value(..), eval) where

import Prelude hiding ((.), id)
import Control.Category

import qualified OnyxEdit.Shell.Scan as S
import qualified OnyxEdit.Shell.Parse as P
import OnyxEdit.Types
import OnyxEdit.Program

--import Data.Accessor
import qualified Data.Accessor.Monad.Trans.State as A

import qualified Data.Map as Map

interpret :: String -> P.Expr
interpret = P.parse . S.scan

data Value
  = Val Rational Int Int
  -- ^ Val r s b = r * seconds^s * beats^b
  | ValBoth Seconds Beats
  deriving (Eq, Ord, Show, Read)

eval :: P.Expr -> Prog Value
eval e = case e of
  P.Now -> do
    pos <- A.get $ vPosition . vTracks
    return $ ValBoth (toSeconds pos) (toBeats pos)
  P.Num r -> return $ Val r 0 0
  P.Secs x -> eval x >>= \v -> case v of
    ValBoth s _ -> return $ Val s 1 0
    Val r s b -> return $ Val r (s + 1) b
  P.Msrs x -> eval x >>= \v -> case v of
    Val r 0 0 -> do
      lns <- A.get $ vLines . vTracks
      let msrs = filter ((Measure ==) . snd) $ Map.toAscList lns
          (a, b) = properFraction r
      return $ case drop a msrs of
        (mpos, _) : msrs' -> if b == 0
          then Val (toBeats mpos) 0 1
          else case msrs' of
            (mpos', _) : _ -> let
              (bts0, bts1) = (toBeats mpos, toBeats mpos')
              in Val (bts0 + ((bts1 - bts0) * b)) 0 1
            [] -> error "eval: invalid measure number"
        [] -> error "eval: invalid measure number"
    _ -> error "eval: unsupported to-measures conversion"
  P.Bts x -> eval x >>= \v -> case v of
    ValBoth _ b -> return $ Val b 0 1
    Val r s b -> return $ Val r s (b + 1)
  P.Add x y -> do
    u <- eval x
    v <- eval y
    case (u, v) of
      (ValBoth s _, Val r 1 0) -> return $ Val (s + r) 1 0
      (ValBoth _ b, Val r 0 1) -> return $ Val (b + r) 0 1
      (Val r 1 0, ValBoth s _) -> return $ Val (r + s) 1 0
      (Val r 0 1, ValBoth _ b) -> return $ Val (r + b) 1 0
      (Val r a b, Val s c d) | a == c && b == d -> return $ Val (r + s) a b
      (Val s 1 0, Val b 0 1) -> do
        pos0 <- positionBoth $ Seconds s
        pos1 <- positionBoth $ Beats $ toBeats pos0 + b
        return $ ValBoth (toSeconds pos1) (toBeats pos1)
      (Val b 0 1, Val s 1 0) -> do
        pos0 <- positionBoth $ Beats b
        pos1 <- positionBoth $ Seconds $ toSeconds pos0 + s
        return $ ValBoth (toSeconds pos1) (toBeats pos1)
      _ -> error "eval: unsupported addition"
  P.Sub x y -> do
    u <- eval x
    v <- eval y
    case (u, v) of
      (ValBoth s _, Val r 1 0) -> return $ Val (s - r) 1 0
      (ValBoth _ b, Val r 0 1) -> return $ Val (b - r) 0 1
      (Val r 1 0, ValBoth s _) -> return $ Val (r - s) 1 0
      (Val r 0 1, ValBoth _ b) -> return $ Val (r - b) 1 0
      (Val r a b, Val s c d) | a == c && b == d -> return $ Val (r - s) a b
      (Val s 1 0, Val b 0 1) -> do
        pos0 <- positionBoth $ Seconds s
        pos1 <- positionBoth $ Beats $ toBeats pos0 - b
        return $ ValBoth (toSeconds pos1) (toBeats pos1)
      (Val b 0 1, Val s 1 0) -> do
        pos0 <- positionBoth $ Beats b
        pos1 <- positionBoth $ Seconds $ toSeconds pos0 - s
        return $ ValBoth (toSeconds pos1) (toBeats pos1)
      _ -> error "eval: unsupported subtraction"
  P.Mult x y -> do
    u <- eval x
    v <- eval y
    case (u, v) of
      (Val r a b, Val s c d) -> return $ Val (r * s) (a + c) (b + d)
      _ -> error "eval: unsupported multiplication"
  P.Div x y -> do
    u <- eval x
    v <- eval y
    case (u, v) of
      (Val r a b, Val s c d) -> return $ Val (r / s) (a - c) (b - d)
      _ -> error "eval: unsupported multiplication"
