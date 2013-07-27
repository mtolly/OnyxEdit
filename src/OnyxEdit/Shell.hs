module OnyxEdit.Shell where

import Prelude hiding ((.), id)
import Control.Category

import qualified OnyxEdit.Shell.Scan as S
import qualified OnyxEdit.Shell.Parse as P
import OnyxEdit.Types
import OnyxEdit.Program

--import Data.Accessor
import qualified Data.Accessor.Monad.Trans.State as A

interpret :: String -> P.Expr
interpret = P.parse . S.scan

data Value
  = Val Rational Int Int
  -- ^ Val r s b = r * seconds^s * beats^b
  | Now
  deriving (Eq, Ord, Show, Read)

modSeconds :: (Rational -> Rational) -> Prog Value
modSeconds f = do
  pos <- A.get $ vPosition . vTracks
  return $ Val (f $ toSeconds pos) 1 0

modBeats :: (Rational -> Rational) -> Prog Value
modBeats f = do
  pos <- A.get $ vPosition . vTracks
  return $ Val (f $ toBeats pos) 0 1

eval :: P.Expr -> Prog Value
eval e = case e of
  P.Now -> return Now
  P.Num r -> return $ Val r 0 0
  P.Secs x -> eval x >>= \v -> case v of
    Now -> do
      pos <- A.get $ vPosition . vTracks
      return $ Val (toSeconds pos) 1 0
    Val r s b -> return $ Val r (s + 1) b
  P.Msrs _ -> error "eval: TODO support measures"
  P.Bts x -> eval x >>= \v -> case v of
    Now -> do
      pos <- A.get $ vPosition . vTracks
      return $ Val (toBeats pos) 0 1
    Val r s b -> return $ Val r s (b + 1)
  P.Add x y -> do
    u <- eval x
    v <- eval y
    case (u, v) of
      (Now, Val r 1 0) -> modSeconds (+ r)
      (Val r 1 0, Now) -> modSeconds (+ r)
      (Now, Val r 0 1) -> modBeats (+ r)
      (Val r 0 1, Now) -> modBeats (+ r)
      (Val r a b, Val s c d) | a == c && b == d -> return $ Val (r + s) a b
      _ -> error "eval: unsupported addition"
  P.Sub x y -> do
    u <- eval x
    v <- eval y
    case (u, v) of
      (Now, Val r 1 0) -> modSeconds $ subtract r
      (Val r 1 0, Now) -> modSeconds (r -)
      (Now, Val r 0 1) -> modBeats $ subtract r
      (Val r 0 1, Now) -> modBeats (r -)
      (Val r a b, Val s c d) | a == c && b == d -> return $ Val (r - s) a b
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
