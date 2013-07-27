{
{-# OPTIONS_GHC -w #-}
module OnyxEdit.Shell.Scan (scan) where

import Data.Char (isDigit)
}

%wrapper "basic"

$digit = 0-9
@integer = $digit+
@decimal = $digit+ ( \. $digit+ )?

tokens :-

$white+ ;

now { const LitNow }

@decimal { LitNumber . dec }
@decimal [Ss] { LitSeconds . dec }
@decimal [Mm] { LitSeconds . (* 60) . dec }
@decimal [Mm] @decimal [Ss] { \s ->
  let (a, b) = break (`elem` "Mm") s
    in LitSeconds $ (60 * dec a) + dec (tail b)
  }
\: @decimal { LitMeasureBeats 0 . dec . tail }
@decimal \: { \s -> LitMeasureBeats (dec s) 0 }
@decimal \: @decimal { \s ->
  let (a, b) = break (== ':') s
    in LitMeasureBeats (dec a) (dec $ tail b)
  }
@decimal [Bb][Pp][Mm] { LitBPS . (/ 60) . dec }
@decimal [Bb][Pp][Ss] { LitBPS . dec }
@decimal [Bb][Pp][Ss] { LitBPS . dec }

\( { const LParen }
\) { const RParen }
\+ { const Plus }
\- { const Minus }
\* { const Star }
\/ { const Slash }

{

data Token
  = LitNow
  | LitNumber Rational
  | LitSeconds Rational
  | LitMeasureBeats Rational Rational
  | LitBPS Rational
  | LParen
  | RParen
  | Plus
  | Minus
  | Star
  | Slash
  deriving (Eq, Ord, Show, Read)

scan :: String -> [Token]
scan = alexScanTokens

dec :: String -> Rational
dec = fst . readDecimal

-- | Reads either "digits" or "digits.digits" from the start of a string.
readDecimal :: String -> (Rational, String)
readDecimal s0 = case span isDigit s0 of
  (whole, s1) -> let
    wholeRat = fromInteger $ read whole
    in case s1 of
      '.' : s2 -> case span isDigit s2 of
        (part, s3) -> let
          decimalDenom = read $ '1' : map (const '0') part
          partRat = fromInteger (read part) / fromInteger decimalDenom
          in (wholeRat + partRat, s3)
      _ -> (wholeRat, s1)

}