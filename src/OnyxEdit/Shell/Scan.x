{
{-# OPTIONS_GHC -w #-}
module OnyxEdit.Shell.Scan (scan, Token(..)) where

import Data.Char (isDigit)
}

%wrapper "basic"

$digit = 0-9
@decimal = $digit+ ( \. $digit+ )?

tokens :-

$white+ ;

@decimal { Num . dec }
\: { const Colon }
[Ss] { const Secs }
[Mm] { const Mins }
[Bb] { const Bts }
[Bb][Pp][Mm] { const BPM }
[Bb][Pp][Ss] { const BPS }
\( { const LParen }
\) { const RParen }
\+ { const Plus }
\- { const Minus }
\* { const Star }
\/ { const Slash }
[Nn][Oo][Ww] { const Now }

{

data Token
  = Num Rational
  | Colon
  | Secs
  | Mins
  | Bts
  | BPM
  | BPS
  | LParen
  | RParen
  | Plus
  | Minus
  | Star
  | Slash
  | Now
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