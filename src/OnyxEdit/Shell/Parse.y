{
module OnyxEdit.Shell.Parse
( parse, Expr(..)
) where

import qualified OnyxEdit.Shell.Scan as T
}

%name parse
%tokentype { T.Token }
%error { parseError }

%token
  num { T.Num $$ }
  ':' { T.Colon }
  s { T.Secs }
  m { T.Mins }
  bpm { T.BPM }
  bps { T.BPS }
  '(' { T.LParen }
  ')' { T.RParen }
  '+' { T.Plus }
  '-' { T.Minus }
  '*' { T.Star }
  '/' { T.Slash }
  now { T.Now }

%%

Expr : Expr0 '*' Expr { $1 * $3 }
     | Expr0 '/' Expr { $1 / $3 }
     | Expr0 { $1 }

Expr0 : Expr1 '+' Expr0 { $1 + $3 }
      | Expr1 '-' Expr0 { $1 - $3 }
      | Expr1 { $1 }

Expr1 : Expr2 s         { Secs $1 }
      | Expr2 m         { Secs ($1 * 60) }
      | Expr2 m Expr2 s { Secs (($1 * 60) + $3) }
      | Expr2 bps       { Bts $1 / Secs 1 }
      | Expr2 bpm       { Bts $1 / Secs 60 }
      | ':' Expr2       { Bts $2 }
      | Expr2 ':'       { Msrs $1 }
      | Expr2 ':' Expr2 { Msrs $1 + Bts $3 }

Expr2 : num { Num $1 }
      | now { Now }
      | '(' Expr ')' { $2 }

{

data Expr
  = Now
  | Num Rational
  | Secs Expr
  | Msrs Expr
  | Bts Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Eq, Ord, Show, Read)

instance Num Expr where
  fromInteger = Num . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mult

instance Fractional Expr where
  (/) = Div
  fromRational = Num

parseError :: [T.Token] -> a
parseError _ = error "Parse error"

}
