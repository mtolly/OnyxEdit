{
module OnyxEdit.Shell.Parse
( scan, parse
) where

import OnyxEdit.Shell.Scan
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  num { Number $$ }
  '(' { LParen }
  ')' { RParen }
  '+' { Plus }
  '-' { Minus }
  '*' { Star }
  '/' { Slash }

%%

Expr : Expr0 '*' Expr { Mult $1 $3 }
     | Expr0 '/' Expr { Div $1 $3 }
     | Expr0 { $1 }

Expr0 : Expr1 '+' Expr0 { Add $1 $3 }
      | Expr1 '-' Expr0 { Sub $1 $3 }
      | Expr1 { $1 }

Expr1 : num { Val $1 }
      | '(' Expr ')' { $2 }

{

data Expr
  = Val Number
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Eq, Ord, Show, Read)

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
