{
module TslGrammar where
import TslTokens
}

%name parseTsl
%tokentype { Token }
%error { parseError }
%token

  LET { TokenLet _ }
  IN { TokenIn _ }
  ASSIGN { TokenAssign _ }

  READ { TokenRead _ }
  OUTPUT { TokenOutput _ }
  COLON { TokenColon _ }

  LPAREN { TokenLParen _ }
  RPAREN { TokenRParen _ }

  IntType { TokenIntType _ }
  StringType { TokenStringType _ }
  TileType { TokenTileType _ }

  int { TokenInt _ $$}
  string { TokenString _ $$ }
  var { TokenVar _ $$}

%nonassoc IntType StringType TileType
%left ASSIGN
%left READ
%nonassoc int var
%%
Exp : Literal { Lit $1 }
    | LET LPAREN var COLON Type RPAREN ASSIGN Exp IN Exp {Let $3 $5 $8 $10}
    | READ Exp {Read $2}
    | OUTPUT Exp {Output $2}
    | var { Var $1 }

Literal : string { String $1 }
        | int { Int $1 }

Type : IntType { IntType }
     | StringType { StringType }
     | TileType { TileType }

{

parseError :: [Token] -> a
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Exp =
         Let String Type Exp Exp
         | Read Exp
         | Output Exp

         | Lit Literal

         | Var String
         deriving (Show, Eq)

data Literal =
          Int Int
          | String String
          | Tile [String]
          deriving (Show, Eq)

data Type =
          IntType
          | StringType
          | TileType
          | FunctionType
          deriving (Show, Eq)
}