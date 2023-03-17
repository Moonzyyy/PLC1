{
module TslGrammar where
import TslTokens
}

%name parseTsl
%tokentype { Token }
%error { parseError }
%token

  LET { TokenLet _ }
  COLON { TokenColon _ }
  IN { TokenIn _ }
  ASSIGN { TokenAssign _ }

  IF { TokenIf _ }
  THEN { TokenThen _ }
  ELSE { TokenElse _ }

  READ { TokenRead _ }
  OUTPUT { TokenOutput _ }

  SIZE { TokenSize _ }
  INTERLACE { TokenInterlace _ }
  ROTATE90 { TokenRotate90 _ }
  ROTATE180 { TokenRotate180 _ }
  ROTATE270 { TokenRotate270 _ }
  SCALE { TokenScale _ }
  FLIPX { TokenFlipX _ }
  FLIPY { TokenFlipY _ }
  BLANK { TokenBlank _ }
  AND { TokenAnd _ }
  OR { TokenOr _ }
  NOT { TokenNot _ }
  GREATER { TokenGreater _ }
  LESS { TokenLess _ }
  GREATEREQUAL { TokenGreaterEqual _ }
  LESSEQUAL { TokenLessEqual _ }

  SUBTILE { TokenSubtile _ }
  PLACERIGHT { TokenPlaceRight _ }
  PLACEBELOW { TokenPlaceBelow _ }
  REPEATRIGHT { TokenRepeatRight _ }
  REPEATDOWN { TokenRepeatDown _ }



  LPAREN { TokenLParen _ }
  RPAREN { TokenRParen _ }

  PLUS { TokenPlus _ }

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
%nonassoc '(' ')'
%%
Exp : Literal { Lit $1 }
    | LET LPAREN var COLON Type RPAREN ASSIGN Exp IN Exp {Let $3 $5 $8 $10}
    | INTERLACE Exp Exp {Interlace $2 $3}
    | ROTATE90 Exp {Rotate90 $2}
    | ROTATE180 Exp {Rotate180 $2}
    | ROTATE270 Exp {Rotate270 $2}
    | SCALE Exp Exp {Scale $2 $3}
    | FLIPX Exp {FlipX $2}
    | FLIPY Exp {FlipY $2}
    | BLANK Exp {Blank $2}
    | SUBTILE Exp Exp Exp Exp {Subtile $2 $3 $4 $5}
    | PLACERIGHT Exp Exp {PlaceRight $2 $3}
    | PLACEBELOW Exp Exp {PlaceBelow $2 $3}
    | NOT Exp {Not $2}
    | Exp AND Exp {And $1 $3}
    | Exp OR Exp {Or $1 $3}
    | Exp GREATER Exp {Greater $1 $3}
    | Exp LESS Exp {Less $1 $3}
    | Exp GREATEREQUAL Exp {GreaterEqual $1 $3}
    | Exp LESSEQUAL Exp {LessEqual $1 $3}
    | IF LPAREN Exp RPAREN THEN LPAREN Exp RPAREN ELSE LPAREN Exp RPAREN {If $3 $7 $11}
    | READ Exp {Read $2}
    | OUTPUT Exp {Output $2}
    | Exp PLUS Exp {Plus $1 $3}
    | SIZE Exp {Size $2}
    | LPAREN Exp RPAREN { $2 }
    | REPEATRIGHT Exp Exp {RepeatRight $2 $3}
    | REPEATDOWN Exp Exp {RepeatDown $2 $3}
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
         | If Exp Exp Exp
         | Read Exp
         | Output Exp

         | Size Exp
         | Plus Exp Exp
         | Interlace Exp Exp
         | Rotate90 Exp
         | Rotate180 Exp
         | Rotate270 Exp
         | Scale Exp Exp
         | FlipX Exp
         | FlipY Exp
         | Blank Exp
         | Subtile Exp Exp Exp Exp
         | PlaceRight Exp Exp
         | PlaceBelow Exp Exp
         | Not Exp
         | And Exp Exp
         | Or Exp Exp
         | Greater Exp Exp
         | Less Exp Exp
         | GreaterEqual Exp Exp
         | LessEqual Exp Exp
         | RepeatRight Exp Exp
         | RepeatDown Exp Exp

         | Lit Literal

         | Var String

         | Cl String Exp [(String, Exp)]
         | END
         deriving (Show, Eq)

data Literal =
          Int Int
          | String String
          | Bool Bool
          | Tile [String]
          deriving (Show, Eq)

data Type =
          IntType
          | StringType
          | TileType
          | BoolType
          | FunctionType
          deriving (Show, Eq)
}