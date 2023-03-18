{

module TslTokens where

}

%wrapper "posn"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabetic characters

tokens :-
  $white+       ;
  "--".*        ;

  Let           { \p _ -> TokenLet p}
  In            { \p _ -> TokenIn p}

  If            { \p _ -> TokenIf p}
  Then          { \p _ -> TokenThen p}
  Else          { \p _ -> TokenElse p}

  Read          { \p _ -> TokenRead p}
  Output        { \p _ -> TokenOutput p}
  Size          { \p _ -> TokenSize p}

  Interlace     { \p _ -> TokenInterlace p}
  Rotate90       { \p _ -> TokenRotate90 p}
  Rotate180      { \p _ -> TokenRotate180 p}
  Rotate270      { \p _ -> TokenRotate270 p}
  Scale         { \p _ -> TokenScale p}
  FlipX          { \p _ -> TokenFlipX p}
  FlipY          { \p _ -> TokenFlipY p}
  Blank         { \p _ -> TokenBlank p}
  And           { \p _ -> TokenAnd p}
  Or            { \p _ -> TokenOr p}
  Not           { \p _ -> TokenNot p}

  \>            { \p _ -> TokenGreater p}
  \<            { \p _ -> TokenLess p}
  \>\=           { \p _ -> TokenGreaterEqual p}
  \<\=           { \p _ -> TokenLessEqual p}

  Subtile       { \p _ -> TokenSubtile p}
  PlaceRight    { \p _ -> TokenPlaceRight p}
  PlaceBelow    { \p _ -> TokenPlaceBelow p}
  repeatRight { \p _ -> TokenRepeatRight p}
  repeatDown { \p _ -> TokenRepeatDown p}




  \:            { \p _ -> TokenColon p}
  \=            { \p _ -> TokenAssign p}

  \(            { \p _ -> TokenLParen p}
  \)            { \p _ -> TokenRParen p}

  \+            { \p _ -> TokenPlus p}

  Def        { \p _ -> TokenDef p}
  Comma       { \p _ -> TokenComma p}


  Int           { \p _ -> TokenIntType p }
  String        { \p _ -> TokenStringType p }
  Bool          { \p _ -> TokenBoolType p }
  Tile          { \p _ -> TokenTileType p }

  $digit+       { \p s -> TokenInt p (read s) }
  \"[$alpha$digit\.]*\"        { \p s -> TokenString p (init (tail s)) }
  $alpha [$alpha$digit]*   { \p s -> TokenVar p s }

{
-- Each action has type :: AlexPosn -> String -> Token
-- The token type:
data Token =
  TokenLet AlexPosn
  | TokenColon AlexPosn
  | TokenAssign AlexPosn
  | TokenIn AlexPosn

  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn

  | TokenRead AlexPosn
  | TokenOutput AlexPosn

  | TokenSize AlexPosn
  | TokenInterlace AlexPosn
  | TokenRotate90 AlexPosn
  | TokenRotate180 AlexPosn
  | TokenRotate270 AlexPosn
  | TokenScale AlexPosn
  | TokenFlipX AlexPosn
  | TokenFlipY AlexPosn
  | TokenBlank AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenNot AlexPosn

  | TokenGreater AlexPosn
  | TokenLess AlexPosn
  | TokenGreaterEqual AlexPosn
  | TokenLessEqual AlexPosn

  | TokenSubtile AlexPosn
  | TokenPlaceRight AlexPosn
  | TokenPlaceBelow AlexPosn
  | TokenRepeatRight AlexPosn
  | TokenRepeatDown AlexPosn

  | TokenLParen AlexPosn
  | TokenRParen AlexPosn

  | TokenPlus AlexPosn


  | TokenDef AlexPosn
  | TokenComma AlexPosn

  | TokenIntType AlexPosn
  | TokenStringType AlexPosn
  | TokenBoolType AlexPosn
  | TokenTileType AlexPosn

  | TokenInt AlexPosn Int
  | TokenString AlexPosn String
  | TokenVar AlexPosn String
  deriving (Eq,Show)

tokenPosn :: Token -> String

tokenPosn (TokenLet p) = printPosn p
tokenPosn (TokenColon p) = printPosn p
tokenPosn (TokenAssign p) = printPosn p
tokenPosn (TokenIn p) = printPosn p

tokenPosn (TokenIf p) = printPosn p
tokenPosn (TokenThen p) = printPosn p
tokenPosn (TokenElse p) = printPosn p

tokenPosn (TokenRead p) = printPosn p
tokenPosn (TokenOutput p) = printPosn p
tokenPosn (TokenSize p) = printPosn p

tokenPosn (TokenDef p) = printPosn p
tokenPosn (TokenComma p) = printPosn p

tokenPosn (TokenLParen p) = printPosn p
tokenPosn (TokenRParen p) = printPosn p

tokenPosn (TokenPlus p) = printPosn p

tokenPosn (TokenIntType p) = printPosn p
tokenPosn (TokenStringType p) = printPosn p
tokenPosn (TokenBoolType p) = printPosn p
tokenPosn (TokenTileType p) = printPosn p

tokenPosn (TokenInt p _) = printPosn p
tokenPosn (TokenString p _) = printPosn p
tokenPosn (TokenVar p _) = printPosn p

printPosn :: AlexPosn -> String
printPosn (AlexPn _ l c) = show(l) ++ ":" ++ show(c)
}