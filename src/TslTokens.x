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
  Read          { \p _ -> TokenRead p}
  Output        { \p _ -> TokenOutput p}
  \:            { \p _ -> TokenColon p}
  \=            { \p _ -> TokenAssign p}

  \(            { \p _ -> TokenLParen p}
  \)            { \p _ -> TokenRParen p}

  Int           { \p _ -> TokenIntType p }
  String        { \p _ -> TokenStringType p }
  Tile          { \p _ -> TokenTileType p }

  $digit+       { \p s -> TokenInt p (read s) }
  \".*\"        { \p s -> TokenString p (init (tail s)) }
  $alpha [$alpha$digit]*   { \p s -> TokenVar p s }

{
-- Each action has type :: AlexPosn -> String -> Token
-- The token type:
data Token =
  TokenLet AlexPosn
  | TokenAssign AlexPosn
  | TokenIn AlexPosn

  | TokenRead AlexPosn
  | TokenOutput AlexPosn
  | TokenColon AlexPosn

  | TokenLParen AlexPosn
  | TokenRParen AlexPosn

  | TokenIntType AlexPosn
  | TokenStringType AlexPosn
  | TokenTileType AlexPosn


  | TokenInt AlexPosn Int
  | TokenString AlexPosn String
  | TokenVar AlexPosn String
  deriving (Eq,Show)

tokenPosn :: Token -> String

tokenPosn (TokenLet p) = printPosn p
tokenPosn (TokenAssign p) = printPosn p
tokenPosn (TokenIn p) = printPosn p

tokenPosn (TokenRead p) = printPosn p
tokenPosn (TokenOutput p) = printPosn p
tokenPosn (TokenColon p) = printPosn p

tokenPosn (TokenLParen p) = printPosn p
tokenPosn (TokenRParen p) = printPosn p

tokenPosn (TokenIntType p) = printPosn p
tokenPosn (TokenStringType p) = printPosn p
tokenPosn (TokenTileType p) = printPosn p

tokenPosn (TokenInt p _) = printPosn p
tokenPosn (TokenString p _) = printPosn p
tokenPosn (TokenVar p _) = printPosn p

printPosn :: AlexPosn -> String
printPosn (AlexPn _ l c) = show(l) ++ ":" ++ show(c)
}