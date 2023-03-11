{ 
module MDLTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 

tokens :-
$white+       ; 
  "--".*        ; 
  Int          { \s -> TokenTypeInt }
  Bool         { \s -> TokenTypeBool }
  [1-9]          { \p s -> TokenDigit p (read s) } 
  true       { \s -> TokenTrue}
  false      { \s -> TokenFalse}
  $digit $digit+ { \p s -> TokenInt p (read s) }
  Check          { \p s -> TokenCheck p }
  If             { \p s -> TokenIf p }
  Then           { \p s -> TokenThen p }
  Else           { \p s -> TokenElse p }
  \<             { \s -> TokenLessThan}
  \>             { \s -> TokenGreaterThan}
  \;             { \p s -> TokenSeq p }
  \(             { \p s -> TokenLParen p }
  \)             { \p s -> TokenRParen p }
  \[             { \p s -> TokenLSBracket p }
  \]             { \p s -> TokenRSBracket p }

{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- The token type: 
data MDLToken = 

  TokenTypeInt                 |
  TokenTypeBool                |
  TokenDigit AlexPosn Int      |
  TokenInt AlexPosn Int        |
  TokenTrue AlexPosn           |
  TokenFalse AlexPosn          |
  TokenCheck AlexPosn          |
  TokenIf AlexPosn             |
  TokenThen AlexPosn           |
  TokenElse AlexPosn           |
  TokenLessThan AlexPosn       |
  TokenGreaterThan AlexPosn    |
  TokenSeq AlexPosn            |
  TokenLParen AlexPosn         |
  TokenRParen AlexPosn         |
  TokenLSBracket AlexPosn      |
  TokenRSBracket AlexPosn      
  deriving (Eq,Show) 

tokenPosn :: MDLToken -> String
tokenPosn (TokenTypeInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDigit  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCheck  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


}