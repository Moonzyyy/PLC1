{ 
module AlexTran where 
}

%wrapper "posn" 
$digit = [0-9]
-- digits
$alpha = [a-ZA-Z]
-- alpha

tokens :-
$white+       ; 
  "--".*        ; 
  Int          { \p s -> TokenTypeInt p }
  Bool         { \p s -> TokenTypeBool p }
  [1-9]          { \p s -> TokenDigit p (read s) } 
  true       { \p s -> TokenTrue p }
  false      { \p s -> TokenFalse p }
  $digit+ { \p s -> TokenInt p (read s) }
  Check          { \p s -> TokenCheck p }
  If             { \p s -> TokenIf p }
  Then           { \p s -> TokenThen p }
  Else           { \p s -> TokenElse p }
  Combine        { \p s -> TokenCombine p }
  Duplicate      { \p s -> TokenDuplicate p }
  Repeat         { \p s -> TokenRepeat p }
  Blank          { \p s -> TokenBlank p }
  Size           { \p s -> TokenSize p }
  For            { \p s -> TokenFor p }
  \+             { \p s -> TokenPlus p }
  \<             { \p s -> TokenLessThan p }
  \>             { \p s -> TokenGreaterThan p}
  \;             { \p s -> TokenSeq p }
  \(             { \p s -> TokenLParen p }
  \)             { \p s -> TokenRParen p }
  \[             { \p s -> TokenLSBracket p }
  \]             { \p s -> TokenRSBracket p }
{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- The token type: 
data Token =

  TokenTypeInt AlexPosn
  | TokenTypeBool AlexPosn
  | TokenDigit AlexPosn Int
  | TokenInt AlexPosn Int
  | TokenTrue AlexPosn
  | TokenFalse AlexPosn
  | TokenCheck AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenCombine AlexPosn
  | TokenDuplicate AlexPosn
  | TokenRepeat AlexPosn
  | TokenBlank AlexPosn
  | TokenSize AlexPosn
  | TokenFor AlexPosn
  | TokenPlus AlexPosn
  | TokenLessThan AlexPosn
  | TokenGreaterThan AlexPosn
  | TokenSeq AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenLSBracket AlexPosn
  | TokenRSBracket AlexPosn
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenTypeInt posn) = printPosn posn
tokenPosn (TokenTypeBool  posn) = printPosn posn
tokenPosn (TokenDigit  posn _) = printPosn posn
tokenPosn (TokenInt  posn _) = printPosn posn
tokenPosn (TokenCheck  posn) = printPosn posn
tokenPosn (TokenTrue  posn) = printPosn posn
tokenPosn (TokenFalse  posn) = printPosn posn
tokenPosn (TokenIf posn) = printPosn posn
tokenPosn (TokenThen posn) = printPosn posn
tokenPosn (TokenElse posn) = printPosn posn
tokenPosn (TokenCombine posn) = printPosn posn
tokenPosn (TokenDuplicate posn) = printPosn posn
tokenPosn (TokenRepeat posn) = printPosn posn
tokenPosn (TokenBlank posn) = printPosn posn
tokenPosn (TokenSize posn) = printPosn posn
tokenPosn (TokenFor posn) = printPosn posn
tokenPosn (TokenPlus posn) = printPosn posn
tokenPosn (TokenLessThan posn) = printPosn posn
tokenPosn (TokenGreaterThan posn) = printPosn posn
tokenPosn (TokenSeq posn) = printPosn posn
tokenPosn (TokenLParen posn) = printPosn posn
tokenPosn (TokenRParen posn) = printPosn posn
tokenPosn (TokenLSBracket posn) = printPosn posn
tokenPosn (TokenRSBracket posn) = printPosn posn

printPosn :: AlexPosn -> String
printPosn (AlexPn _ l c) = show(l) ++ ":" ++ show(c)
}