{ 
module MDLGrammar where 
import MDLTokens 
}

%name parseCalc 
%tokentype { MDLToken } 
%error { parseError }
%token 
    digit   { TokenDigit _ $$ } 
    int     { TokenInt _ $$ } 
    If      { TokenIf _ } 
    Then    { TokenThen _ } 
    Else    { TokenElse _ } 
    Join    { TokenCombine _ }
    Duplicate { TokenDuplicate _ }
    Repeat { TokenRepeate _ }
    Blank { TokenBlank _ }
    Size { TokenSize _ }
    For { TokenFor _ }
    ';'     { TokenSeq _ }
    '('     { TokenLParen _ } 
    ')'     { TokenRParen _ }
    '['     { TokenLSBracket _ }
    ']'     { TokenRSBracket _ } 

%right Then
%right Else 
%right ';' 
%% 
Exp : If Exp Then Exp Else Exp  { Cond $2 $4 $6 } 
    | Exp ';' Exp               { Seq $1 $3 } 
    | '(' Exp ')'                   { $2 }

{ 
parseError :: [MDLToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))
data Exp = Cond Exp Exp Exp
         | Seq Exp Exp 
         deriving Show 
} 