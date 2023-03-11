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
    true { TokenTrue}
    false { TokenFalse}
    If      { TokenIf _ } 
    Then    { TokenThen _ } 
    Else    { TokenElse _ } 
    Join    { TokenCombine _ }
    Duplicate { TokenDuplicate _ }
    Repeat { TokenRepeate _ }
    Blank { TokenBlank _ }
    Size { TokenSize _ }
    For { TokenFor _ }
    IntType { TokenTypeInt}
    BoolType { TokenTypeBool}
    ';'     { TokenSeq _ }
    '('     { TokenLParen _ } 
    ')'     { TokenRParen _ }
    '['     { TokenLSBracket _ }
    ']'     { TokenRSBracket _ } 

%right Then
%right Else 
%right ';'  
%left '(' ')'
%left '<'
%left '>'  
%% 
Exp : If Exp Then Exp Else Exp  { If $2 $4 $6 } 
    | Exp ';' Exp               { Seq $1 $3 } 
    | '(' Exp ')'                   { $2 }
    | For Exp Then Exp          { For $2 $4 }
    | true {Bool True}
    | false {Bool False}
    | true {Bool True}
    | false {Bool False}
    | let '(' var ':' Type')' '=' Exp in Exp {Let $3 $5 $8 $10}
    | Exp '<' Exp {LessThan $1 $3}
    | Exp '+' Exp {Plus $1 $3}

Type : IntType {IntType}
     | BoolType {BoolType}

{ 
parseError :: [MDLToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))
data Exp = Int Int
         | Bool Bool
         | If Exp Exp Exp
         | Seq Exp Exp 
         | Bool Bool
         | LessThan Exp Exp
         deriving Show 

data Type = IntType
          | BoolType
          deriving (Show, Eq)
} 