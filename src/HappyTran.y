{ 
module HappyTran where 
import AlexTran
}

%name parseCalc 
%tokentype { Token }
%error { parseError }
%token 
    digit   { TokenDigit _ $$ }
    int     { TokenInt _ $$ } 
    true { TokenTrue _ }
    false { TokenFalse _}
    If      { TokenIf _ } 
    Then    { TokenThen _ } 
    Else    { TokenElse _ } 
    Join    { TokenCombine _ }
    Duplicate { TokenDuplicate _ }
    Repeat { TokenRepeat _ }
    Blank { TokenBlank _ }
    Size { TokenSize _ }
    For { TokenFor _ }
    IntType { TokenTypeInt _ }
    BoolType { TokenTypeBool _ }
    '<' { TokenLessThan _ }
    '>' { TokenGreaterThan _}
    '+' { TokenPlus _ }
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
    | Exp '<' Exp {LessThan $1 $3}
    | Exp '>' Exp {GreaterThan $1 $3}
    | Exp '+' Exp {Plus $1 $3}
    | int {Int $1}

Type : IntType {IntType}
     | BoolType {BoolType}

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))
data Exp = Int Int
         | Bool Bool
         | If Exp Exp Exp
         | For Exp Exp
         | Seq Exp Exp
         | Plus Exp Exp
         | LessThan Exp Exp
         | GreaterThan Exp Exp
         deriving Show 

data Type = IntType
          | BoolType
          deriving (Show, Eq)
}