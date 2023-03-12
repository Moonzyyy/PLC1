module TranType where
import HappyTran

type TypeEnv = [(String, Type)]

typeLookup :: TypeEnv -> String -> Type
typeLookup [] x = error ("unbound variable " ++ x)
typeLookup ((y,t):env) x | x == y = t
                         | otherwise = typeLookup env x

typeAdd :: TypeEnv -> String -> Type -> TypeEnv
typeAdd env x t = (x,t):env


unparseType :: Type -> String
unparseType IntType = "int"
unparseType BoolType = "bool"

typeOf :: TypeEnv -> Exp -> Type
typeOf env (Int n) = IntType
typeOf env (Bool b) = BoolType
-- typeOf env (Var x) = typeLookup env x TODO implement Var
typeOf env (Plus e1 e2) = case (typeOf env e1, typeOf env e2) of 
                               (IntType, IntType) -> IntType
                               _ -> error "type error in addition"
typeOf env (If e1 e2 e3) = case (typeOf env e1, typeOf env e2, typeOf env e3) of 
                            (BoolType, t2, t3) -> if t2 == t3 then t2 else error "type error in if"
                            _ -> error "type error in if"
-- TODO implement Let
--typeOf env (Let x t e1 e2) = if t == typeOf env e1 then typeOf (typeAdd env x t) e2 else error "type error in let"


typeOf env (LessThan e1 e2) = case (typeOf env e1, typeOf env e2) of 
                              (IntType, IntType) -> BoolType
                              _ -> error "type error in less than"
typeOf env (GreaterThan e1 e2) = case (typeOf env e1, typeOf env e2) of 
                              (IntType, IntType) -> BoolType
                              _ -> error "type error in greater than"