module TranType where
import HappyTran





unparseType :: Type -> String
unparseType IntType = "int"
unparseType BoolType = "bool"

typeOf :: TypeEnv -> Exp -> Type
typeOf env (Int n) = IntType
typeOf env (Bool b) = BoolType