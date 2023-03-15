module TslType where

import TslGrammar

type TypeEnv = [(String, Type)]

lookupEnv :: String -> TypeEnv -> Type
lookupEnv x env = maybe (error ("Unassigned variable: " ++ x)) id (lookup x env)

unparsedType :: Type -> String
unparsedType IntType = "int"
unparsedType StringType = "string"
unparsedType TileType = "tile"

addType :: TypeEnv -> String -> Type -> TypeEnv
addType env x t = (x,t):env

        
typeOf :: TypeEnv -> Exp -> Type
typeOf env (Lit (Int n)) = IntType
typeOf env (Lit (String s)) = StringType
typeOf env (Lit (Bool b)) = BoolType
typeOf env (Lit (Tile t)) = TileType

typeOf env (Var x) = lookupEnv x env

typeOf env (Let x t e1 e2) | typeOf env e1 == t = typeOf (addType env x t) e2
                           | otherwise = error "Type does not match in Let"

typeOf env (If e1 e2 e3) | typeOf env e1 == BoolType = typeOf env e2
                         | otherwise = error "Type does not match in If"

typeOf env (Interlace e1 e2) | typeOf env e1 == TileType && typeOf env e2 == TileType = TileType
                             | otherwise = error "Type does not match in Interlace"
typeOf env (Size x) | typeOf env x == TileType = IntType
                    | otherwise = error "Type does not match in Size"
typeOf env (Read x) | typeOf env x == StringType = TileType
                    | otherwise = error "Type does not match in Read"
typeOf env (Output x) | typeOf env x == TileType = BoolType
                      | otherwise = error "Type does not match in Output"