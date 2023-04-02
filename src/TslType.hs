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

typeOf env (Static x t e1 e2) | typeOf env e1 == t = typeOf (addType env x t) e2
                              | otherwise = error "Type does not match in Static"


typeOf env (If e1 e2 e3) | typeOf env e1 == BoolType = typeOf env e2
                         | otherwise = error "Type does not match in If"

typeOf env (Interlace e1 e2) | typeOf env e1 == TileType && typeOf env e2 == TileType = TileType
                             | otherwise = error "Type does not match in Interlace"
typeOf env (Size x) | typeOf env x == TileType = IntType
                    | otherwise = error "Type does not match in Size"

typeOf env (Rotate90 x) | typeOf env x == TileType = TileType
                        | otherwise = error "Type does not match in Rotate90"

typeOf env (Rotate180 x) | typeOf env x == TileType = TileType
                         | otherwise = error "Type does not match in Rotate180"

typeOf env (Rotate270 x) | typeOf env x == TileType = TileType
                         | otherwise = error "Type does not match in Rotate270"

typeOf env (Scale e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = TileType
                         | otherwise = error "Type does not match in Scale"

typeOf env (FlipX x) | typeOf env x == TileType = TileType
                     | otherwise = error "Type does not match in FlipX"

typeOf env (FlipY x) | typeOf env x == TileType = TileType
                     | otherwise = error "Type does not match in FlipY"
typeOf env (FlipXY x) | typeOf env x == TileType = TileType
                     | otherwise = error "Type does not match in FlipXY"

typeOf env (Blank x) | typeOf env x == IntType = TileType
                     | otherwise = error "Type does not match in Blank"

typeOf env (And e1 e2) | typeOf env e1 == TileType && typeOf env e2 == TileType = TileType
                       | otherwise = error "Type does not match in And"
typeOf env (Or e1 e2) | typeOf env e1 == TileType && typeOf env e2 == TileType = TileType
                       | otherwise = error "Type does not match in Or"
typeOf env (Not x) | typeOf env x == TileType = TileType
                   | otherwise = error "Type does not match in Not"

typeOf env (Plus e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = IntType
                        | otherwise = error "Type does not match in Plus"
typeOf env (Minus e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = IntType
                        | otherwise = error "Type does not match in Minus"
typeOf env (IDiv e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = IntType
                        | otherwise = error "Type does not match in IDiv"
typeOf env (Mult e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = IntType
                        | otherwise = error "Type does not match in Mult"


typeOf env (Greater e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = BoolType
                        | otherwise = error "Type does not match in Greater"
typeOf env (GreaterEqual e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = BoolType
                                | otherwise = error "Type does not match in GreaterEqual"
typeOf env (Less e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = BoolType
                                | otherwise = error "Type does not match in Less"
typeOf env (LessEqual e1 e2) | typeOf env e1 == IntType && typeOf env e2 == IntType = BoolType
                                | otherwise = error "Type does not match in LessEqual"
typeOf env (Subtile e1 e2 e3 e4) | typeOf env e1 == IntType && typeOf env e2 == IntType && typeOf env e3 == IntType && typeOf env e4 == TileType = TileType
                                 | otherwise = error "Type does not match in Subtile"
typeOf env (PlaceRight e1 e2) | typeOf env e1 == TileType && typeOf env e2 == TileType = TileType
                                | otherwise = error "Type does not match in PlaceRight"
typeOf env (PlaceBelow e1 e2) | typeOf env e1 == TileType && typeOf env e2 == TileType = TileType
                                | otherwise = error "Type does not match in PlaceBelow"
typeOf env (RepeatRight e1 e2) | typeOf env e1 == IntType && typeOf env e2 == TileType = TileType
                                | otherwise = error "Type does not match in RepeatRight"
typeOf env (RepeatDown e1 e2) | typeOf env e1 == IntType && typeOf env e2 == TileType = TileType
                                | otherwise = error "Type does not match in RepeatDown"
typeOf env (RemoveRow e1 e2) | typeOf env e1 == IntType && typeOf env e2 == TileType = TileType
                                | otherwise = error "Type does not match in RemoveRow"
typeOf env (RemoveColumn e1 e2) | typeOf env e1 == IntType && typeOf env e2 == TileType = TileType
                                | otherwise = error "Type does not match in RemoveColumn"

--need to figure out for loops























typeOf env (Read x) | typeOf env x == StringType = TileType
                    | otherwise = error "Type does not match in Read"
typeOf env (Output x) | typeOf env x == TileType = BoolType
                      | otherwise = error "Type does not match in Output"