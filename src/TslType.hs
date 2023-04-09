module TslType where

import TslGrammar

type TypeEnv = [(String, Type)]

lookupEnv :: String -> TypeEnv -> TypeEnv -> Type
lookupEnv x env senv = maybe (senvCheck) id (lookup x env)
  where senvCheck = maybe (error "unassigned variable") id (lookup x senv)

unparsedType :: Type -> String
unparsedType IntType = "int"
unparsedType StringType = "string"
unparsedType TileType = "tile"

addType :: TypeEnv -> String -> Type -> TypeEnv
addType env x t = (x,t):env

        
typeOf :: TypeEnv -> TypeEnv -> Exp -> (Type,TypeEnv)
typeOf env senv (Lit (Int n)) = (IntType,senv)
typeOf env senv (Lit (String s)) = (StringType,senv)
typeOf env senv (Lit (Bool b)) = (BoolType,senv)
typeOf env senv (Lit (Tile t)) = (TileType,senv)

typeOf env senv (Var x) = (lookupEnv x env senv,senv)

typeOf env senv (Let x t e1 e2) | t1 == t = o
                                | otherwise = error "Type does not match in Let"
                                where (t1,senv1) = typeOf env senv e1
                                      o@(t2,senv2) = typeOf (addType env x t) senv1 e2

typeOf env senv (Static x t e1 e2) | t1 == t = o
                                   | otherwise = error "Type does not match in Static"
                                   where (t1,senv1) = typeOf env senv e1
                                         o@(t2,senv2) = typeOf env (addType senv1 x t) e2

typeOf env senv (If e1 e2 e3) | t1 == BoolType && t2 == t3 = o
                              | otherwise = error "Type does not match in If"
                              where (t1,senv1) = typeOf env senv e1
                                    (t2,senv2) = typeOf env senv1 e2
                                    o@(t3,senv3) = typeOf env senv2 e3

typeOf env senv (Interlace e1 e2) | t1 == TileType && t2 == TileType = o
                                  | otherwise = error "Type does not match in Interlace"
                                  where (t1,senv1) = typeOf env senv e1
                                        o@(t2,senv2) = typeOf env senv1 e2

typeOf env senv (Size e1) | t1 == TileType = (IntType,senv1)
                          | otherwise = error "Type does not match in Size"
                          where (t1,senv1) = typeOf env senv e1

typeOf env senv (Rotate90 e1) | t1 == TileType = (TileType,senv1)
                              | otherwise = error "Type does not match in Rotate90"
                              where o@(t1,senv1) = typeOf env senv e1
                        

typeOf env senv (Rotate180 e1) | t1 == TileType = (TileType,senv1)
                               | otherwise = error "Type does not match in Rotate180"
                               where o@(t1,senv1) = typeOf env senv e1

typeOf env senv (Rotate270 e1) | t1 == TileType = (TileType,senv1)
                               | otherwise = error "Type does not match in Rotate270"
                               where o@(t1,senv1) = typeOf env senv e1

typeOf env senv (Scale e1 e2) | t1 == IntType && t2 == TileType = (TileType,senv2)
                              | otherwise = error "Type does not match in Scale"
                              where (t1,senv1) = typeOf env senv e1
                                    o@(t2,senv2) = typeOf env senv1 e2

typeOf env senv (FlipX e1) | t1 == TileType = (TileType,senv1)
                           | otherwise = error "Type does not match in FlipX"
                           where o@(t1,senv1) = typeOf env senv e1

typeOf env senv (FlipY e1) | t1 == TileType = (TileType,senv1)
                           | otherwise = error "Type does not match in FlipY"
                           where o@(t1,senv1) = typeOf env senv e1
                           
typeOf env senv (FlipXY e1) | t1 == TileType = (TileType,senv1)
                            | otherwise = error "Type does not match in FlipXY"
                            where o@(t1,senv1) = typeOf env senv e1

typeOf env senv (Blank x) | t1 == IntType = (TileType,senv1)
                          | otherwise = error "Type does not match in Blank"
                          where o@(t1,senv1) = typeOf env senv x

typeOf env senv (And e1 e2) | t1 == TileType && t2 == TileType = (TileType,senv2)
                            | otherwise = error "Type does not match in And"
                            where (t1,senv1) = typeOf env senv e1
                                  o@(t2,senv2) = typeOf env senv1 e2
                       
typeOf env senv (Or e1 e2) | t1 == TileType && t2 == TileType = (TileType,senv2)
                           | otherwise = error "Type does not match in Or"
                           where (t1,senv1) = typeOf env senv e1
                                 o@(t2,senv2) = typeOf env senv1 e2
                             
typeOf env senv (Not e1) | t1 == TileType = (TileType,senv1)
                         | otherwise = error "Type does not match in Not"
                         where o@(t1,senv1) = typeOf env senv e1

typeOf env senv (Plus e1 e2) | t1 == IntType && t2 == IntType = (IntType,senv2)
                             | otherwise = error "Type does not match in Plus"
                             where (t1,senv1) = typeOf env senv e1
                                   o@(t2,senv2) = typeOf env senv1 e2
                                   
typeOf env senv (Minus e1 e2) | t1 == IntType && t2 == IntType = (IntType,senv2)
                              | otherwise = error "Type does not match in Minus"
                              where (t1,senv1) = typeOf env senv e1
                                    o@(t2,senv2) = typeOf env senv1 e2
                              
typeOf env senv (IDiv e1 e2) | t1 == IntType && t2 == IntType = (IntType,senv2)
                             | otherwise = error "Type does not match in IDiv"
                             where (t1,senv1) = typeOf env senv e1
                                   o@(t2,senv2) = typeOf env senv1 e2
                              
typeOf env senv (Mult e1 e2) | t1 == IntType && t2 == IntType = (IntType,senv2)
                             | otherwise = error "Type does not match in Mult"
                             where (t1,senv1) = typeOf env senv e1
                                   o@(t2,senv2) = typeOf env senv1 e2

typeOf env senv (Greater e1 e2) | t1 == IntType && t2 == IntType = (BoolType,senv2)
                                | otherwise = error "Type does not match in Greater"
                                where (t1,senv1) = typeOf env senv e1
                                      o@(t2,senv2) = typeOf env senv1 e2
                              
typeOf env senv (GreaterEqual e1 e2) | t1 == IntType && t2 == IntType = (BoolType,senv2)
                                     | otherwise = error "Type does not match in GreaterEqual"
                                     where (t1,senv1) = typeOf env senv e1
                                           o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (Less e1 e2) | t1 == IntType && t2 == IntType = (BoolType,senv2)
                             | otherwise = error "Type does not match in Less"
                             where (t1,senv1) = typeOf env senv e1
                                   o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (LessEqual e1 e2) | t1 == IntType && t2 == IntType = (BoolType,senv2)
                                  | otherwise = error "Type does not match in LessEqual"
                                  where (t1,senv1) = typeOf env senv e1
                                        o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (Subtile e1 e2 e3 e4) | t1 == IntType && t2 == IntType && t3 == IntType && t4 == TileType = (TileType,senv4)
                                      | otherwise = error "Type does not match in Subtile"
                                      where (t1,senv1) = typeOf env senv e1
                                            (t2,senv2) = typeOf env senv1 e2
                                            (t3,senv3) = typeOf env senv2 e3
                                            o@(t4,senv4) = typeOf env senv3 e4
                                       
typeOf env senv (PlaceRight e1 e2) | t1 == TileType && t2 == TileType = (TileType,senv2)
                                | otherwise = error "Type does not match in PlaceRight"
                                where (t1,senv1) = typeOf env senv e1
                                      o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (PlaceBelow e1 e2) | t1 == TileType && t2 == TileType = (TileType,senv2)
                                   | otherwise = error "Type does not match in PlaceBelow"
                                   where (t1,senv1) = typeOf env senv e1
                                         o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (RepeatRight e1 e2) | t1 == IntType && t2 == TileType = (TileType,senv2)
                                    | otherwise = error "Type does not match in RepeatRight"
                                    where (t1,senv1) = typeOf env senv e1
                                          o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (RepeatDown e1 e2) | t1 == IntType && t2 == TileType = (TileType,senv2)
                                   | otherwise = error "Type does not match in RepeatDown"
                                   where (t1,senv1) = typeOf env senv e1
                                         o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (RemoveRow e1 e2) | t1 == IntType && t2 == TileType = (TileType,senv2)
                                  | otherwise = error "Type does not match in RemoveRow"
                                  where (t1,senv1) = typeOf env senv e1
                                        o@(t2,senv2) = typeOf env senv1 e2
                                      
typeOf env senv (RemoveColumn e1 e2) | t1 == IntType && t2 == TileType = (TileType,senv2)
                                     | otherwise = error "Type does not match in RemoveColumn"
                                     where (t1,senv1) = typeOf env senv e1
                                           o@(t2,senv2) = typeOf env senv1 e2

--need to figure out for loops

typeOf env senv (Read e1) | t1 == StringType = (TileType,senv1)
                    | otherwise = error "Type does not match in Read"
                    where o@(t1,senv1) = typeOf env senv e1
                         
typeOf env senv (Output e1) | t1 == TileType = (BoolType,senv1)
                            | otherwise = error "Type does not match in Output"
                            where o@(t1,senv1) = typeOf env senv e1

typeOf env senv (For e1 e2 e3 e4) | t1 == IntType && t2 == IntType = o
                               | otherwise = error "Type does not match in For"
                               where (t1,senv1) = typeOf env senv e1
                                     (t2,senv2) = typeOf env senv1 e2
                                     (t3,senv3) = typeOf env senv2 e3
                                     o@(t4,senv4) = typeOf env senv3 e4