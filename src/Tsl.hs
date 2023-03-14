module Tsl
    ( interpret
    ) where

import TslTokens
import TslGrammar
import TslType
import System.Environment
import Data.Maybe (fromJust, isNothing)

interpret :: IO ()
interpret = do
           (fileName:_) <- getArgs
           fileContent <- readFile fileName
           let parser = alexScanTokens(fileContent)
           let grammar = parseTsl(parser)
           let typeCheck = typeOf [] grammar
           evalLoop (grammar, [], [])
           return ()

type CEK = (Control, Environment, Continuation)
type Control = Exp
type Environment = [(String,Exp)]
type Frame = Exp -> Exp
type Continuation = [Frame]

eval :: CEK -> IO (Maybe CEK)
eval (Var v, envs, cons) = return (Just (newV , envs, cons))
  where newV = maybe (error ("Unassigned variable " ++ v)) id (lookup v envs)

eval (Lit v, _, []) = return Nothing
eval (Lit v, env, con:cons) = return (Just (control, env, cons))
  where control = con (Lit v)

eval (Read e1@(Lit (String v)), env, cons) = do
  t <- readFile v
  let tile = Lit (Tile (lines t))
  return (Just (tile, env, cons))
eval (Read e1, env, cons) = return (Just (e1, env, (\e -> Read e):cons))

eval (Output e1@(Lit (Tile v)), env, cons) = do
  putStrLn (unlines v)
  return Nothing
eval (Output e1, env, cons) = return (Just (e1, env, (\e -> Output e):cons))

eval (Let v t e1@(Lit _) e2, env, cons) = return (Just (e2, (v,e1):env, cons))
eval (Let v t e1 e2, env, cons) = return (Just (e1, env, (\e -> Let v t e e2):cons))

eval (Lit v, envs, con:cons) = return (Just (control, envs, cons))
  where control = con (Lit v)

evalLoop :: CEK -> IO ()
evalLoop cek = do next <- eval cek
                  case isNothing next of
                    True -> return ()
                    False -> evalLoop (fromJust next)