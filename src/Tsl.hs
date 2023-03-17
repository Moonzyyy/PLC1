module Tsl
    ( someFunc
    ) where

import TslTokens
import TslGrammar
import TslType
import System.Environment
import Data.Maybe (fromJust, isNothing, fromMaybe)

someFunc :: IO ()
someFunc = do
           (fileName:_) <- getArgs
           fileContent <- readFile fileName
           let parser = alexScanTokens(fileContent)
           print parser
           let grammar = parseTsl(parser)
           let typeCheck = typeOf [] grammar
           evalLoop (grammar, [], [])
           return ()


type CEK = (Control, Environment, Continuation)
type Control = Exp
type Environment = [(String,Exp)]
data Frame = HoleApp Exp Environment
 | InterlaceHole Literal Environment
 | InterlaceHole1 Exp Environment
 | AppHole Exp
 | ReadHole Environment
 | OutputHole Environment
 | SizeHole Environment

 | Rotate90Hole Environment
 | Rotate180Hole Environment
 | Rotate270Hole Environment
 | ScaleHole Literal Environment
 | ScaleHole1 Exp Environment
 | FlipXHole Environment
 | FlipYHole Environment
 | BlankHole Environment

 | AndHole  Literal Environment
 | AndHole1 Exp Environment
 | OrHole Literal Environment
 | OrHole1 Exp Environment
 | NotHole Environment
 | GreaterThanHole Literal Environment
 | GreaterThanHole1 Exp Environment
 | LessThanHole Literal Environment
 | LessThanHole1 Exp Environment
 | GreaterThanEqualHole Literal Environment
 | GreaterThanEqualHole1 Exp Environment
 | LessThanEqualHole Literal Environment
 | LessThanEqualHole1 Exp Environment
 | SubTileHole Literal Literal Literal Environment
 | SubTileHole1 Literal Literal Exp Environment
 | SubTileHole2 Literal Exp Exp Environment
 | SubTileHole3 Exp Exp Exp Environment
 | PlaceRightHole Literal Environment
 | PlaceRightHole1 Exp Environment
 | PlaceBelowHole Literal Environment
 | PlaceBelowHole1 Exp Environment
 | FunctionHole (Literal -> Literal) Environment
 | FunctionHole2 (Literal -> Literal -> Literal) Exp Environment
 | FunctionHole3 (Literal -> Literal -> Literal -> Literal) Exp Exp Environment
  | FunctionHole4 (Literal -> Literal -> Literal -> Literal -> Literal) Exp Exp Exp Environment

type Continuation = [Frame]

unpack :: Exp -> (Exp,Environment)
--unpack (Cl x e env1) = ((Lam x e) , env1)
unpack _ = error "Cannont unpack closure"

-- | Rule 1 of the CEK machine
eval :: CEK -> IO CEK
eval (Var v, envs, cons) = return (newV , envs, cons)
  where newV = fromMaybe (error ("Unassigned variable " ++ v)) (lookup v envs)

-- | Rule 4 of the CEK machine
eval (Lit v, env, (HoleApp exp env2):cons) = return (exp, env2, AppHole (Lit v): cons)

-- | Rule 5 of the CEK machine
eval (Lit v1 , env1, (AppHole (Cl v2 exp env2 )):cons) = return (exp, (v2,Lit v1):env2, cons)

-- | Program End rule
eval (Lit v1 , _ , []) =  return (END, [], [])

-- | Evaluates Read function
eval (Read e, env, cons) = return (e, env, ReadHole env:cons)
eval (Lit v, env, ReadHole env':cons) = do t <- readTl v
                                           return (t, env', cons)

-- | Evaluates Output function
eval (Output e, env, cons) = return (e, env, OutputHole env:cons)
eval (Lit v, env, OutputHole env':cons) = do output v
                                             return (END, env', cons)

-- | Evaluate Size function
eval (Size e, env, cons) = return (e, env, FunctionHole size env:cons)

-- | Evaluate functions
eval (Lit v, env, FunctionHole4 f e1 e2 e3 env':cons) = return (e1, env', FunctionHole3 (f v) e2 e3 env':cons)
eval (Lit v, env, FunctionHole3 f e1 e2 env':cons) = return (e1, env', FunctionHole2 (f v) e2 env':cons)
eval (Lit v, env, FunctionHole2 f e env':cons) = return (e, env', FunctionHole (f v) env':cons)
eval (Lit v, env, FunctionHole f env':cons) = return (Lit (f v) , env', cons)
-- | Evaluates Interlace function
eval (Interlace e1 e2, env, cons) = return (e1, env, FunctionHole2 interlace e2 env:cons)
  where
    f = InterlaceHole1 e2 env : cons
-- | Evaluates let statement (\x -> e2) . e1
eval (Let v _ e1 e2, env, cons) = return (e1, env, f:cons)
  where
    c = Cl v e2 env
    f = AppHole c

eval (Rotate90 e, env, cons) = return (e, env, FunctionHole rotate90 env:cons)
eval (Rotate180 e, env, cons) = return (e, env, FunctionHole rotate180 env:cons)
eval (Rotate270 e, env, cons) = return (e, env, FunctionHole rotate270 env:cons)

eval (Scale e1 e2, env, cons) = return (e1, env, FunctionHole2 scale e2 env:cons)

eval (FlipX e, env, cons) = return (e, env, FunctionHole flipX env:cons)
eval (FlipY e, env, cons) = return (e, env, FunctionHole flipY env:cons)

eval (Blank e, env, cons) = return (e, env, FunctionHole blank env:cons)

eval (And e1 e2, env, cons) = return (e1, env, FunctionHole2 andT e2 env:cons)
eval (Or e1 e2, env, cons) = return (e1, env, FunctionHole2 orT e2 env:cons)
eval (Not e, env, cons) = return (e, env, FunctionHole notT env: cons)

eval (Greater e1 e2, env, cons) = return (e1, env, FunctionHole2 greaterThan e2 env:cons)
eval (GreaterEqual e1 e2, env, cons) = return (e1, env, FunctionHole2 greaterThanEquals e2 env:cons)
eval (Less e1 e2, env, cons) = return (e1, env, FunctionHole2 lessThan e2 env:cons)
eval (LessEqual e1 e2, env, cons) = return (e1, env, FunctionHole2 lessThanEquals e2 env:cons)

eval (Subtile e1 e2 e3 e4, env, cons) = return (e1, env, FunctionHole4 subTile e2 e3 e4 env:cons)
eval (PlaceRight e1 e2, env, cons) = return (e1, env, FunctionHole2 placeRight e2 env:cons)
eval (PlaceBelow e1 e2, env, cons) = return (e1, env, FunctionHole2 placeBelow e2 env:cons)

eval _ = error "Runtime evaluation error"

-- | TODO: Add error handling for when length =/= height
readTl :: Literal -> IO Exp
readTl (String x) = do
  t <- readFile x
  return (Lit (Tile (lines t)))

output :: Literal -> IO ()
output (Tile x) = do
  print x
  return ()

size :: Literal -> Literal
size (Tile x) = Int (length x)

interlace :: Literal -> Literal -> Literal
interlace (Tile x) (Tile y) = undefined


rotate90 :: Literal -> Literal
rotate90 (Tile x) = undefined
rotate180 :: Literal -> Literal
rotate180 (Tile x) = rotate90 $ rotate90 $ Tile x
rotate270 :: Literal -> Literal
rotate270 (Tile x) = rotate90 $ rotate90 $ rotate90 $ Tile x

scale :: Literal -> Literal -> Literal
scale (Int x) (Tile y) = undefined

flipX :: Literal -> Literal
flipX (Tile x) = undefined

flipY :: Literal -> Literal
flipY (Tile x) = undefined

blank :: Literal -> Literal
blank (Int x) = undefined

andT :: Literal -> Literal -> Literal
andT (Tile x) (Tile y) = undefined

orT :: Literal -> Literal -> Literal
orT (Tile x) (Tile y) = undefined

notT :: Literal -> Literal
notT (Tile x) = undefined

add :: Literal -> Literal -> Literal
add (Int x) (Int y) = (Int (x+y))

greaterThan :: Literal -> Literal -> Literal
greaterThan (Int x) (Int y) = Bool (x>y)

greaterThanEquals :: Literal -> Literal -> Literal
greaterThanEquals (Int x) (Int y) = Bool (x>=y)

lessThan :: Literal -> Literal -> Literal
lessThan (Int x) (Int y) = Bool (x<y)
lessThanEquals :: Literal -> Literal -> Literal
lessThanEquals (Int x) (Int y) = Bool (x<=y)

subTile :: Literal -> Literal -> Literal -> Literal -> Literal
subTile (Int n) (Int x) (Int y) (Tile t) = undefined

--make error checking
placeRight :: Literal -> Literal -> Literal
placeRight (Tile x) (Tile y) = Tile (zipWith (++) x y)

--make error checking
placeBelow :: Literal -> Literal -> Literal
placeBelow (Tile x) (Tile y) = Tile (x ++ y)

-- | TODO: Add If statements and some form of recursion

evalLoop :: CEK -> IO ()
evalLoop cek = do
                  next <- eval cek
                  case next of
                    (END,_,_) -> do
                      print "Program Terminated Cleanly"
                      return ()
                    _ -> evalLoop next