module Tsl
    ( someFunc
    ) where


import TslTokens
import TslGrammar
import TslType
import System.Environment
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Data.List

someFunc :: IO ()
someFunc = do
           (fileName:_) <- getArgs
           fileContent <- readFile fileName
           let parser = alexScanTokens(fileContent)
           print parser
           let grammar = parseTsl(parser)
           print grammar
           let typeCheck = typeOf [] grammar
           evalLoop (grammar, [], [], [])
           return ()


type CEK = (Control, Environment, Continuation, Environment)
type Control = Exp
type Environment = [(String,Exp)]
data Frame = HoleApp Exp Environment
 | ForHole2 (Literal -> Literal -> (Exp,Environment,Environment) -> Exp -> IO (Exp,Environment,Environment)) Exp (Exp,Environment,Environment) Exp Environment
 | ForHole (Literal -> (Exp,Environment,Environment) -> Exp -> IO (Exp,Environment,Environment)) (Exp,Environment,Environment) Exp Environment
 | StaticHole String Exp Environment

 | AppHole Exp
 | ReadHole Environment
 | OutputHole Environment

 | FunctionHole (Literal -> Literal) Environment
 | FunctionHole2 (Literal -> Literal -> Literal) Exp Environment
 | FunctionHole3 (Literal -> Literal -> Literal -> Literal) Exp Exp Environment
 | FunctionHole4 (Literal -> Literal -> Literal -> Literal -> Literal) Exp Exp Exp Environment

type Continuation = [Frame]


-- | Rule 1 of the CEK machine
eval :: CEK -> IO CEK
eval (Var v, envs, cons, senv) = return (newV' , envs, cons, senv)
  where newV = lookup v envs
        newV' | isNothing newV = fromMaybe (error ("Unassigned variable " ++ v)) (lookup v senv)
              | otherwise = fromJust newV

-- | Rule 4 of the CEK machine
eval (Lit v, env, (HoleApp exp env2):cons, senv) = return (exp, env2, AppHole (Lit v): cons, senv)

-- | Rule 5 of the CEK machine
eval (Lit v1 , env1, (AppHole (Cl v2 exp env2 )):cons, senv) = return (exp, (v2,Lit v1):env2, cons, senv)

eval (Static v _ e1 e2, env, cons, senv) = return (e1, env, StaticHole v e2 env:cons, senv)
eval (Lit v1, env, StaticHole v e env':cons, senv) = return (e, env', cons, (v,(Lit v1)):senv )
-- | Program End rule
eval (Lit v1 , _ , [], senv) =  return (END, [], [], senv)


-- | Evaluates Read function
eval (Read e, env, cons, senv) = return (e, env, ReadHole env:cons, senv)
eval (Lit v, env, ReadHole env':cons, senv) = do t <- readTl v
                                                 return (t, env', cons, senv)

-- | Evaluates Output function
eval (Output e, env, cons, senv) = return (e, env, OutputHole env:cons, senv)
eval (Lit v, env, OutputHole env':cons, senv) = do output v
                                                   return (END, env', cons, senv)
-- | Evaluate functions
eval (Lit v, env, FunctionHole4 f e1 e2 e3 env':cons, senv) = return (e1, env', FunctionHole3 (f v) e2 e3 env':cons, senv)
eval (Lit v, env, FunctionHole3 f e1 e2 env':cons, senv) = return (e1, env', FunctionHole2 (f v) e2 env':cons, senv)
eval (Lit v, env, FunctionHole2 f e env':cons, senv) = return (e, env', FunctionHole (f v) env':cons, senv)
eval (Lit v, env, FunctionHole f env':cons, senv) = return (Lit (f v) , env', cons, senv)
eval (Lit v, env, ForHole2 f e1 e2 e3 env':cons, senv) = return (e1, env', ForHole (f v) e2 e3 env': cons, senv)
eval (Lit v, env, ForHole f e1 e2 env':cons, senv) = do (e, forEnv, forSenv) <- f v e1 e2
                                                        return (e2, env', cons, forSenv)

-- | Evaluates Interlace function
eval (Interlace e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 interlace e2 env:cons, senv)
-- | Evaluates let statement (\x -> e2) . e1
eval (Let v _ e1 e2, env, cons, senv) = return (e1, env, f:cons, senv)
  where
    c = Cl v e2 env
    f = AppHole c

eval (For e1 e2 e3 e4, env, cons, senv) = return (e1, env, ForHole2 for e2 (e3,env,senv) e4 env:cons, senv)
eval (Size e, env, cons, senv) = return (e, env, FunctionHole size env:cons, senv)
eval (Rotate90 e, env, cons, senv) = return (e, env, FunctionHole rotate90 env:cons, senv)
eval (Rotate180 e, env, cons, senv) = return (e, env, FunctionHole rotate180 env:cons, senv)
eval (Rotate270 e, env, cons, senv) = return (e, env, FunctionHole rotate270 env:cons, senv)

eval (Scale e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 scale e2 env:cons, senv)

eval (FlipX e, env, cons, senv) = return (e, env, FunctionHole flipX env:cons, senv)
eval (FlipY e, env, cons, senv) = return (e, env, FunctionHole flipY env:cons, senv)
eval (FlipXY e, env, cons, senv) = return (e, env, FunctionHole flipXY env:cons, senv)
eval (Blank e, env, cons, senv) = return (e, env, FunctionHole blank env:cons, senv)

eval (And e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 andT e2 env:cons, senv)
eval (Or e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 orT e2 env:cons, senv)
eval (Not e, env, cons, senv) = return (e, env, FunctionHole notT env: cons, senv)

eval (Greater e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 greaterThan e2 env:cons, senv)
eval (GreaterEqual e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 greaterThanEquals e2 env:cons, senv)
eval (Less e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 lessThan e2 env:cons, senv)
eval (LessEqual e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 lessThanEquals e2 env:cons, senv)
eval (Plus e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 plus e2 env:cons, senv)
eval (Mult e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 mult e2 env:cons, senv)

eval (Swap e1 e2 e3, env, cons, senv) = return (e1, env, FunctionHole3 swap e2 e3 env:cons, senv)
eval (Change e1 e2 e3, env, cons, senv) = return (e1, env, FunctionHole3 change e2 e3 env:cons, senv)


eval (Subtile e1 e2 e3 e4, env, cons, senv) = return (e1, env, FunctionHole4 subTile e2 e3 e4 env:cons, senv)
eval (PlaceRight e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 placeRight e2 env:cons, senv)
eval (PlaceBelow e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 placeBelow e2 env:cons, senv)
eval (RepeatDown  e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 repeatDown e2 env:cons, senv)
eval (RepeatRight e1 e2, env, cons, senv) = return (e1, env, FunctionHole2 repeatRight e2 env:cons, senv)
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
output (Int x) = do
    print x
    return ()

size :: Literal -> Literal
size (Tile x) = Int (length x)

interlace :: Literal -> Literal -> Literal
interlace (Tile x) (Tile y) = undefined


rotate90 :: Literal -> Literal
rotate90 (Tile x) = Tile(transpose(reverse x))
rotate180 :: Literal -> Literal
rotate180 (Tile x) = rotate90.rotate90 $ Tile x
rotate270 :: Literal -> Literal
rotate270 (Tile x) = rotate90.rotate90.rotate90 $ Tile x

scale :: Literal -> Literal -> Literal
scale (Int x) (Tile y) = Tile (concat [ repeatLine x line | line <- repeatedAcross ])
                             where repeatedAcross = [ concat [ replicate x element | element <- line] | line <- y ]
                                   repeatLine 0 line = []
                                   repeatLine x line = line : repeatLine (x-1) line

flipX :: Literal -> Literal
flipX (Tile x) = Tile (reverse x)

flipY :: Literal -> Literal
flipY (Tile xs) = Tile ([ reverse x | x <- xs])

blank :: Literal -> Literal
blank (Int x) = Tile [[     '0'         | iterateX <- [1..x]] | iterateY <- [1..x]]

andT :: Literal -> Literal -> Literal
andT (Tile x) (Tile y) = Tile (zipWith (zipWith andNum)  x y)
                         
andNum :: Char -> Char -> Char
andNum '0' '0' = '0'
andNum '0' '1' = '0'
andNum '1' '0' = '0'
andNum '1' '1' = '1'

orT :: Literal -> Literal -> Literal
orT (Tile x) (Tile y) = Tile (zipWith (zipWith orNum)  x y)

orNum :: Char -> Char -> Char
orNum '0' '0' = '0'
orNum '0' '1' = '1'
orNum '1' '0' = '1'
orNum '1' '1' = '1'

notT :: Literal -> Literal
notT (Tile xs) = Tile [[  if y == '1' then '0' else '1'| y <- x] | x <-xs ]

add :: Literal -> Literal -> Literal
add (Int x) (Int y) = Int (x+y)

greaterThan :: Literal -> Literal -> Literal
greaterThan (Int x) (Int y) = Bool (x>y)

greaterThanEquals :: Literal -> Literal -> Literal
greaterThanEquals (Int x) (Int y) = Bool (x>=y)

lessThan :: Literal -> Literal -> Literal
lessThan (Int x) (Int y) = Bool (x<y)
lessThanEquals :: Literal -> Literal -> Literal
lessThanEquals (Int x) (Int y) = Bool (x<=y)

--size, x position, y position, tile
subTile :: Literal -> Literal -> Literal -> Literal -> Literal
subTile (Int n) (Int x) (Int y) (Tile t) = Tile [[  t !! yPos !! xPos |   xPos <- [x..(x+n-1)] ] | yPos <- [y..(y+n-1)]]

--make error checking
placeRight :: Literal -> Literal -> Literal
placeRight (Tile x) (Tile y) = Tile (zipWith (++) x y)

--make error checking
placeBelow :: Literal -> Literal -> Literal
placeBelow (Tile x) (Tile y) = Tile (x ++ y)

repeatDown :: Literal -> Literal -> Literal
repeatDown (Int x) (Tile ys) = Tile  [     y     | repetition <- [1..x], y <- ys ]

repeatRight :: Literal -> Literal -> Literal
repeatRight (Int x) (Tile ys) = Tile ([concat[     y     | repeat <- [1..x]] | y <- ys   ])

flipXY :: Literal -> Literal
flipXY (Tile x) = flipY $ flipX (Tile x)

swap :: Literal -> Literal -> Literal -> Literal
swap = undefined

change :: Literal -> Literal -> Literal -> Literal
change = undefined

plus :: Literal -> Literal -> Literal
plus (Int a) (Int b) = Int (a + b)

mult :: Literal -> Literal -> Literal
mult (Int a) (Int b) = Int (a * b)

for :: Literal -> Literal -> (Exp,Environment,Environment) -> Exp -> IO (Exp,Environment,Environment)
for (Int n) (Int m) (e,env,senv) exp = nextExp
  where nextExp | n < m = do senv' <- evalLoop (e, env, [], senv)
                             let n' = n + 1
                             for (Int n') (Int m) (e, env, senv') exp

                | otherwise = return (exp, env, senv)


-- | TODO: Add If statements and some form of recursion
evalLoop :: CEK -> IO Environment
evalLoop cek@(e,_,_,_) = do
                  next@(e1,_,_,_) <- eval cek
                  case next of
                    (END,_,_,senv) -> do
                      print "Program Terminated Cleanly"
                      return senv
                    _ -> evalLoop next

