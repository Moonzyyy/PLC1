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
           evalLoop (grammar, [], [])
           return ()


type CEK = (Control, Environment, Continuation)
type Control = Exp
type Environment = [(String,Exp)]
data Frame = HoleApp Exp Environment
 | ForHole Exp Environment
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
-- | Evaluate functions
eval (Lit v, env, FunctionHole4 f e1 e2 e3 env':cons) = return (e1, env', FunctionHole3 (f v) e2 e3 env':cons)
eval (Lit v, env, FunctionHole3 f e1 e2 env':cons) = return (e1, env', FunctionHole2 (f v) e2 env':cons)
eval (Lit v, env, FunctionHole2 f e env':cons) = return (e, env', FunctionHole (f v) env':cons)
eval (Lit v, env, FunctionHole f env':cons) = return (Lit (f v) , env', cons)
-- | Evaluates Interlace function
eval (Interlace e1 e2, env, cons) = return (e1, env, FunctionHole2 interlace e2 env:cons)

-- | Evaluates let statement (\x -> e2) . e1
eval (Let v _ e1 e2, env, cons) = return (e1, env, f:cons)
  where
    c = Cl v e2 env
    f = AppHole c

eval (For e1 e2, env, cons) = return (e1, env, ForHole  e2 env:cons)
eval (Size e, env, cons) = return (e, env, FunctionHole size env:cons)
eval (Rotate90 e, env, cons) = return (e, env, FunctionHole rotate90 env:cons)
eval (Rotate180 e, env, cons) = return (e, env, FunctionHole rotate180 env:cons)
eval (Rotate270 e, env, cons) = return (e, env, FunctionHole rotate270 env:cons)

eval (Scale e1 e2, env, cons) = return (e1, env, FunctionHole2 scale e2 env:cons)

eval (FlipX e, env, cons) = return (e, env, FunctionHole flipX env:cons)
eval (FlipY e, env, cons) = return (e, env, FunctionHole flipY env:cons)
eval (FlipXY e, env, cons) = return (e, env, FunctionHole flipXY env:cons)
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
eval (RepeatDown  e1 e2, env, cons) = return (e1, env, FunctionHole2 repeatDown e2 env:cons)
eval (RepeatRight e1 e2, env, cons) = return (e1, env, FunctionHole2 repeatRight e2 env:cons)
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
andT (Tile x) (Tile y) = undefined

orT :: Literal -> Literal -> Literal
orT (Tile x) (Tile y) = undefined

notT :: Literal -> Literal
notT (Tile xs) = Tile [[  if y == '1' then '0' else '1'| y <- x] | x <-xs ]

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

for :: Literal -> Exp -> Exp
for (Int 0) e = undefined
for (Int n) e = undefined 


-- | TODO: Add If statements and some form of recursion

evalLoop :: CEK -> IO ()
evalLoop cek = do
                  next <- eval cek
                  case next of
                    (END,_,_) -> do
                      print "Program Terminated Cleanly"
                      return ()
                    _ -> evalLoop next