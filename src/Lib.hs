module Lib
    ( someFunc
    ) where

import AlexTran
import HappyTran
import System.Environment
import Control.Monad

someFunc :: IO ()
someFunc = do
           (fileName:_) <- getArgs
           fileContent <- readFile fileName
           let parser = alexScanTokens(fileContent)
           putStrLn(show(parser))
           let grammar = parseCalc(parser)
           putStrLn(show(reduce (grammar)))
           return ()

reduce :: Exp -> Maybe Exp
reduce (Plus (Int n) (Int m)) = Just $ Int (n+m)
reduce (Plus e1 e2) | newE1 /= Nothing = liftM2 Plus newE1 (Just e2)
                            | otherwise = liftM (Plus e1) newE2
                            where newE1 = reduce e1
                                  newE2 = reduce e2
