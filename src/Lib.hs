module Lib
    ( someFunc
    ) where

import AlexTran
import HappyTran
import System.Environment

someFunc :: IO ()
someFunc = do
           (fileName:_) <- getArgs
           fileContent <- readFile fileName
           let parser = alexScanTokens(fileContent)
           putStrLn(show(parser))
           let grammar = parseCalc(parser)
           putStrLn(show(grammar))
           return ()

