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
  let lexer =  fileContent
  putStrLn (lexer)
  
  return ()
  
check1Arg :: IO([String])
check1Arg = do
   a <- getArgs
   
   return (a)