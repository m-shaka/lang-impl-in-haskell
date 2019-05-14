module Lib
    ( someFunc
    ) where

import           Control.Monad (join)
import           Eval
import           Parser

someFunc :: IO ()
someFunc = do
  s <- getContents
  case parse s of
   Left e    -> print e
   Right ast -> do
     print ast
     res <- evalProgram ast
     case res of
       Left e -> putStrLn e
       r      -> print r
