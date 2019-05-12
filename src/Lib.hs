module Lib
    ( someFunc
    ) where

import           Control.Monad (join)
import           Parser
import           Value

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
