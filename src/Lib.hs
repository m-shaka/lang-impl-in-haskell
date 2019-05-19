module Lib
    ( someFunc
    ) where

import           Control.Monad (forM_, join)
import           Eval
import           Parser
import           Utils

someFunc :: IO ()
someFunc = do
  s <- getContents
  case parse s of
   Left e    -> print e
   Right statements -> do
     forM_ statements (putStrLn . prettyStatement)
     res <- evalProgram statements
     case res of
       Left e  -> putStrLn e
       Right r -> print r
