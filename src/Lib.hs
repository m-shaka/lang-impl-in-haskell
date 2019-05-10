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
     evalProgram ast >>= print
