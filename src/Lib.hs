module Lib
    ( someFunc
    ) where

import           Control.Monad (join)
import           Parser
import           Value         (eval)

someFunc :: IO ()
someFunc = do
  s <- getContents
  let ast = parse s
  print ast
  print . join $ eval <$> ast
