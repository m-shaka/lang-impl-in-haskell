module Lib
    ( someFunc
    ) where

import           AST    (eval)
import           Parser

someFunc :: IO ()
someFunc = do
  s <- getContents
  print $ eval <$> parse s
