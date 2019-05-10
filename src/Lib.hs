module Lib
    ( someFunc
    ) where

import           Parser

someFunc :: IO ()
someFunc = do
  s <- getContents
  print $ parse s
