module Eval.Primitives(primitives) where

import           Eval.Combinators
import           Eval.Value

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Map               as MA

primitives :: MA.Map String Value
primitives = MA.fromList
  [ ("$I", combI_)
  , ("$K", combK_)
  , ("$S", combS_)
  , ("print", print_)
  ]

print_ :: Value
print_ = VLambda $ \x -> do
  liftIO $ print x
  pure x
