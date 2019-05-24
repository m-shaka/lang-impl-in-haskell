module Eval.Primitives where

import           Eval.Core

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map               as MA

primitives :: MA.Map String Value
primitives = MA.fromList
  [ ("$I", VLambda pure)
  , ("$K", VLambda $ \x -> pure . VLambda $ \_ -> pure x)
  , ("$S", VLambda $
      \f -> pure . VLambda $
            \g -> pure . VLambda $
                  \x -> do
                    f' <- f!x
                    g' <- g!x
                    f'!g'
    )
  , ("print", print_)
  ]

print_ :: Value
print_ = VLambda $ \x -> do
  liftIO $ print x
  pure x
