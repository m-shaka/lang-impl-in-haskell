{-# LANGUAGE LambdaCase #-}

module Eval.Primitives(primitives, arith, if_) where

import           Eval.Combinators
import           Eval.Value

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except

import qualified Data.Map                   as MA

primitives :: MA.Map String Value
primitives = MA.fromList
  [ ("print", print_)
  ]

print_ :: Value
print_ = VLambda $ \x -> do
  liftIO $ print x
  pure x

arith :: (Int -> Int -> Int) -> Value
arith op = VLambda $ \(VInt a) -> pure $ VLambda $ \(VInt b) -> pure $ VInt (op a b)

if_ :: Value
if_ = VLambda $ \case
    VBool cond -> pure $ VLambda $ \tr -> pure $ VLambda $ \fl -> pure $ if cond then tr else fl
    v -> lift $ throwE $ "TypeError: " <> show v <> " is not boolean. "


