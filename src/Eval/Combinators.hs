module Eval.Combinators where

import           Eval.Value

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)

combI_ :: Value
combI_ = VLambda pure

combK_ = VLambda $ \x -> pure . VLambda $ \_ -> pure x

combS_ = VLambda $
      \f -> pure . VLambda $
            \g -> pure . VLambda $ \x -> fmap2 (!) (f!x) (g!x)

fmap2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
fmap2 f x y = do
  x' <- x
  y' <- y
  f x' y'

infixl 0 !
(!) :: Value -> Value -> Eval Value
VLambda f ! x = f x
_ ! x = lift $ throwE "ApplicationError"
