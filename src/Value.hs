{-# LANGUAGE LambdaCase #-}

module Value where

import           AST
import           GHC.Natural (Natural)

data Value =
  VBool Bool
  | VNat Natural
  deriving (Show)

eval :: Exp -> Either String Value
eval (Lit (LBool b)) = pure $ VBool b
eval (Lit Zero) = pure $ VNat 0
eval (IfExp cond x y) = eval cond >>= \case
  VBool b -> eval $ if b then x else y
  _ -> Left "TypeError"
eval (Succ n) = eval n >>= \case
  VNat n' -> pure . VNat $ n' + 1
  _ -> Left "TypeError"
eval (Pred (Lit Zero)) = pure $ VNat 0
eval (Pred n) = eval n >>= \case
  VNat n' -> pure . VNat $ n' - 1
  _ -> Left "TypeError"
eval (IsZero (Lit Zero)) = pure $ VBool True
eval (IsZero n@(Succ _))   = eval n >>= \case
  VNat _ -> pure $ VBool True
  _ -> Left "TypeError"
eval (IsZero (Pred n)) = eval n >>= \case
  VNat n -> pure . VBool $ n == 0 || n == 1
  _ -> Left "TypeError"
