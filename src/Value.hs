{-# LANGUAGE LambdaCase #-}

module Value (evalProgram) where

import           AST
import           Control.Monad              (forM, (>=>))
import           Control.Monad.Identity
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                   as MA
import           GHC.Natural                (Natural)

data Value =
  VBool Bool
  | VNat Natural
  | VDecl
  deriving (Show)

type Env = MA.Map String Value

type Eval a = StateT Env (ExceptT String IO) a

eval :: Exp -> Eval Value
eval (Lit (LBool b)) = pure $ VBool b
eval (Lit Zero) = pure $ VNat 0
eval (IfExp cond x y) = eval cond >>= \case
  VBool b -> eval $ if b then x else y
  _ -> lift $ throwE "TypeError"
eval (Succ n) = eval n >>= \case
  VNat n' -> pure . VNat $ n' + 1
  _ -> lift $ throwE "TypeError"
eval (Pred (Lit Zero)) = pure $ VNat 0
eval (Pred n) = eval n >>= \case
  VNat n' -> pure . VNat $ n' - 1
  _ -> lift $ throwE "TypeError"
eval (IsZero (Lit Zero)) = pure $ VBool True
eval (IsZero n@(Succ _))   = eval n >>= \case
  VNat _ -> pure $ VBool True
  _ -> lift $ throwE "TypeError"
eval (IsZero (Pred n)) = eval n >>= \case
  VNat n -> pure . VBool $ n == 0 || n == 1
  _ -> lift $ throwE "TypeError"
eval (Decl n exp) = eval exp >>= \v -> do
  env <- get
  put $ MA.insert n v env
  pure VDecl
eval (Var name) = do
  env <- get
  case MA.lookup name env of
    Just v  -> pure v
    Nothing -> lift $ throwE $ name <> " is undifined variable. "

runEval :: Env -> Eval a -> IO (Either String a)
runEval env ev = runExceptT (runStateT ev env) >>= \case
    Right (v, _) -> pure $ Right v
    Left e -> pure $ Left e

evalProgram' :: [Exp] -> Eval Value
evalProgram' []    = fail "There is no expression. "
evalProgram' [exp] = eval exp
evalProgram' exps  = last <$> forM exps eval

evalProgram :: [Exp] -> IO (Either String Value)
evalProgram exps = runEval MA.empty (evalProgram' exps)
