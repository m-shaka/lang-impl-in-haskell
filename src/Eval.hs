{-# LANGUAGE LambdaCase #-}

module Eval (evalProgram) where

import           AST
import           Control.Monad              (forM, (>=>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.RWS
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import qualified Data.Map                   as MA
import           GHC.Natural                (Natural)

data Value =
  VBool Bool
  | VNat Natural
  | VDecl
  | VLambda Name Exp

instance Show Value where
  show (VBool b)     = show b
  show (VNat n)      = show n
  show VDecl         = "Declaration"
  show (VLambda _ _) = "function"


type Env = MA.Map String Value

type Eval a = RWST Env () Env (ExceptT String IO) a

throwE' :: Position -> String -> ExceptT String IO a
throwE' pos detail =
  throwE $ "error occurs " <> prettyPos pos <> "\n\t" <> detail


eval :: Exp -> Eval Value
eval (Located pos exp) = eval' exp
  where
    eval' (Lit (LBool b)) = pure $ VBool b
    eval' (Lit Zero) = pure $ VNat 0
    eval' (IfExp cond x y) = eval cond >>= \case
      VBool b -> eval $ if b then x else y
      v -> lift $ throwE' (loc cond) $ "TypeError: " <> show v <> " is not boolean. "
    eval' (Succ n) = eval n >>= \case
      VNat n' -> pure . VNat $ n' + 1
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    eval' (Pred (Located _ (Lit Zero))) = pure $ VNat 0
    eval' (Pred n) = eval n >>= \case
      VNat n' -> pure . VNat $ n' - 1
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    eval' (IsZero (Located _ (Lit Zero))) = pure $ VBool True
    eval' (IsZero n) = eval n >>= \case
      VNat n' -> pure $ VBool $ n' == 0
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    eval' (Decl n exp) = eval exp >>= \v -> do
      env <- get
      put $ MA.insert n v env
      pure VDecl
    eval' (Var name) = do
      env <- get
      localEnv <- ask
      case MA.lookup name $ MA.union localEnv env of
        Just v  -> pure v
        Nothing -> lift $ throwE' pos $ "UndefinedVariableError: " <> name
    eval' (Lambda name exp') = pure $ VLambda name exp'
    eval' (Application (Located _ (Lambda name abst)) exp2) = do
      v <- eval exp2
      local (MA.insert name v) $ eval abst
    eval' (Application exp1@(Located pos exp1'@(Var _)) exp2) = eval exp1 >>= \case
        VLambda name exp'-> eval' $ Application (Located pos (Lambda name exp')) exp2
        _ -> appError pos exp1'
    eval' (Application (Located pos exp1) _) = appError pos exp1

    appError pos exp = lift $ throwE' pos $ "ApplicationError: " <> show exp <> " is not function. "



runEval :: Env -> Eval a -> IO (Either String a)
runEval env ev = runExceptT (runRWST ev env env) >>= \case
    Right (v, _, _) -> pure $ Right v
    Left e -> pure $ Left e

evalProgram' :: [Exp] -> Eval Value
evalProgram' []    = fail "There is no expression. "
evalProgram' [exp] = eval exp
evalProgram' exps  = last <$> forM exps eval

evalProgram :: [Exp] -> IO (Either String Value)
evalProgram exps = runEval MA.empty (evalProgram' exps)

