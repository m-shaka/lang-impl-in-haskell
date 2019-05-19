{-# LANGUAGE LambdaCase #-}

module Eval.Core where

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
  | VInt Int
  | VLambda Env Name Exp
  | VDecl deriving (Eq)

instance Show Value where
  show (VBool b) = show b
  show (VInt n)  = show n
  show VLambda{} = "<<function>>"
  show _         = ""

type Env = MA.Map String Value

type Eval a = RWST Env () Env (ExceptT String IO) a

throwE' :: Position -> String -> ExceptT String IO a
throwE' pos detail =
  throwE $ "error occurs " <> prettyPos pos <> "\n\t" <> detail


eval :: Exp -> Eval Value
eval (Located pos exp) = eval' exp
  where
    eval' (Lit (LBool b)) = pure $ VBool b
    eval' (Lit (LInt i)) = pure $ VInt i
    eval' (IfExp cond x y) = eval cond >>= \case
      VBool b -> eval $ if b then x else y
      v -> lift $ throwE' (loc cond) $ "TypeError: " <> show v <> " is not boolean. "
    eval' (Succ n) = eval n >>= \case
      VInt n' -> pure . VInt $ n' + 1
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    eval' (Pred (Located _ (Lit (LInt 0)))) = pure $ VInt 0
    eval' (Pred n) = eval n >>= \case
      VInt n' -> pure . VInt $ n' - 1
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    eval' (IsZero n) = eval n >>= \case
      VInt n' -> pure $ VBool $ n' == 0
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    eval' (Var name) = do
      env <- get
      localEnv <- ask
      case MA.lookup name $ MA.union localEnv env of
        Just v  -> pure v
        Nothing -> lift $ throwE' pos $ "UndefinedVariableError: " <> name
    eval' (Lambda name exp') = do
      localEnv <- ask
      pure $ VLambda localEnv name exp'
    eval' (Application (Located _ (Lambda name abst)) [arg]) = do
      v <- eval arg
      local (MA.insert name v) $ eval abst
    eval' (Application (Located _ (Lambda name abst)) (fstArg:restArgs)) = do
      v <- eval fstArg
      local (MA.insert name v) $ eval' $ Application abst restArgs
    eval' (Application exp1@(Located pos exp1') exp2) = eval exp1 >>= \case
        VLambda localEnv name exp'-> local (`MA.union` localEnv) $ eval' $ Application (Located pos (Lambda name exp')) exp2
        badValue -> lift $ throwE' pos $ "ApplicationError: " <> show badValue <> " is not function. "
    eval' (BinOp op exp1 exp2) = evalBinOp op exp1 exp2

evalBinOp :: BinOp -> Exp -> Exp -> Eval Value
evalBinOp op exp1 exp2 = do
  v1 <- eval exp1
  v2 <- eval exp2
  let actualOperator = case op of
        Plus  -> (+)
        Minus -> (-)
        Multi -> (*)
        Div   -> div -- temporary return Int
  case (v1, v2) of
    (VInt n1, VInt n2) -> pure . VInt $ actualOperator n1 n2
    (VInt _, badValue) -> lift $ throwE' (loc exp2) $ "TypeError: " <> show badValue <> " is not natural number. "
    (badValue, _) -> lift $ throwE' (loc exp1) $ "TypeError: " <> show badValue <> " is not natural number. "


runEval :: Env -> Eval a -> IO (Either String a)
runEval env ev = runExceptT (runRWST ev env env) >>= \case
    Right (v, _, _) -> pure $ Right v
    Left e -> pure $ Left e

evalStatement (Decl pos name exp) = do
  v <- eval exp
  get >>= \env -> put $ MA.insert name v env
  pure VDecl
evalStatement (Exp exp) = eval exp

evalProgram' :: Program -> Eval Value
evalProgram' p = last <$> forM p evalStatement

