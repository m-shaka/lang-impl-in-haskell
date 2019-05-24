{-# LANGUAGE LambdaCase #-}

module Eval.Core where

import           AST
import           Control.Applicative        (liftA2)
import           Control.Monad              (forM, (>=>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.RWS
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import qualified Data.Map                   as MA
import           Data.Maybe                 (fromJust)
import           GHC.Natural                (Natural)

data Value =
  VVar Name
  | VBool Bool
  | VInt Int
  | VLambda (Value -> Value)
  | VApp Value Value
  | VDecl

instance Show Value where
  show (VBool b) = show b
  show (VInt n)  = show n
  show VLambda{} = "<<function>>"
  show VApp{}    = "<<application>>"
  show _         = ""

type Env = MA.Map String Value

type Eval a = RWST Env () Env (ExceptT String IO) a

throwE' :: Position -> String -> ExceptT String IO a
throwE' pos detail =
  throwE $ "error occurs " <> prettyPos pos <> "\n\t" <> detail


compile :: Exp -> Eval Value
compile (Located pos exp) = compile' exp
  where
    compile' (Lit (LBool b)) = pure $ VBool b
    compile' (Lit (LInt i)) = pure $ VInt i
    compile' (IfExp cond x y) = compile cond >>= \case
      VBool b -> compile $ if b then x else y
      v -> lift $ throwE' (loc cond) $ "TypeError: " <> show v <> " is not boolean. "
    compile' (Succ n) = compile n >>= \case
      VInt n' -> pure . VInt $ n' + 1
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    compile' (Pred (Located _ (Lit (LInt 0)))) = pure $ VInt 0
    compile' (Pred n) = compile n >>= \case
      VInt n' -> pure . VInt $ n' - 1
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    compile' (IsZero n) = compile n >>= \case
      VInt n' -> pure $ VBool $ n' == 0
      v -> lift $ throwE' (loc n) $ "TypeError: " <> show v <> " is not natural number. "
    compile' (Var name) = pure $ VVar name
    compile' (Lambda name exp') = abstract name <$> compile exp'
    compile' (Application exp1 exp2) = liftA2 VApp (compile exp1) (compile exp2)
    compile' (BinOp op exp1 exp2) = compileBinOp op exp1 exp2

compileBinOp :: BinOp -> Exp -> Exp -> Eval Value
compileBinOp op exp1 exp2 = do
  v1 <- compile exp1
  v2 <- compile exp2
  let actualOperator = case op of
        Plus  -> (+)
        Minus -> (-)
        Multi -> (*)
        Div   -> div -- temporary return Int
  case (v1, v2) of
    (VInt n1, VInt n2) -> pure . VInt $ actualOperator n1 n2
    (VInt _, badValue) -> lift $ throwE' (loc exp2) $ "TypeError: " <> show badValue <> " is not natural number. "
    (badValue, _) -> lift $ throwE' (loc exp1) $ "TypeError: " <> show badValue <> " is not natural number. "

infixl 0 !
(!) :: Value -> Value -> Value
VLambda f ! x = f x

link :: Value -> Eval Value
link (VApp fun arg) = liftA2 (!) (link fun) (link arg)
link (VVar n)       = fromJust . MA.lookup n <$> get
link e              = pure e

abstract :: Name -> Value -> Value
abstract x (VApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (VVar n)       | x == n = combI
abstract _ k              = combK k

combS :: Value -> Value -> Value
combS f = VApp (VApp (VVar "$S") f)

combK :: Value -> Value
combK = VApp (VVar "$K")

combI :: Value
combI = VVar "$I"

runEval :: Env -> Eval a -> IO (Either String a)
runEval env ev = runExceptT (runRWST ev env env) >>= \case
    Right (v, _, _) -> pure $ Right v
    Left e -> pure $ Left e

evalStatement (Decl pos name exp) = do
  v <- compile exp >>= link
  get >>= \env -> put $ MA.insert name v env
  pure VDecl
evalStatement (Exp exp) = compile exp >>= link

evalProgram' :: Program -> Eval Value
evalProgram' p = last <$> forM p evalStatement

