{-# LANGUAGE LambdaCase #-}

module Eval.Core where

import           AST
import           Eval.Combinators
import           Eval.Primitives
import           Eval.Value

import           Control.Applicative        (liftA2)
import           Control.Monad              (forM, (>=>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.RWS
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import qualified Data.Map                   as MA
import           Data.Maybe                 (fromJust)
import           GHC.Natural                (Natural)


throwE' :: Position -> String -> ExceptT String IO a
throwE' pos detail =
  throwE $ "error occurs " <> prettyPos pos <> "\n\t" <> detail

compile :: Exp -> Eval LocVal
compile exp@(Located pos exp') = compile' exp'
  where
    compile' (Lit (LBool b)) = pure $ Located pos (VBool b)
    compile' (Lit (LInt i)) = pure $ Located pos (VInt i)
    compile' (IfExp cond x y) = do
      cond' <- toVal <$> compile cond
      x' <- toVal <$> compile x
      y' <- toVal <$> compile y
      pure $ Located pos $ foldl VApp if_ [cond', x', y']
    compile' (Var name) = pure $ Located pos (VVar name)
    compile' (Lambda name exp') = abstract name <$> compile exp'
    compile' (Application exp1 exp2) = do
      val1 <- toVal <$> compile exp1
      val2 <- toVal <$> compile exp2
      pure $ Located pos $ VApp val1 val2
    compile' (BinOp op exp1 exp2) = compileBinOp op exp1 exp2 pos

    compileBinOp :: BinOp -> Exp -> Exp -> Position -> Eval LocVal
    compileBinOp op exp1 exp2 pos = do
      val1 <- toVal <$> compile exp1
      val2 <- toVal <$> compile exp2
      let actualOp = case op of
            Plus  -> (+)
            Minus -> (-)
            Multi -> (*)
            Div   -> div
      pure $ Located pos $ foldl VApp (arith actualOp) [val1, val2]

link :: LocVal -> Eval Value
link (Located pos v) = linkVal v
     where
       linkVal (VApp fun arg) = do
         f' <- linkVal fun
         arg' <- linkVal arg
         f' ! arg'
       linkVal (VVar n)       = MA.lookup n <$> get >>= \case
         Just v -> pure v
         Nothing -> lift $ throwE' pos $ "UndefinedVariableError: " <> n
       linkVal e              = pure e

abstract :: Name -> LocVal -> LocVal
abstract x (Located pos v) = Located pos $ abstractVal x v
  where
    abstractVal x (VApp fun arg) =  combS (abstractVal x fun) (abstractVal x arg)
    abstractVal x (VVar n)       | x == n = combI
    abstractVal _ k              = combK k

combS :: Value -> Value -> Value
combS f = VApp (VApp combS_ f)

combK :: Value -> Value
combK = VApp combK_

combI :: Value
combI = combI_

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

