module Eval(evalProgram) where

import           AST
import           Eval.Core

import qualified Data.Map  as MA

evalProgram :: [Exp] -> IO (Either String Value)
evalProgram exps = runEval MA.empty (evalProgram' exps)
