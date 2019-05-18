module Eval(evalProgram) where

import           AST
import           Eval.Core

import qualified Data.Map  as MA

evalProgram :: Program -> IO (Either String Value)
evalProgram p = runEval MA.empty (evalProgram' p)
