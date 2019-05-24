module Eval(evalProgram) where

import           AST
import           Eval.Core
import           Eval.Primitives
import           Eval.Value      (Value)

import qualified Data.Map        as MA

evalProgram :: Program -> IO (Either String Value)
evalProgram p = runEval primitives (evalProgram' p)
