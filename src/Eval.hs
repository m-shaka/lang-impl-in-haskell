module Eval(evalProgram) where

import           AST
import           Eval.Core
import           Eval.Primitives

import qualified Data.Map        as MA

evalProgram :: Program -> IO (Either String Value)
evalProgram p = runEval primitives (evalProgram' p)
