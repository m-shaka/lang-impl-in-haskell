module Eval.Value(Value(..), Eval, Env) where

import           AST

import           Control.Monad.RWS
import           Control.Monad.Trans.Except
import qualified Data.Map                   as MA

data Value =
  VVar Name
  | VBool Bool
  | VInt Int
  | VLambda (Value -> Eval Value)
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
