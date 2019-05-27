module Eval.Value(LocVal, Value(..), Eval, Env, toVal) where

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

type LocVal = Located Value

toVal :: LocVal -> Value
toVal (Located _ v) = v

instance Show Value where
  show (VBool b)   = show b
  show (VInt n)    = show n
  show VLambda{}   = "<<function>>"
  show VApp{}      = "<<application>>"
  show (VVar name) = "variable \"" <> name <> "\""
  show VDecl       = "<<declaration>>"


type Env = MA.Map String Value

type Eval a = RWST Env () Env (ExceptT String IO) a
