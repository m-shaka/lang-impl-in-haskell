module AST where

type Exp = Exp'

data Exp' =
  TrueExp
  | FalseExp
  | IfExp Exp Exp Exp
  deriving (Show)
