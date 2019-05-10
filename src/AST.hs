module AST where

type Exp = Exp'

data Exp' =
  Lit Lit
  | IfExp Exp Exp Exp
  deriving (Show)

data Lit =
  LBool Bool
  deriving (Show)

mkBool :: Bool -> Exp
mkBool = Lit . LBool


eval :: Exp -> Bool
eval (Lit (LBool b))  = b
eval (IfExp cond x y) = eval $ if eval cond then x else y
