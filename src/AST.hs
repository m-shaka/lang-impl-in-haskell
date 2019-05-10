module AST where

type Exp = Exp'

data Exp' =
  Lit Lit
  | IfExp Exp Exp Exp
  | Succ Exp
  | Pred Exp
  | IsZero Exp
  deriving (Show)

data Lit =
  LBool Bool
  | Zero
  deriving (Show)

mkBool :: Bool -> Exp
mkBool = Lit . LBool

zero :: Exp
zero = Lit Zero
