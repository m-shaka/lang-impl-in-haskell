module AST where

type Name = String

type Exp = Exp'

data Exp' =
  Var Name
  | Lit Lit
  | IfExp Exp Exp Exp
  | Succ Exp
  | Pred Exp
  | IsZero Exp
  | Decl Name Exp
  deriving (Show)

data Lit =
  LBool Bool
  | Zero
  deriving (Show)

mkBool :: Bool -> Exp
mkBool = Lit . LBool

zero :: Exp
zero = Lit Zero
