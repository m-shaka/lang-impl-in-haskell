module AST where

import           Lexer

type Name = String

type Position = (Int, Int)

data Located a = Located Position a deriving (Show)

type Exp = Located Exp'

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

mkBool :: Bool -> Exp'
mkBool = Lit . LBool

zero :: Exp'
zero = Lit Zero

mkExp :: AlexPosn -> Exp' -> Exp
mkExp (AlexPn _ line column) = Located (line, column)

prettyPos :: Position -> String
prettyPos (line, col) = show line <> ":" <> show col

loc :: Exp -> Position
loc (Located pos _) = pos
