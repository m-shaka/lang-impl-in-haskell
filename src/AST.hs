module AST where

import           Lexer

type Name = String

type Position = (Int, Int)

type VarInfo = (Position, Name)

data Located a = Located Position a deriving (Show, Eq)

type Exp = Located Exp'

data Exp' =
  Var Name
  | Lit Lit
  | IfExp Exp Exp Exp
  | Succ Exp
  | Pred Exp
  | IsZero Exp
  | BinOp BinOp Exp Exp
  | Decl Name Exp
  | Lambda Name Exp
  | Application Exp [Exp]
  deriving (Show, Eq)

data Lit =
  LBool Bool
  | LInt Int
  deriving (Show, Eq)

data BinOp = Plus | Minus | Multi | Div deriving(Show, Eq)

mkBool :: Bool -> Exp'
mkBool = Lit . LBool

mkInt :: Int -> Exp'
mkInt = Lit . LInt

mkLambda :: [VarInfo] -> Exp -> Exp
mkLambda varInfo exp = foldr mk exp varInfo
  where mk (pos, name) exp' = Located pos $ Lambda name exp'

mkExp :: AlexPosn -> Exp' -> Exp
mkExp pos = Located $ mkPos pos

mkPos :: AlexPosn -> Position
mkPos (AlexPn _ line column) = (line, column)

mkVarInfo :: (AlexPosn, Name) -> VarInfo
mkVarInfo (pos, name) = (mkPos pos, name)

prettyPos :: Position -> String
prettyPos (line, col) = "at line " <> show line <> ", column " <> show col

loc :: Exp -> Position
loc (Located pos _) = pos
