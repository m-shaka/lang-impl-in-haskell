module AST where

import           Lexer

type Name = String

type Position = (Int, Int)

data VarPos = Pos Position | Dummy deriving(Show, Eq)

type VarInfo = (Position, Name)

data Located a = Located Position a deriving (Show, Eq)

type Exp = Located Exp'

data Exp' =
  Var Name
  | Lit Lit
  | IfExp Exp Exp Exp
  | BinOp BinOp Exp Exp
  | Lambda Name Exp
  | Application Exp Exp
  deriving (Show, Eq)

data Lit =
  LBool Bool
  | LInt Int
  deriving (Show, Eq)

data BinOp = Plus | Minus | Multi | Div deriving(Show, Eq)

data Statement=
  Decl VarPos Name Exp
  | Exp Exp
  deriving (Show, Eq)

type Program = [Statement]

mkBool :: Bool -> Exp'
mkBool = Lit . LBool

mkInt :: Int -> Exp'
mkInt = Lit . LInt

mkLambda :: [VarInfo] -> Exp -> Exp
mkLambda varInfo exp = foldr mk exp varInfo
  where mk (pos, name) exp' = Located pos $ Lambda name exp'

mkApp :: Exp -> [Exp] -> Exp
mkApp = foldr mk
  where mk arg f' = Located (loc f') $ Application f' arg

mkExp :: AlexPosn -> Exp' -> Exp
mkExp pos = Located $ mkPos pos

mkDecl :: (AlexPosn, Name) -> Exp -> Statement
mkDecl posname =
  let (pos, name) = mkVarInfo posname in
  Decl (Pos pos) name

mkPos :: AlexPosn -> Position
mkPos (AlexPn _ line column) = (line, column)

mkVarInfo :: (AlexPosn, Name) -> VarInfo
mkVarInfo (pos, name) = (mkPos pos, name)

prettyPos :: Position -> String
prettyPos (line, col) = "at line " <> show line <> ", column " <> show col

loc :: Exp -> Position
loc (Located pos _) = pos

