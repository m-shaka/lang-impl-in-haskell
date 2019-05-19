module Utils(prettyStatement) where

import           AST
import           Data.List

genTab :: Int -> String
genTab count = concat $ replicate count "\t"

prettyExp :: Exp -> Int -> String
prettyExp (Located _ (IfExp cond x y)) tabCount =
  let
      root = genTab tabCount ++ "IfExp"
      children = fmap (`prettyExp` (tabCount + 1)) [cond, x, y]
  in intercalate "\n" (root:children)
prettyExp (Located _ (Succ e)) tabCount =
  genTab tabCount ++ "Succ" ++ "\n" ++ prettyExp e (tabCount + 1)
prettyExp (Located _ (Pred e)) tabCount =
  genTab tabCount ++ "Pred" ++ "\n" ++ prettyExp e (tabCount + 1)
prettyExp (Located _ (IsZero e)) tabCount =
  genTab tabCount ++ "isZero" ++ "\n" ++ prettyExp e (tabCount + 1)
prettyExp (Located _ (BinOp op x y)) tabCount =
  let
    root = genTab tabCount ++ "BinOp " ++ show op
    children = fmap (`prettyExp` (tabCount + 1)) [x, y]
  in intercalate "\n" (root:children)
prettyExp (Located _ (Lambda n e)) tabCount =
  genTab tabCount ++ "Lambda " ++ n ++ "\n" ++ prettyExp e (tabCount + 1)
prettyExp (Located _ (Application f exps)) tabCount =
  let
    root = genTab tabCount ++ "Application"
    func = prettyExp f (tabCount + 1)
    args = intercalate ",\n" $ fmap (`prettyExp` (tabCount + 2)) exps
  in intercalate "\n" [root, func, args]
prettyExp (Located _ e) tabCount = genTab tabCount ++ show e

prettyStatement :: Statement -> String
prettyStatement (Decl _ name e) = "Decl " ++ name ++ "\n" ++ prettyExp e 1
prettyStatement (Exp e)         = prettyExp e 0
