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
prettyExp (Located _ (BinOp op x y)) tabCount =
  let
    root = genTab tabCount ++ "BinOp " ++ show op
    children = fmap (`prettyExp` (tabCount + 1)) [x, y]
  in intercalate "\n" (root:children)
prettyExp (Located _ (Lambda n e)) tabCount =
  genTab tabCount ++ "Lambda " ++ n ++ "\n" ++ prettyExp e (tabCount + 1)
prettyExp (Located _ (Application f exp)) tabCount =
  let
    root = genTab tabCount ++ "Application"
    func = prettyExp f (tabCount + 1)
    arg =  prettyExp exp (tabCount + 2)
  in intercalate "\n" [root, func, arg]
prettyExp (Located _ e) tabCount = genTab tabCount ++ show e

prettyStatement :: Statement -> String
prettyStatement (Decl _ name e) = "Decl " ++ name ++ "\n" ++ prettyExp e 1
prettyStatement (Exp e)         = prettyExp e 0
