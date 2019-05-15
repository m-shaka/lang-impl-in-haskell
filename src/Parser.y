{
module Parser(parse) where

import Lexer
import AST
}

%name parser
%error { parseError }
%lexer { lexwrap } { TkEof }
%monad { Alex }
%tokentype { Token }

%token
IF { TkIf $$ }
THEN { TkThen $$ }
ELSE { TkElse $$ }
TRUE { TkTrue $$ }
FALSE { TkFalse $$ }
ZERO { TkZero $$ }
SUCC { TkSucc $$ }
PRED { TkPred $$ }
ISZERO { TkIsZero $$ }
';' { TkSep $$ }
'=' { TkEq $$ }
VARID { TkName $$ }
'(' { TkLParen $$ }
')' { TkRParen $$ }
'\\' { TkBSlash $$ }
'->' { TkArrow $$ }

%right LAMBDA
%left APP

%%
program :: { [Exp] }
program
  : exp { [$1] }
  | decl { [$1] }
  | exp ';' program { $1 : $3 }
  | decl ';' program { $1 : $3 }

decl :: { Exp }
decl
  : VARID '=' exp { mkExp (fst $1) $ Decl (snd $1) $3 }
  | VARID funcDefArgs '=' exp { mkExp (fst $1) $ Decl (snd $1) (mkLambda $2 $4) }

funcDefArgs :: { [VarInfo] }
funcDefArgs
  : VARID { [mkVarInfo $1] }
  | VARID funcDefArgs { mkVarInfo $1 : $2 }

actualArgs
  : factor { [$1] }
  | factor actualArgs { $1 : $2 }

exp :: { Exp }
exp
  : factor { $1 }
  | IF exp THEN exp ELSE exp { mkExp $1 $ IfExp $2 $4 $6 }
  | SUCC factor { mkExp $1 $ Succ $2 }
  | PRED factor { mkExp $1 $ Pred $2 }
  | ISZERO factor { mkExp $1 $ IsZero $2 }
  | lambda { $1 }
  | factor actualArgs %prec APP { Located (loc $1) (Application $1 $2) }

factor :: { Exp }
factor
  : VARID { mkExp (fst $1) $ Var (snd $1) }
  | TRUE { mkExp $1 $ mkBool True }
  | FALSE { mkExp $1 $ mkBool False }
  | ZERO { mkExp $1 zero }
  | '(' exp ')' { $2 }

lambda :: { Exp }
lambda
  : '\\' '->' exp %prec LAMBDA { mkLambda [mkVarInfo $1] $3 }
  | '\\' funcDefArgs '->' exp %prec LAMBDA { mkLambda (mkVarInfo $1 : $2) $4 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
