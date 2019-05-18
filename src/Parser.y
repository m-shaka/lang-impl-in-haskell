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
INT { TkInt $$ }
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
'+' { TkBinOp ($$, TkPlus) }
'-' { TkBinOp ($$, TkMinus) }
'*' { TkBinOp ($$, TkMulti) }
'/' { TkBinOp ($$, TkDiv) }

%right precIf
%left '+' '-'
%left '*' '/'
%right lambdaPrec
%left appPrec
%nonassoc expPrec

%%
program :: { Program }
program
  : exp %prec expPrec { [Exp $1] }
  | decl { [$1] }
  | exp ';' program { Exp $1 : $3 }
  | decl ';' program { $1 : $3 }

decl :: { Statement }
decl
  : VARID '=' exp { mkDecl $1 $3 }
  | VARID funcDefArgs '=' exp { mkDecl $1 $ mkLambda $2 $4 }

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
  | IF exp THEN exp ELSE exp %prec precIf { mkExp $1 $ IfExp $2 $4 $6 }
  | SUCC factor { mkExp $1 $ Succ $2 }
  | PRED factor { mkExp $1 $ Pred $2 }
  | ISZERO factor { mkExp $1 $ IsZero $2 }
  | lambda { $1 }
  | factor actualArgs %prec appPrec { Located (loc $1) (Application $1 $2) }
  | binOp { $1 }

factor :: { Exp }
factor
  : VARID { mkExp (fst $1) $ Var (snd $1) }
  | TRUE { mkExp $1 $ mkBool True }
  | FALSE { mkExp $1 $ mkBool False }
  | INT { mkExp (fst $1) $ mkInt (snd $1)  }
  | '(' exp ')' { $2 }

lambda :: { Exp }
lambda
  : '\\' '->' exp %prec lambdaPrec { mkLambda [mkVarInfo $1] $3 }
  | '\\' funcDefArgs '->' exp %prec lambdaPrec { mkLambda (mkVarInfo $1 : $2) $4 }

binOp :: { Exp }
binOp
  : exp '+' exp { Located (loc $1) $ BinOp Plus $1 $3 }
  | exp '-' exp { Located (loc $1) $ BinOp Minus $1 $3 }
  | exp '*' exp { Located (loc $1) $ BinOp Multi $1 $3 }
  | exp '/' exp { Located (loc $1) $ BinOp Div $1 $3 }


{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
