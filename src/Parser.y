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
VAR { TkName $$ }

%%
program :: { [Exp] }
program
  : exp  { [$1] }
  | decl { [$1] }
  | decl ';' program { $1 : $3 }
  | exp ';' program { $1 : $3 }

decl :: { Exp }
decl
  : VAR '=' exp { mkExp (fst $1) $ Decl (snd $1) $3 }
  | VAR funcArgs '=' exp { mkExp (fst $1) $ Decl (snd $1) (mkLambda $2 $4) }

exp :: { Exp }
exp
  : TRUE { mkExp $1 $ mkBool True }
  | FALSE { mkExp $1 $ mkBool False }
  | IF exp THEN exp ELSE exp { mkExp $1 $ IfExp $2 $4 $6 }
  | ZERO { mkExp $1 zero }
  | SUCC exp { mkExp $1 $ Succ $2 }
  | PRED exp { mkExp $1 $ Pred $2 }
  | ISZERO exp { mkExp $1 $ IsZero $2 }
  | VAR { mkExp (fst $1) $ Var (snd $1) }

funcArgs :: { [VarInfo] }
funcArgs
  : VAR { [mkVarInfo $1] }
  | VAR funcArgs { mkVarInfo $1 : $2 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
