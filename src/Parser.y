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
IF { TkIf }
THEN { TkThen }
ELSE { TkElse }
TRUE { TkTrue }
FALSE { TkFalse }
ZERO { TkZero }
SUCC { TkSucc }
PRED { TkPred }
ISZERO { TkIsZero }
';' { TkSep }
'=' { TkDecl }
VAR { TkName $$ }

%%
exps :: { [Exp] }
exps
: exp { [$1] }
| exp ';' exps { $1 : $3 }

exp :: { Exp }
exp
  : TRUE { mkBool True }
  | FALSE { mkBool False }
  | IF exp THEN exp ELSE exp { IfExp $2 $4 $6 }
  | ZERO { zero }
  | SUCC exp { Succ $2 }
  | PRED exp { Pred $2 }
  | ISZERO exp { IsZero $2 }
  | VAR '=' exp { Decl $1 $3 }
  | VAR { Var $1 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
