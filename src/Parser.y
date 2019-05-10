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

%%
exp :: { Exp }
exp
  : TRUE { mkBool True }
  | FALSE { mkBool False }
  | IF exp THEN exp ELSE exp { IfExp $2 $4 $6 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

parseError :: Token -> Alex a
parseError t = alexError $ "parseError: " ++ show t

parse s = runAlex s parser
}
