{
module Lexer where
}

%wrapper "monadUserState"

$alpha = [a-zA-Z]

tokens :-

$white ;
[\;\n] { \_ _ -> pure TkSep }
\= { \_ _ -> pure TkDecl }
if { \_ _ -> pure TkIf }
then { \_ _ -> pure TkThen }
else { \_ _ -> pure TkElse }
True { \_ _ -> pure TkTrue }
False { \_ _ -> pure TkFalse }
0 { \_ _ -> pure TkZero }
succ { \_ _ -> pure TkSucc }
pred { \_ _ -> pure TkPred }
isZero { \_ _ -> pure TkIsZero }
$alpha+ { \(pos, _, _, str) len -> let t = take len str in pure $ TkName t }

{
data Token =
  TkSep
  | TkDecl
  | TkIf
  | TkThen
  | TkElse
  | TkTrue
  | TkFalse
  | TkZero
  | TkSucc
  | TkPred
  | TkIsZero
  | TkName String
  | TkEof
  deriving (Show)

alexEOF :: Alex Token
alexEOF = pure TkEof

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
}
