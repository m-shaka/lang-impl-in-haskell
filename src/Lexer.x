{
module Lexer where
}

%wrapper "monadUserState"

tokens :-

$white ;
if { \_ _ -> pure TkIf }
then { \_ _ -> pure TkThen }
else { \_ _ -> pure TkElse }
True { \_ _ -> pure TkTrue }
False { \_ _ -> pure TkFalse }
0 { \_ _ -> pure TkZero }
succ { \_ _ -> pure TkSucc }
pred { \_ _ -> pure TkPred }
isZero { \_ _ -> pure TkIsZero }

{
data Token =
  TkIf
  | TkThen
  | TkElse
  | TkTrue
  | TkFalse
  | TkZero
  | TkSucc
  | TkPred
  | TkIsZero
  | TkEof
  deriving (Show)

alexEOF :: Alex Token
alexEOF = pure TkEof

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
}
