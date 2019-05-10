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

{
data Token =
  TkIf
  | TkThen
  | TkElse
  | TkTrue
  | TkFalse
  | TkEof
  deriving (Show)

alexEOF :: Alex Token
alexEOF = pure TkEof

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
}
