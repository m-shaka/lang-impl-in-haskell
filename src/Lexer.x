{
module Lexer where
}

%wrapper "monadUserState"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-

$white+ ;
[\;\n] { mkToken LxSep }
\= { mkToken LxEq }
\( { mkToken LxLParen }
\) { mkToken LxRParen }
\\ $white* $alpha+ { mkToken LxBSlash }
\-\> { mkToken LxArrow }
if { mkToken LxIf }
then { mkToken LxThen }
else { mkToken LxElse }
True { mkToken LxTrue }
False { mkToken LxFalse }
$digit+ { mkToken LxInt }
succ { mkToken LxSucc }
pred { mkToken LxPred }
isZero { mkToken LxIsZero }
$alpha+ { mkToken LxName }

{
mkToken lx (pos, _, _, str) len =
  let t = take len str in
  case lx of
    LxSep -> pure $ TkSep pos
    LxEq -> pure $ TkEq pos
    LxLParen -> pure $ TkLParen pos
    LxRParen -> pure $ TkRParen pos
    LxBSlash -> pure $ TkBSlash (pos, tail . filter (/=' ') $ t)
    LxArrow -> pure $ TkArrow pos
    LxIf -> pure $ TkIf pos
    LxThen -> pure $ TkThen pos
    LxElse -> pure $ TkElse pos
    LxTrue -> pure $ TkTrue pos
    LxFalse -> pure $ TkFalse pos
    LxInt -> pure $ TkInt (pos, read t)
    LxSucc -> pure $ TkSucc pos
    LxPred -> pure $ TkPred pos
    LxIsZero -> pure $ TkIsZero pos
    LxName -> pure $ TkName (pos, t)

data Lexeme
  = LxSep
  | LxEq
  | LxLParen
  | LxRParen
  | LxBSlash
  | LxArrow
  | LxIf
  | LxThen
  | LxElse
  | LxTrue
  | LxFalse
  | LxInt
  | LxSucc
  | LxPred
  | LxIsZero
  | LxName
  deriving (Eq, Show)

data Token =
  TkSep AlexPosn
  | TkEq AlexPosn
  | TkLParen AlexPosn
  | TkRParen AlexPosn
  | TkBSlash (AlexPosn, String)
  | TkArrow AlexPosn
  | TkIf AlexPosn
  | TkThen AlexPosn
  | TkElse AlexPosn
  | TkTrue AlexPosn
  | TkFalse AlexPosn
  | TkInt (AlexPosn, Int)
  | TkSucc AlexPosn
  | TkPred AlexPosn
  | TkIsZero AlexPosn
  | TkName (AlexPosn, String)
  | TkEof
  deriving (Show)

alexEOF :: Alex Token
alexEOF = pure TkEof

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
}
