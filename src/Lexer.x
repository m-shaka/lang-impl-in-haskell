{
module Lexer where
}

%wrapper "monadUserState"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-

$white+ ;
<0> [\;\n] { mkToken LxSep }
<0> \= { mkToken LxEq }
<0> \( { mkToken LxLParen }
<0> \) { mkToken LxRParen }
<0> \\ { mkToken LxBSlash }
<0> \-\> { mkToken LxArrow }
<0> "+" { mkToken LxPlus }
<0> "-" { mkToken LxMinus }
<0> "*" { mkToken LxMulti }
<0> "/" { mkToken LxDiv }
<0> if { mkToken LxIf }
<0> then { mkToken LxThen }
<0> else { mkToken LxElse }
<0> True { mkToken LxTrue }
<0> False { mkToken LxFalse }
<0> $digit+ { mkToken LxInt }
<0> $alpha+ { mkToken LxName }

{
mkToken lx (pos, _, _, str) len =
  let t = take len str in
  case lx of
    LxSep -> pure $ TkSep pos
    LxEq -> pure $ TkEq pos
    LxLParen -> pure $ TkLParen pos
    LxRParen -> pure $ TkRParen pos
    LxBSlash -> pure $ TkBSlash pos
    LxArrow -> pure $ TkArrow pos
    LxPlus -> pure $ TkBinOp $ (pos, TkPlus)
    LxMinus -> pure $ TkBinOp $ (pos, TkMinus)
    LxMulti -> pure $ TkBinOp $ (pos, TkMulti)
    LxDiv -> pure $ TkBinOp $ (pos, TkDiv)
    LxIf -> pure $ TkIf pos
    LxThen -> pure $ TkThen pos
    LxElse -> pure $ TkElse pos
    LxTrue -> pure $ TkTrue pos
    LxFalse -> pure $ TkFalse pos
    LxInt -> pure $ TkInt (pos, read t)
    LxName -> pure $ TkName (pos, t)

data Lexeme
  = LxSep
  | LxEq
  | LxLParen
  | LxRParen
  | LxBSlash
  | LxArrow
  | LxPlus
  | LxMinus
  | LxMulti
  | LxDiv
  | LxIf
  | LxThen
  | LxElse
  | LxTrue
  | LxFalse
  | LxInt
  | LxName
  deriving (Eq, Show)

data TkBinOp = TkPlus | TkMinus | TkMulti | TkDiv deriving(Eq, Show)

data Token =
  TkSep AlexPosn
  | TkEq AlexPosn
  | TkLParen AlexPosn
  | TkRParen AlexPosn
  | TkBSlash AlexPosn
  | TkArrow AlexPosn
  | TkBinOp (AlexPosn, TkBinOp)
  | TkIf AlexPosn
  | TkThen AlexPosn
  | TkElse AlexPosn
  | TkTrue AlexPosn
  | TkFalse AlexPosn
  | TkInt (AlexPosn, Int)
  | TkName (AlexPosn, String)
  | TkEof
  deriving (Show)

alexEOF :: Alex Token
alexEOF = pure TkEof

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
}
