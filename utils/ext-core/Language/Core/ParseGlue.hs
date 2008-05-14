module Language.Core.ParseGlue where

import Encoding

import Data.List

data ParseResult a = OkP a | FailP String
type P a = String -> Int -> ParseResult a

instance Show a => Show (ParseResult a)
  where show (OkP r) = show r
        show (FailP s) = s

thenP :: P a -> (a -> P b) -> P b
m `thenP`  k = \ s l -> 
  case m s l of 
    OkP a -> k a s l
    FailP s -> FailP s

returnP :: a -> P a
returnP m _ _ = OkP m

failP :: String -> P a
failP s s' _ = FailP (s ++ ":" ++ s')

data Token =
   TKmodule 
 | TKdata 
 | TKnewtype 
 | TKforall 
 | TKrec 
 | TKlet 
 | TKin 
 | TKcase 
 | TKof 
 | TKcast
 | TKnote 
 | TKexternal
 | TKwild
 | TKoparen 
 | TKcparen 
 | TKobrace
 | TKcbrace
 | TKhash
 | TKeq 
 | TKcoloncolon 
 | TKstar 
 | TKrarrow 
 | TKlambda
 | TKbiglambda
 | TKat 
 | TKdot
 | TKcolon
 | TKquestion
 | TKsemicolon
 | TKname String 
 | TKcname String
 | TKinteger Integer 
 | TKrational Rational
 | TKstring String 
 | TKchar Char 
 | TKEOF
