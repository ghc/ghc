--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockLex ( 
	Token(..), 
	tokenise 
 ) where

import IOExts --tmp
import Char

special = "\'\"/[]<>"

data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokSpecial Char
  | TokString String
  deriving Show

-- simple finite-state machine for tokenising the doc string

tokenise :: String -> [Token]
tokenise "" = []
tokenise str = case str of
  c:cs | c `elem` special -> TokSpecial c : tokenise cs
  '\n':cs -> tokenise_newline cs
  _other  -> tokenise_string "" str

tokenise_newline cs =
 case dropWhile nonNewlineSpace cs of
   '\n':cs -> TokPara : tokenise_para cs -- paragraph break
   _other -> tokenise_string "\n" cs

tokenise_para cs =
  case dropWhile nonNewlineSpace cs of   
	-- bullet:  '*'
   '*':cs  -> TokBullet  : tokenise cs
	-- bullet: '-'
   '-':cs  -> TokBullet  : tokenise cs
	-- enumerated item: '1.'
   str | (ds,'.':cs) <- span isDigit str, not (null ds)
		-> TokNumber : tokenise cs
	-- enumerated item: '(1)'
   '(':cs | (ds,')':cs') <- span isDigit cs, not (null ds)
		-> TokNumber : tokenise cs'
   other -> tokenise cs

nonNewlineSpace c = isSpace c && c /= '\n'

tokenise_string str cs = 
  case cs of
    [] -> [TokString (reverse str)]
    '\\':c:cs -> tokenise_string (c:str) cs
    '\n':cs   -> tokenise_string_newline str cs
    c:cs | c `elem` special -> TokString (reverse str) : tokenise (c:cs)
         | otherwise        -> tokenise_string (c:str) cs

tokenise_string_newline str cs =
  case dropWhile nonNewlineSpace cs  of
   '\n':cs -> TokString (reverse str) : TokPara : tokenise_para cs
   _other -> tokenise_string ('\n':str) cs  -- don't throw away whitespace
      
