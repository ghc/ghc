--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockLex ( 
	Token(..), 
	tokenise 
 ) where

import Char

special = '`' : '\'' : '\"' : '@' : []

data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokSpecial Char
  | TokString String
  | TokEmph String
  | TokURL String
  | TokBirdTrack
  deriving Show

-- simple finite-state machine for tokenising the doc string

-- ----------------------------------------------------------------------------
-- At the beginning of a paragraph, we throw away initial whitespace

tokenise :: String -> [Token]
tokenise "" = []
tokenise str = case str of
  '<':cs  -> tokenise_url cs
  '\n':cs -> tokenise_newline tokenise cs
  '/':cs  -> tokenise_emph tokenise cs
  c:cs | c `elem` special -> TokSpecial c : tokenise cs
  _other  -> tokenise_string "" str

tokenise_newline next cs =
 case dropWhile nonNewlineSpace cs of
   '\n':cs -> TokPara : tokenise_para cs -- paragraph break
   '>':cs  -> TokBirdTrack : next cs -- bird track
   _other  -> tokenise_string "" cs

tokenise_emph next cs = 
 case break newlineSlash cs of
   (bef, aft@('\n':cs)) -> TokString ('/':bef) : next aft -- paragraph break
   (bef, '/':cs)        -> TokEmph bef : next cs
   _other               -> tokenise_string "" cs

tokenise_para cs =
  case dropWhile nonNewlineSpace cs of   
	-- bullet:  '*'
   '*':cs  -> TokBullet  : tokenise cs
	-- bullet: '-'
   '-':cs  -> TokBullet  : tokenise cs
	-- enumerated item: '1.'
   '>':cs  -> TokBirdTrack : tokenise cs
	-- bird track
   str | (ds,'.':cs) <- span isDigit str, not (null ds)
		-> TokNumber : tokenise cs
	-- enumerated item: '(1)'
   '(':cs | (ds,')':cs') <- span isDigit cs, not (null ds)
		-> TokNumber : tokenise cs'
   other -> tokenise cs

nonNewlineSpace c = isSpace c && c /= '\n'

newlineSlash c = c == '\n' || c == '/'

-- ----------------------------------------------------------------------------
-- Within a paragraph, we don't throw away any whitespace (except before a
-- birdtrack, and before a paragraph break).

tokenise1 :: String -> [Token]
tokenise1 "" = []
tokenise1 str = case str of
  '<':cs  -> tokenise_url cs
  '\n':cs -> tokenise_newline1 cs
  '/':cs  -> tokenise_emph tokenise1 cs
  c:cs | c `elem` special -> TokSpecial c : tokenise1 cs
  _other  -> tokenise_string "" str

tokenise_newline1 cs =
 case dropWhile nonNewlineSpace cs of
   '\n':cs -> TokPara : tokenise_para cs -- paragraph break
   '>':cs  -> TokString "\n" : TokBirdTrack : tokenise1 cs -- bird track
   _other  -> tokenise_string "\n" cs

tokenise_url cs =
  let (url,rest) = break (=='>') cs in
  TokURL url : case rest of
		 '>':rest -> tokenise1 rest
		 _ -> tokenise1 rest

-- ----------------------------------------------------------------------------
-- Within a string, we don't throw away any whitespace

tokenise_string str cs = 
  case cs of
    [] -> [TokString (reverse str)]
    '\\':c:cs -> tokenise_string (c:str) cs
    '\n':cs   -> tokenise_string_newline str cs
    '<':cs    -> TokString (reverse str) : tokenise_url cs
    '/':cs    -> TokString (reverse str) : tokenise_emph (tokenise_string "") cs
    c:cs | c `elem` special -> TokString (reverse str) : tokenise1 (c:cs)
         | otherwise 	    -> tokenise_string (c:str) cs

tokenise_string_newline str cs =
  case dropWhile nonNewlineSpace cs  of
   '\n':cs -> TokString (reverse str) : TokPara : tokenise_para cs
   '>':cs  -> TokString (reverse ('\n':str)) : TokBirdTrack : tokenise1 cs
		 -- bird track
   _other  -> tokenise_string ('\n':str) cs  -- don't throw away whitespace
