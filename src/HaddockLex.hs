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
import HsSyn
import HsLexer hiding (Token)
import HsParseMonad

data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokSpecial Char
  | TokIdent [HsQName]
  | TokString String
  | TokURL String
  | TokBirdTrack
  deriving Show

isSpecial c     = c `elem` ['\"', '@', '/']
isSingleQuote c = c `elem` ['\'', '`']
isIdent c       = isAlphaNum c || c == '_' || c == '.'

-- simple finite-state machine for tokenising the doc string

-- ----------------------------------------------------------------------------
-- At the beginning of a paragraph, we throw away initial whitespace

tokenise :: String -> [Token]
tokenise "" = []
tokenise str = case str of
  '<':cs  -> tokenise_url cs
  '\n':cs -> tokenise_newline cs
  c:cs 	| isSingleQuote c -> tokenise_identifier c cs
	| isSpecial c     -> TokSpecial c : tokenise1 cs
  _other  -> tokenise_string "" str

tokenise_newline cs =
 case dropWhile nonNewlineSpace cs of
   '\n':cs -> TokPara : tokenise_para cs -- paragraph break
   '>':cs  -> TokBirdTrack : tokenise_birdtrack cs -- bird track
   _other  -> tokenise_string "" cs

tokenise_para cs =
  case dropWhile nonNewlineSpace cs of   
	-- bullet:  '*'
   '*':cs  -> TokBullet  : tokenise cs
	-- bullet: '-'
   '-':cs  -> TokBullet  : tokenise cs
	-- enumerated item: '1.'
   '>':cs  -> TokBirdTrack : tokenise_birdtrack cs
	-- bird track
   str | (ds,'.':cs) <- span isDigit str, not (null ds)
		-> TokNumber : tokenise cs
	-- enumerated item: '(1)'
   '(':cs | (ds,')':cs') <- span isDigit cs, not (null ds)
		-> TokNumber : tokenise cs'
   other -> tokenise cs

nonNewlineSpace c = isSpace c && c /= '\n'

-- ----------------------------------------------------------------------------
-- Within a paragraph, we don't throw away any whitespace (except before a
-- birdtrack, and before a paragraph break).

tokenise1 :: String -> [Token]
tokenise1 str = tokenise_string "" str

-- found a single quote, check whether we have an identifier...
tokenise_identifier q cs =
  let (ident,rest) = break (not.isIdent) cs in
  case (rest, strToHsQNames ident) of
    (c:cs, Just names) | isSingleQuote c -> TokIdent names : tokenise1 cs
    _other -> tokenise_string [q] cs

tokenise_url cs =
  let (url,rest) = break (=='>') cs in
  TokURL url : case rest of
		 '>':rest -> tokenise1 rest
		 _ -> tokenise1 rest

tokenise_string str cs = 
  case cs of
    []        -> tokString str []
    '\\':c:cs -> tokenise_string (c:str) cs
    '\n':cs   -> tokenise_string_newline str cs
    '<':cs    -> tokString str (tokenise_url cs)
    c:cs | isSpecial c     -> tokString str (TokSpecial c : tokenise1 cs)
	 | isSingleQuote c -> tokString str (tokenise_identifier c cs)
         | otherwise 	   -> tokenise_string (c:str) cs

tokenise_string_newline str cs =
  case dropWhile nonNewlineSpace cs  of
   '\n':cs -> tokString str (TokPara : tokenise_para cs)
		-- paragraph break: throw away all whitespace
   '>':cs  -> tokString ('\n':str) (TokBirdTrack : tokenise_birdtrack cs)
		-- keep the \n, but throw away any space before the '>'
   _other  -> tokenise_string ('\n':str) cs
		-- don't throw away whitespace at all

tokString [] rest = rest
tokString cs rest = TokString (reverse cs) : rest

-- A bird-tracked line is verbatim, no markup characters are interpreted
tokenise_birdtrack cs = 
  let (line, rest) = break (=='\n') cs in
  TokString line : tokenise1 rest

-- -----------------------------------------------------------------------------
-- Lex a string as a Haskell identifier

strToHsQNames :: String -> Maybe [HsQName]
strToHsQNames str
 = case lexer (\t -> returnP t) str (SrcLoc 1 1) 1 1 [] of
	Ok _ (VarId str)
	   -> Just [ UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QVarId (mod,str))
 	   -> Just [ Qual (Module mod) (HsVarName (HsIdent str)) ]
	Ok _ (ConId str)
	   -> Just [ UnQual (HsTyClsName (HsIdent str)),
		     UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QConId (mod,str))
	   -> Just [ Qual (Module mod) (HsTyClsName (HsIdent str)),
		     Qual (Module mod) (HsVarName (HsIdent str)) ]
        Ok _ (VarSym str)
	   -> Just [ UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (ConSym str)
	   -> Just [ UnQual (HsTyClsName (HsSymbol str)),
		     UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (QVarSym (mod,str))
	   -> Just [ Qual (Module mod) (HsVarName (HsSymbol str)) ]
        Ok _ (QConSym (mod,str))
	   -> Just [ Qual (Module mod) (HsTyClsName (HsSymbol str)),
		     Qual (Module mod) (HsVarName (HsSymbol str)) ]
	other
	   -> Nothing
