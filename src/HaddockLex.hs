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

isSpecial, isSingleQuote, isIdent :: Char -> Bool
isSpecial c     = c `elem` ['\"', '@', '/']
isSingleQuote c = c `elem` ['\'', '`']
isIdent c       = isAlphaNum c || c `elem` "_.!#$%&*+/<=>?@\\^|-~"

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

tokenise_newline :: String -> [Token]
tokenise_newline cs0 =
 case dropWhile nonNewlineSpace cs0 of
   '\n':cs -> TokPara : tokenise_para cs -- paragraph break
   '>':cs  -> TokBirdTrack : tokenise_birdtrack cs -- bird track
   _       -> tokenise_string "" cs0

tokenise_para :: String -> [Token]
tokenise_para cs0 =
  case dropWhile nonNewlineSpace cs0 of   
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
   _ -> tokenise cs0

nonNewlineSpace :: Char -> Bool
nonNewlineSpace c = isSpace c && c /= '\n'

-- ----------------------------------------------------------------------------
-- Within a paragraph, we don't throw away any whitespace (except before a
-- birdtrack, and before a paragraph break).

tokenise1 :: String -> [Token]
tokenise1 str = tokenise_string "" str

-- found a single quote, check whether we have an identifier...
tokenise_identifier :: Char -> String -> [Token]
tokenise_identifier q cs0 =
  let (ident,rest) = break (not.isIdent) cs0 in
  case (rest, strToHsQNames ident) of
    (c:cs, Just names) | isSingleQuote c -> TokIdent names : tokenise1 cs
    _ -> tokenise_string [q] cs0

tokenise_url :: String -> [Token]
tokenise_url cs =
  let (url,rest0) = break (=='>') cs in
  TokURL url : case rest0 of
		 '>':rest -> tokenise1 rest
		 _ -> tokenise1 rest0

tokenise_string :: String -> String -> [Token]
tokenise_string str cs0 = 
  case cs0 of
    []        -> tokString str []
    '\\':c:cs -> tokenise_string (c:str) cs
    '\n':cs   -> tokenise_string_newline str cs
    '<':cs    -> tokString str (tokenise_url cs)
    c:cs | isSpecial c     -> tokString str (TokSpecial c : tokenise1 cs)
	 | isSingleQuote c -> tokString str (tokenise_identifier c cs)
         | otherwise 	   -> tokenise_string (c:str) cs

tokenise_string_newline :: String -> String -> [Token]
tokenise_string_newline str cs0 =
  case dropWhile nonNewlineSpace cs0  of
   '\n':cs -> tokString str (TokPara : tokenise_para cs)
		-- paragraph break: throw away all whitespace
   '>':cs  -> tokString ('\n':str) (TokBirdTrack : tokenise_birdtrack cs)
		-- keep the \n, but throw away any space before the '>'
   _       -> tokenise_string ('\n':str) cs0
		-- don't throw away whitespace at all

tokString :: String -> [Token] -> [Token]
tokString [] rest = rest
tokString cs rest = TokString (reverse cs) : rest

-- A bird-tracked line is verbatim, no markup characters are interpreted
tokenise_birdtrack :: String -> [Token]
tokenise_birdtrack cs = 
  let (line, rest) = break (=='\n') cs in
  TokString line : tokenise1 rest

-- -----------------------------------------------------------------------------
-- Lex a string as a Haskell identifier

strToHsQNames :: String -> Maybe [HsQName]
strToHsQNames str0
 = case lexer (\t -> returnP t) str0 (SrcLoc 1 1) 1 1 [] of
	Ok _ (VarId str)
	   -> Just [ UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QVarId (mod0,str))
 	   -> Just [ Qual (Module mod0) (HsVarName (HsIdent str)) ]
	Ok _ (ConId str)
	   -> Just [ UnQual (HsTyClsName (HsIdent str)),
		     UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QConId (mod0,str))
	   -> Just [ Qual (Module mod0) (HsTyClsName (HsIdent str)),
		     Qual (Module mod0) (HsVarName (HsIdent str)) ]
        Ok _ (VarSym str)
	   -> Just [ UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (ConSym str)
	   -> Just [ UnQual (HsTyClsName (HsSymbol str)),
		     UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (QVarSym (mod0,str))
	   -> Just [ Qual (Module mod0) (HsVarName (HsSymbol str)) ]
        Ok _ (QConSym (mod0,str))
	   -> Just [ Qual (Module mod0) (HsTyClsName (HsSymbol str)),
		     Qual (Module mod0) (HsVarName (HsSymbol str)) ]
	_other
	   -> Nothing
