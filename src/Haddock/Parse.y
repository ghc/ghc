{
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Haddock.Parse (parseString, parseParas) where

import Haddock.Lex
import Haddock.Types (Doc(..), Example(Example), Hyperlink(..))
import Haddock.Doc
import HsSyn
import RdrName
import Data.Char  (isSpace)
import Data.Maybe (fromMaybe)
import Data.List  (stripPrefix)
}

%expect 0

%tokentype { LToken }

%token	'/'	{ (TokSpecial '/',_) }
	'@'	{ (TokSpecial '@',_) }
	'['     { (TokDefStart,_) }
	']'     { (TokDefEnd,_) }
	DQUO 	{ (TokSpecial '\"',_) }
	URL	{ (TokURL $$,_) }
	PIC     { (TokPic $$,_) }
	ANAME	{ (TokAName $$,_) }
	'/../'  { (TokEmphasis $$,_) }
	'-'	{ (TokBullet,_) }
	'(n)'	{ (TokNumber,_) }
	'>..'	{ (TokBirdTrack $$,_) }
	PROP	{ (TokProperty $$,_) }
	PROMPT	{ (TokExamplePrompt $$,_) }
	RESULT	{ (TokExampleResult $$,_) }
	EXP	{ (TokExampleExpression $$,_) }
	IDENT   { (TokIdent $$,_) }
	PARA    { (TokPara,_) }
	STRING	{ (TokString $$,_) }

%monad { Maybe }

%name parseParas doc
%name parseString seq

%%

doc	:: { Doc RdrName }
	: apara PARA doc	{ docAppend $1 $3 }
	| PARA doc 		{ $2 }
	| apara			{ $1 }
	| {- empty -}		{ DocEmpty }

apara	:: { Doc RdrName }
	: ulpara		{ DocUnorderedList [$1] }
	| olpara		{ DocOrderedList [$1] }
        | defpara               { DocDefList [$1] }
	| para			{ $1 }

ulpara  :: { Doc RdrName }
	: '-' para		{ $2 }

olpara  :: { Doc RdrName } 
	: '(n)' para		{ $2 }

defpara :: { (Doc RdrName, Doc RdrName) }
	: '[' seq ']' seq	{ ($2, $4) }

para    :: { Doc RdrName }
	: seq			{ docParagraph $1 }
	| codepara		{ DocCodeBlock $1 }
	| property		{ $1 }
	| examples		{ DocExamples $1 }

codepara :: { Doc RdrName }
	: '>..' codepara	{ docAppend (DocString $1) $2 }
	| '>..'			{ DocString $1 }

property :: { Doc RdrName }
	: PROP			{ makeProperty $1 }

examples :: { [Example] }
	: example examples	{ $1 : $2 }
	| example		{ [$1] }

example :: { Example }
	: PROMPT EXP result	{ makeExample $1 $2 (lines $3) }
	| PROMPT EXP		{ makeExample $1 $2 [] }

result :: { String }
	: RESULT result		{ $1 ++ $2 }
	| RESULT		{ $1 }

seq	:: { Doc RdrName }
	: elem seq		{ docAppend $1 $2 }
	| elem			{ $1 }

elem	:: { Doc RdrName }
	: elem1			{ $1 }
	| '@' seq1 '@'		{ DocMonospaced $2 }

seq1	:: { Doc RdrName }
	: PARA seq1             { docAppend (DocString "\n") $2 }
	| elem1 seq1            { docAppend $1 $2 }
	| elem1			{ $1 }

elem1	:: { Doc RdrName }
	: STRING		{ DocString $1 }
	| '/../'                { DocEmphasis (DocString $1) }
	| URL			{ DocHyperlink (makeHyperlink $1) }
	| PIC                   { DocPic $1 }
	| ANAME			{ DocAName $1 }
	| IDENT			{ DocIdentifier $1 }
	| DQUO strings DQUO	{ DocModule $2 }

strings  :: { String }
	: STRING		{ $1 }
	| STRING strings	{ $1 ++ $2 }

{
happyError :: [LToken] -> Maybe a
happyError toks = Nothing

-- | Create a `Hyperlink` from given string.
--
-- A hyperlink consists of a URL and an optional label.  The label is separated
-- from the url by one or more whitespace characters.
makeHyperlink :: String -> Hyperlink
makeHyperlink input = case break isSpace $ strip input of
  (url, "")    -> Hyperlink url Nothing
  (url, label) -> Hyperlink url (Just . dropWhile isSpace $ label)

makeProperty :: String -> Doc RdrName
makeProperty s = case strip s of
  'p':'r':'o':'p':'>':xs ->
	DocProperty (dropWhile isSpace xs)
  xs ->
	error $ "makeProperty: invalid input " ++ show xs

-- | Create an 'Example', stripping superfluous characters as appropriate
makeExample :: String -> String -> [String] -> Example
makeExample prompt expression result =
  Example
	(strip expression)	-- we do not care about leading and trailing
				-- whitespace in expressions, so drop them
	result'
  where
	-- 1. drop trailing whitespace from the prompt, remember the prefix
	(prefix, _) = span isSpace prompt

	-- 2. drop, if possible, the exact same sequence of whitespace
	-- characters from each result line
	--
	-- 3. interpret lines that only contain the string "<BLANKLINE>" as an
	-- empty line
	result' = map (substituteBlankLine . tryStripPrefix prefix) result
	  where
		tryStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys

		substituteBlankLine "<BLANKLINE>" = ""
		substituteBlankLine line          = line

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
}
