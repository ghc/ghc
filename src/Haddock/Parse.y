{
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Haddock.Parse where

import Haddock.Lex
import Haddock.Types (Doc(..), Example(Example))
import Haddock.Doc
import HsSyn
import RdrName
import Data.Char  (isSpace)
import Data.Maybe (fromMaybe)
import Data.List  (stripPrefix)
}

%expect 0

%tokentype { Token }

%token	'/'	{ TokSpecial '/' }
	'@'	{ TokSpecial '@' }
	'['     { TokDefStart }
	']'     { TokDefEnd }
	DQUO 	{ TokSpecial '\"' }
	URL	{ TokURL $$ }
	PIC     { TokPic $$ }
	ANAME	{ TokAName $$ }
	'/../'  { TokEmphasis $$ }
	'-'	{ TokBullet }
	'(n)'	{ TokNumber }
	'>..'	{ TokBirdTrack $$ }
	PROMPT	{ TokExamplePrompt $$ }
	RESULT	{ TokExampleResult $$ }
	EXP	{ TokExampleExpression $$ }
	IDENT   { TokIdent $$ }
	PARA    { TokPara }
	STRING	{ TokString $$ }

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
	| examples		{ DocExamples $1 }

codepara :: { Doc RdrName }
	: '>..' codepara	{ docAppend (DocString $1) $2 }
	| '>..'			{ DocString $1 }

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
	| URL			{ DocURL $1 }
	| PIC                   { DocPic $1 }
	| ANAME			{ DocAName $1 }
	| IDENT			{ DocIdentifier $1 }
	| DQUO strings DQUO	{ DocModule $2 }

strings  :: { String }
	: STRING		{ $1 }
	| STRING strings	{ $1 ++ $2 }

{
happyError :: [Token] -> Maybe a
happyError toks = Nothing

-- | Create an 'Example', stripping superfluous characters as appropriate
makeExample :: String -> String -> [String] -> Example
makeExample prompt expression result =
  Example
	(strip expression)	-- we do not care about leading and trailing
				-- whitespace in expressions, so drop them
	result'
  where
	-- drop trailing whitespace from the prompt, remember the prefix
	(prefix, _) = span isSpace prompt
	-- drop, if possible, the exact same sequence of whitespace characters
	-- from each result line
	result' = map (tryStripPrefix prefix) result
	  where
		tryStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
}
