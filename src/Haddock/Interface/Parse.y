{
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Haddock.Interface.Parse (
  parseHaddockParagraphs, 
  parseHaddockString 
) where

import Haddock.Interface.Lex
import Haddock.Types (Doc(..))
import Haddock.Doc
import HsSyn
import RdrName
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
	IDENT   { TokIdent $$ }
	PARA    { TokPara }
	STRING	{ TokString $$ }

%monad { Maybe }

%name parseHaddockParagraphs  doc
%name parseHaddockString seq

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

codepara :: { Doc RdrName }
	: '>..' codepara	{ docAppend (DocString $1) $2 }
	| '>..'			{ DocString $1 }

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
}
