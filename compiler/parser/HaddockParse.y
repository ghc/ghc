{
module HaddockParse (parseHaddockParagraphs, parseHaddockString) where

import {-# SOURCE #-} HaddockLex
import HsSyn
import RdrName
}

%tokentype { Token }

%token	'/'	{ TokSpecial '/' }
	'@'	{ TokSpecial '@' }
	'['     { TokDefStart }
	']'     { TokDefEnd }
	DQUO 	{ TokSpecial '\"' }
	URL	{ TokURL $$ }
	ANAME	{ TokAName $$ }
	'-'	{ TokBullet }
	'(n)'	{ TokNumber }
	'>..'	{ TokBirdTrack $$ }
	IDENT   { TokIdent $$ }
	PARA    { TokPara }
	STRING	{ TokString $$ }

%monad { Either String }

%name parseHaddockParagraphs  doc
%name parseHaddockString seq

%%

doc	:: { HsDoc RdrName }
	: apara PARA doc	{ docAppend $1 $3 }
	| PARA doc 		{ $2 }
	| apara			{ $1 }
	| {- empty -}		{ DocEmpty }

apara	:: { HsDoc RdrName }
	: ulpara		{ DocUnorderedList [$1] }
	| olpara		{ DocOrderedList [$1] }
        | defpara               { DocDefList [$1] }
	| para			{ $1 }

ulpara  :: { HsDoc RdrName }
	: '-' para		{ $2 }

olpara  :: { HsDoc RdrName } 
	: '(n)' para		{ $2 }

defpara :: { (HsDoc RdrName, HsDoc RdrName) }
	: '[' seq ']' seq	{ ($2, $4) }

para    :: { HsDoc RdrName }
	: seq			{ docParagraph $1 }
	| codepara		{ DocCodeBlock $1 }

codepara :: { HsDoc RdrName }
	: '>..' codepara	{ docAppend (DocString $1) $2 }
	| '>..'			{ DocString $1 }

seq	:: { HsDoc RdrName }
	: elem seq		{ docAppend $1 $2 }
	| elem			{ $1 }

elem	:: { HsDoc RdrName }
	: elem1			{ $1 }
	| '@' seq1 '@'		{ DocMonospaced $2 }

seq1	:: { HsDoc RdrName }
	: elem1 seq1		{ docAppend $1 $2 }
	| elem1			{ $1 }

elem1	:: { HsDoc RdrName }
	: STRING		{ DocString $1 }
	| '/' strings '/'	{ DocEmphasis (DocString $2) }
	| URL			{ DocURL $1 }
	| ANAME			{ DocAName $1 }
	| IDENT			{ DocIdentifier $1 }
	| DQUO strings DQUO	{ DocModule $2 }

strings  :: { String }
	: STRING		{ $1 }
	| STRING strings	{ $1 ++ $2 }

{
happyError :: [Token] -> Either String a
happyError toks = 
--  Left ("parse error in doc string: "  ++ show (take 3 toks))
  Left ("parse error in doc string")

-- Either monad (we can't use MonadError because GHC < 5.00 has
-- an older incompatible version).
instance Monad (Either String) where
	return        = Right
	Left  l >>= _ = Left l
	Right r >>= k = k r
	fail msg      = Left msg
}
