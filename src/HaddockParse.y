{
module HaddockParse (parseParas, parseString) where

import HaddockLex
import HsSyn
}

%tokentype { Token }

%token	'/'	{ TokSpecial '/' }
	'@'	{ TokSpecial '@' }
	DQUO 	{ TokSpecial '\"' }
	URL	{ TokURL $$ }
	'*'	{ TokBullet }
	'(n)'	{ TokNumber }
	'>'	{ TokBirdTrack }
	IDENT   { TokIdent $$ }
	PARA    { TokPara }
	STRING	{ TokString $$ }

%monad { Either String }

%name parseParas  doc
%name parseString seq

%%

doc	:: { Doc }
	: apara PARA doc	{ docAppend $1 $3 }
	| PARA doc 		{ $2 }
	| apara			{ $1 }
	| {- empty -}		{ DocEmpty }

apara	:: { Doc }
	: ulpara		{ DocUnorderedList [$1] }
	| olpara		{ DocOrderedList [$1] }
	| para			{ $1 }

ulpara  :: { Doc }
	: '*' para		{ $2 }

olpara  :: { Doc } 
	: '(n)' para		{ $2 }

para    :: { Doc }
	: seq			{ docParagraph $1 }
	| codepara		{ DocCodeBlock $1 }

codepara :: { Doc }
	: '>' seq codepara	{ docAppend $2 $3 }
	| '>' seq		{ $2 }

seq	:: { Doc }
	: elem seq		{ docAppend $1 $2 }
	| elem			{ $1 }

elem	:: { Doc }
	: elem1			{ $1 }
	| '@' seq1 '@'		{ DocMonospaced $2 }

seq1	:: { Doc }
	: elem1 seq1		{ docAppend $1 $2 }
	| elem1			{ $1 }

elem1	:: { Doc }
	: STRING		{ DocString $1 }
	| '/' STRING '/'	{ DocEmphasis (DocString $2) }
	| URL			{ DocURL $1 }
	| IDENT			{ DocIdentifier $1 }
	| DQUO STRING DQUO	{ DocModule $2 }

{
happyError :: [Token] -> Either String a
happyError toks = 
  Left ("parse error in doc string: "  ++ show (take 3 toks))

-- Either monad (we can't use MonadError because GHC < 5.00 has
-- an older incompatible version).
instance Monad (Either String) where
	return        = Right
	Left  l >>= _ = Left l
	Right r >>= k = k r
	fail msg      = Left msg
}
