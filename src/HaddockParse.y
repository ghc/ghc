{
module HaddockParse (parseParas, parseString) where

import HaddockLex
import HaddockTypes
}

%tokentype { Token }

%token 	SQUO	{ TokSpecial '\'' }
	DQUO 	{ TokSpecial '\"' }
	'/'	{ TokSpecial '/' }
	'['	{ TokSpecial '[' }
	']'	{ TokSpecial ']' }
	'*'	{ TokBullet }
	'(n)'	{ TokNumber }
	PARA    { TokPara }
	STRING	{ TokString $$ }

%name parseParas  doc
%name parseString seq

%%

doc	:: { ParsedDoc }
	: apara PARA doc	{ docAppend $1 $3 }
	| apara			{ $1 }

apara	:: { ParsedDoc }
	: ulpara		{ DocUnorderedList [$1] }
	| olpara		{ DocOrderedList [$1] }
	| para			{ $1 }

ulpara  :: { ParsedDoc }
	: '*' para		{ $2 }

olpara  :: { ParsedDoc } 
	: '(n)' para		{ $2 }

para	: seq			{ docParagraph $1 }

seq	:: { ParsedDoc }
	: elem seq		{ DocAppend $1 $2 }
	| elem			{ $1 }

elem	:: { ParsedDoc }
	: STRING		{ DocString $1 }
	| '/' STRING '/'	{ DocEmphasis (DocString $2) }
	| SQUO STRING SQUO	{ DocIdentifier $2 }
	| DQUO STRING DQUO	{ DocModule $2 }
	| '[' seq ']'		{ DocMonospaced $2 }

{
happyError = error "Parse error in doc string"
}
