{
module HaddockParse (parseParas, parseString) where

import HaddockLex
import HsSyn
import HsLexer hiding (Token)
import HsParseMonad
}

%tokentype { Token }

%token 	SQUO	{ TokSpecial '\'' }
	BQUO	{ TokSpecial '`' }
	DQUO 	{ TokSpecial '\"' }
	'/'	{ TokSpecial '/' }
	'@'	{ TokSpecial '@' }
	URL	{ TokURL $$ }
	'*'	{ TokBullet }
	'(n)'	{ TokNumber }
	'>'	{ TokBirdTrack }
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
	| squo STRING squo	{ DocIdentifier (strToHsQNames $2) }
	| DQUO STRING DQUO	{ DocModule $2 }

squo :: { () }
	: SQUO			{ () }
	| BQUO			{ () }

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

strToHsQNames :: String -> [ HsQName ]
strToHsQNames str
 = case lexer (\t -> returnP t) str (SrcLoc 1 1) 1 1 [] of
	Ok _ (VarId str)
		-> [ UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QVarId (mod,str))
		-> [ Qual (Module mod) (HsVarName (HsIdent str)) ]
	Ok _ (ConId str)
		-> [ UnQual (HsTyClsName (HsIdent str)),
		     UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QConId (mod,str))
		-> [ Qual (Module mod) (HsTyClsName (HsIdent str)),
		     Qual (Module mod) (HsVarName (HsIdent str)) ]
        Ok _ (VarSym str)
		-> [ UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (ConSym str)
		-> [ UnQual (HsTyClsName (HsSymbol str)),
		     UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (QVarSym (mod,str))
		-> [ Qual (Module mod) (HsVarName (HsSymbol str)) ]
        Ok _ (QConSym (mod,str))
		-> [ Qual (Module mod) (HsTyClsName (HsSymbol str)),
		     Qual (Module mod) (HsVarName (HsSymbol str)) ]
	other -> []
}
