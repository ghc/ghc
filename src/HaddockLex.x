--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

{
module HaddockLex ( 
	Token(..), 
	tokenise 
 ) where

import Char
import Numeric
import HsSyn
import HsLexer hiding (Token)
import HsParseMonad
--import Debug.Trace
}

$ws    = $white # \n
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
$special =  [\"\@\/]
$alphanum = [A-Za-z0-9]
$ident    = [$alphanum \'\_\.\!\#\$\%\&\*\+\/\<\=\>\?\@\\\\\^\|\-\~]

:-

-- beginning of a paragraph
<0,para> {
 $ws* \n		;
 $ws* \>		{ begin birdtrack }
 $ws* [\*\-]		{ token TokBullet `andBegin` string }
 $ws* \[		{ token TokDefStart `andBegin` def }
 $ws* \( $digit+ \) 	{ token TokNumber `andBegin` string }
 $ws*			{ begin string }		
}

-- beginning of a line
<line> {
  $ws* \>		{ begin birdtrack }
  $ws* \n		{ token TokPara `andBegin` para }
  -- Here, we really want to be able to say
  -- $ws* (\n | <eof>) 	{ token TokPara `andBegin` para}
  -- because otherwise a trailing line of whitespace will result in 
  -- a spurious TokString at the end of a docstring.  We don't have <eof>,
  -- though (NOW I realise what it was for :-).  To get around this, we always
  -- append \n to the end of a docstring.
  () 			{ begin string }
}

<birdtrack> .*	\n?	{ strtoken TokBirdTrack `andBegin` line }

<string,def> {
  $special			{ strtoken $ \s -> TokSpecial (head s) }
  \<.*\>			{ strtoken $ \s -> TokURL (init (tail s)) }
  \#.*\#			{ strtoken $ \s -> TokAName (init (tail s)) }
  [\'\`] $ident+ [\'\`]		{ ident }
  \\ .				{ strtoken (TokString . tail) }
  "&#" $digit+ \;		{ strtoken $ \s -> TokString [chr (read (init (drop 2 s)))] }
  "&#" [xX] $hexdigit+ \;	{ strtoken $ \s -> case readHex (init (drop 3 s)) of [(n,_)] -> TokString [chr n] }
  -- allow special characters through if they don't fit one of the previous
  -- patterns.
  [\'\`\<\#\&\\]			{ strtoken TokString }
  [^ $special \< \# \n \'\` \& \\ \]]* \n { strtoken TokString `andBegin` line }
  [^ $special \< \# \n \'\` \& \\ \]]+    { strtoken TokString }
}

<def> {
  \]				{ token TokDefEnd `andBegin` string }
}

-- ']' doesn't have any special meaning outside of the [...] at the beginning
-- of a definition paragraph.
<string> {
  \]				{ strtoken TokString }
}

{
data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokDefStart
  | TokDefEnd
  | TokSpecial Char
  | TokIdent [HsQName]
  | TokString String
  | TokURL String
  | TokAName String
  | TokBirdTrack String
  deriving Show

-- -----------------------------------------------------------------------------
-- Alex support stuff

type StartCode = Int
type Action = String -> StartCode -> (StartCode -> [Token]) -> [Token]

type AlexInput = (Char,String)

alexGetChar (_, [])   = Nothing
alexGetChar (_, c:cs) = Just (c, (c,cs))

alexInputPrevChar (c,_) = c

tokenise :: String -> [Token]
tokenise str = let toks = go ('\n', eofHack str) para in {-trace (show toks)-} toks
  where go inp@(_,str) sc =
	  case alexScan inp sc of
		AlexEOF -> []
		AlexError _ -> error "lexical error"
		AlexSkip  inp' len     -> go inp' sc
		AlexToken inp' len act -> act (take len str) sc (\sc -> go inp' sc)

-- NB. we add a final \n to the string, (see comment in the beginning of line
-- production above).
eofHack str = str++"\n"

andBegin  :: Action -> StartCode -> Action
andBegin act new_sc = \str sc cont -> act str new_sc cont

token :: Token -> Action
token t = \str sc cont -> t : cont sc

strtoken :: (String -> Token) -> Action
strtoken t = \str sc cont -> t str : cont sc

begin :: StartCode -> Action
begin sc = \str _ cont -> cont sc

-- -----------------------------------------------------------------------------
-- Lex a string as a Haskell identifier

ident :: Action
ident str sc cont = 
  case strToHsQNames id of
	Just names -> TokIdent names : cont sc
	Nothing -> TokString str : cont sc
 where id = init (tail str)

strToHsQNames :: String -> Maybe [HsQName]
strToHsQNames str0
 = case lexer (\t -> returnP t) str0 (SrcLoc 1 1 "") 1 1 "" [] of
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
}
