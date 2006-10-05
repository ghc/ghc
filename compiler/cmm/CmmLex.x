-----------------------------------------------------------------------------
-- (c) The University of Glasgow, 2004
--
-- Lexer for concrete Cmm.  We try to stay close to the C-- spec, but there
-- are a few minor differences:
--
--   * extra keywords for our macros, and float32/float64 types
--   * global registers (Sp,Hp, etc.)
--
-----------------------------------------------------------------------------

{
module CmmLex (
   CmmToken(..), cmmlex,
  ) where

#include "HsVersions.h"

import Cmm
import Lexer

import SrcLoc
import UniqFM
import StringBuffer
import FastString
import Ctype
import Util		( readRational )
--import TRACE
}

$whitechar   = [\ \t\n\r\f\v\xa0]
$white_no_nl = $whitechar # \n

$ascdigit  = 0-9
$unidigit  = \x01
$digit     = [$ascdigit $unidigit]
$octit	   = 0-7
$hexit     = [$digit A-F a-f]

$unilarge  = \x03
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge $unilarge]

$unismall  = \x04
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall $unismall \_]

$namebegin = [$large $small \_ \. \$ \@]
$namechar  = [$namebegin $digit]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

@escape      = \\ ([abfnrt\\\'\"\?] | x @hexadecimal | @octal)
@strchar     = ($printable # [\"\\]) | @escape

cmm :-

$white_no_nl+		;
^\# pragma .* \n        ; -- Apple GCC 3.3 CPP generates pragmas in its output

^\# (line)? 		{ begin line_prag }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag> $digit+			{ setLine line_prag1 }
<line_prag1> \" ($printable # \")* \"	{ setFile line_prag2 }
<line_prag2> .*				{ pop }

<0> {
  \n			;

  [\:\;\{\}\[\]\(\)\=\`\~\/\*\%\-\+\&\^\|\>\<\,\!]	{ special_char }
  
  ".." 			{ kw CmmT_DotDot }
  "::" 			{ kw CmmT_DoubleColon }
  ">>"			{ kw CmmT_Shr }
  "<<"			{ kw CmmT_Shl }
  ">="			{ kw CmmT_Ge }
  "<="			{ kw CmmT_Le }
  "=="			{ kw CmmT_Eq }
  "!="			{ kw CmmT_Ne }
  "&&"			{ kw CmmT_BoolAnd }
  "||"			{ kw CmmT_BoolOr }
  
  R@decimal		{ global_regN VanillaReg }
  F@decimal		{ global_regN FloatReg }
  D@decimal		{ global_regN DoubleReg }
  L@decimal		{ global_regN LongReg }
  Sp			{ global_reg Sp }
  SpLim			{ global_reg SpLim }
  Hp			{ global_reg Hp }
  HpLim			{ global_reg HpLim }
  CurrentTSO		{ global_reg CurrentTSO }
  CurrentNursery	{ global_reg CurrentNursery }
  HpAlloc		{ global_reg HpAlloc }
  BaseReg		{ global_reg BaseReg }
  
  $namebegin $namechar*	{ name }
  
  0 @octal		{ tok_octal }
  @decimal		{ tok_decimal }
  0[xX] @hexadecimal	{ tok_hexadecimal }
  @floating_point	{ strtoken tok_float }
  
  \" @strchar* \"	{ strtoken tok_string }
}

{
data CmmToken
  = CmmT_SpecChar  Char
  | CmmT_DotDot
  | CmmT_DoubleColon
  | CmmT_Shr
  | CmmT_Shl
  | CmmT_Ge
  | CmmT_Le
  | CmmT_Eq
  | CmmT_Ne
  | CmmT_BoolAnd
  | CmmT_BoolOr
  | CmmT_CLOSURE
  | CmmT_INFO_TABLE
  | CmmT_INFO_TABLE_RET
  | CmmT_INFO_TABLE_FUN
  | CmmT_INFO_TABLE_CONSTR
  | CmmT_INFO_TABLE_SELECTOR
  | CmmT_else
  | CmmT_export
  | CmmT_section
  | CmmT_align
  | CmmT_goto
  | CmmT_if
  | CmmT_jump
  | CmmT_foreign
  | CmmT_prim
  | CmmT_import
  | CmmT_switch
  | CmmT_case
  | CmmT_default
  | CmmT_bits8
  | CmmT_bits16
  | CmmT_bits32
  | CmmT_bits64
  | CmmT_float32
  | CmmT_float64
  | CmmT_GlobalReg GlobalReg
  | CmmT_Name	   FastString
  | CmmT_String	   String
  | CmmT_Int	   Integer
  | CmmT_Float     Rational
  | CmmT_EOF
#ifdef DEBUG
  deriving (Show)
#endif

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = SrcSpan -> StringBuffer -> Int -> P (Located CmmToken)

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = do popLexState; lexToken

special_char :: Action
special_char span buf len = return (L span (CmmT_SpecChar (currentChar buf)))

kw :: CmmToken -> Action
kw tok span buf len = return (L span tok)

global_regN :: (Int -> GlobalReg) -> Action
global_regN con span buf len 
  = return (L span (CmmT_GlobalReg (con (fromIntegral n))))
  where buf' = stepOn buf
	n = parseInteger buf' (len-1) 10 octDecDigit

global_reg :: GlobalReg -> Action
global_reg r span buf len = return (L span (CmmT_GlobalReg r))

strtoken :: (String -> CmmToken) -> Action
strtoken f span buf len = 
  return (L span $! (f $! lexemeToString buf len))

name :: Action
name span buf len = 
  case lookupUFM reservedWordsFM fs of
	Just tok -> return (L span tok)
	Nothing  -> return (L span (CmmT_Name fs))
  where
	fs = lexemeToFastString buf len

reservedWordsFM = listToUFM $
	map (\(x, y) -> (mkFastString x, y)) [
	( "CLOSURE",		CmmT_CLOSURE ),
	( "INFO_TABLE",		CmmT_INFO_TABLE ),
	( "INFO_TABLE_RET",	CmmT_INFO_TABLE_RET ),
	( "INFO_TABLE_FUN",	CmmT_INFO_TABLE_FUN ),
	( "INFO_TABLE_CONSTR",	CmmT_INFO_TABLE_CONSTR ),
	( "INFO_TABLE_SELECTOR",CmmT_INFO_TABLE_SELECTOR ),
	( "else",		CmmT_else ),
	( "export",		CmmT_export ),
	( "section",		CmmT_section ),
	( "align",		CmmT_align ),
	( "goto",		CmmT_goto ),
	( "if",			CmmT_if ),
	( "jump",		CmmT_jump ),
	( "foreign",		CmmT_foreign ),
	( "prim",		CmmT_prim ),
	( "import",		CmmT_import ),
	( "switch",		CmmT_switch ),
	( "case",		CmmT_case ),
	( "default",		CmmT_default ),
	( "bits8",		CmmT_bits8 ),
	( "bits16",		CmmT_bits16 ),
	( "bits32",		CmmT_bits32 ),
	( "bits64",		CmmT_bits64 ),
	( "float32",		CmmT_float32 ),
	( "float64",		CmmT_float64 )
	]

tok_decimal span buf len 
  = return (L span (CmmT_Int  $! parseInteger buf len 10 octDecDigit))

tok_octal span buf len 
  = return (L span (CmmT_Int  $! parseInteger (offsetBytes 1 buf) (len-1) 8 octDecDigit))

tok_hexadecimal span buf len 
  = return (L span (CmmT_Int  $! parseInteger (offsetBytes 2 buf) (len-2) 16 hexDigit))

tok_float str = CmmT_Float $! readRational str

tok_string str = CmmT_String (read str)
		 -- urk, not quite right, but it'll do for now

-- -----------------------------------------------------------------------------
-- Line pragmas

setLine :: Int -> Action
setLine code span buf len = do
  let line = parseInteger buf len 10 octDecDigit
  setSrcLoc (mkSrcLoc (srcSpanFile span) (fromIntegral line - 1) 0)
	-- subtract one: the line number refers to the *following* line
  -- trace ("setLine "  ++ show line) $ do
  popLexState
  pushLexState code
  lexToken

setFile :: Int -> Action
setFile code span buf len = do
  let file = lexemeToFastString (stepOn buf) (len-2)
  setSrcLoc (mkSrcLoc file (srcSpanEndLine span) (srcSpanEndCol span))
  popLexState
  pushLexState code
  lexToken

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

cmmlex :: (Located CmmToken -> P a) -> P a
cmmlex cont = do
  tok@(L _ tok__) <- lexToken
  --trace ("token: " ++ show tok__) $ do
  cont tok

lexToken :: P (Located CmmToken)
lexToken = do
  inp@(loc1,buf) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> do let span = mkSrcSpan loc1 loc1
		  setLastToken span 0 0
		  return (L span CmmT_EOF)
    AlexError (loc2,_) -> do failLocMsgP loc1 loc2 "lexical error"
    AlexSkip inp2 _ -> do
	setInput inp2
	lexToken
    AlexToken inp2@(end,buf2) len t -> do
	setInput inp2
	let span = mkSrcSpan loc1 end
	span `seq` setLastToken span len len
	t span buf len

-- -----------------------------------------------------------------------------
-- Monad stuff

-- Stuff that Alex needs to know about our input type:
type AlexInput = (SrcLoc,StringBuffer)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,s) = prevChar s '\n'

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (loc,s) 
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq` Just (c, (loc', s'))
  where c = currentChar s
        loc' = advanceSrcLoc loc c
	s'   = stepOn s

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (l,b)

setInput :: AlexInput -> P ()
setInput (l,b) = P $ \s -> POk s{ loc=l, buffer=b } ()
}
