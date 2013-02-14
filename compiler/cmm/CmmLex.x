-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004-2006
--
-- Lexer for concrete Cmm.  We try to stay close to the C-- spec, but there
-- are a few minor differences:
--
--   * extra keywords for our macros, and float32/float64 types
--   * global registers (Sp,Hp, etc.)
--
-----------------------------------------------------------------------------

{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wwarn -w #-}
-- The above -Wwarn supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module CmmLex (
   CmmToken(..), cmmlex,
  ) where

import CmmExpr

import Lexer
import SrcLoc
import UniqFM
import StringBuffer
import FastString
import Ctype
import Util
--import TRACE

import Data.Word
import Data.Char
}

$whitechar   = [\ \t\n\r\f\v\xa0] -- \xa0 is Unicode no-break space
$white_no_nl = $whitechar # \n

$ascdigit  = 0-9
$unidigit  = \x01 -- Trick Alex into handling Unicode. See alexGetChar.
$digit     = [$ascdigit $unidigit]
$octit	   = 0-7
$hexit     = [$digit A-F a-f]

$unilarge  = \x03 -- Trick Alex into handling Unicode. See alexGetChar.
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge $unilarge]

$unismall  = \x04 -- Trick Alex into handling Unicode. See alexGetChar.
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall $unismall \_]

$namebegin = [$large $small \. \$ \@]
$namechar  = [$namebegin $digit]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

@escape      = \\ ([abfnrt\\\'\"\?] | x $hexit{1,2} | $octit{1,3})
@strchar     = ($printable # [\"\\]) | @escape

cmm :-

$white_no_nl+		;
^\# pragma .* \n        ; -- Apple GCC 3.3 CPP generates pragmas in its output

^\# (line)? 		{ begin line_prag }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag> $digit+			{ setLine line_prag1 }
<line_prag1> \" [^\"]* \"	{ setFile line_prag2 }
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
  
  P@decimal		{ global_regN (\n -> VanillaReg n VGcPtr) }
  R@decimal		{ global_regN (\n -> VanillaReg n VNonGcPtr) }
  F@decimal		{ global_regN FloatReg }
  D@decimal		{ global_regN DoubleReg }
  L@decimal		{ global_regN LongReg }
  Sp			{ global_reg Sp }
  SpLim			{ global_reg SpLim }
  Hp			{ global_reg Hp }
  HpLim			{ global_reg HpLim }
  CCCS                  { global_reg CCCS }
  CurrentTSO            { global_reg CurrentTSO }
  CurrentNursery        { global_reg CurrentNursery }
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
  | CmmT_call
  | CmmT_jump
  | CmmT_foreign
  | CmmT_never
  | CmmT_prim
  | CmmT_return
  | CmmT_returns
  | CmmT_import
  | CmmT_switch
  | CmmT_case
  | CmmT_default
  | CmmT_push
  | CmmT_bits8
  | CmmT_bits16
  | CmmT_bits32
  | CmmT_bits64
  | CmmT_bits128
  | CmmT_bits256
  | CmmT_float32
  | CmmT_float64
  | CmmT_gcptr
  | CmmT_GlobalReg GlobalReg
  | CmmT_Name	   FastString
  | CmmT_String	   String
  | CmmT_Int	   Integer
  | CmmT_Float     Rational
  | CmmT_EOF
  deriving (Show)

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated CmmToken)

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
	n = parseUnsignedInteger buf' (len-1) 10 octDecDigit

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
        ( "call",               CmmT_call ),
        ( "jump",               CmmT_jump ),
        ( "foreign",            CmmT_foreign ),
	( "never",		CmmT_never ),
	( "prim",		CmmT_prim ),
	( "return",		CmmT_return ),
	( "returns",		CmmT_returns ),
	( "import",		CmmT_import ),
	( "switch",		CmmT_switch ),
	( "case",		CmmT_case ),
        ( "default",            CmmT_default ),
        ( "push",               CmmT_push ),
        ( "bits8",              CmmT_bits8 ),
	( "bits16",		CmmT_bits16 ),
	( "bits32",		CmmT_bits32 ),
	( "bits64",		CmmT_bits64 ),
	( "bits128",		CmmT_bits128 ),
	( "bits256",		CmmT_bits256 ),
	( "float32",		CmmT_float32 ),
	( "float64",		CmmT_float64 ),
-- New forms
	( "b8",			CmmT_bits8 ),
	( "b16",		CmmT_bits16 ),
	( "b32",		CmmT_bits32 ),
	( "b64",		CmmT_bits64 ),
	( "b128",		CmmT_bits128 ),
	( "b256",		CmmT_bits256 ),
	( "f32",		CmmT_float32 ),
	( "f64",		CmmT_float64 ),
	( "gcptr",		CmmT_gcptr )
	]

tok_decimal span buf len 
  = return (L span (CmmT_Int  $! parseUnsignedInteger buf len 10 octDecDigit))

tok_octal span buf len 
  = return (L span (CmmT_Int  $! parseUnsignedInteger (offsetBytes 1 buf) (len-1) 8 octDecDigit))

tok_hexadecimal span buf len 
  = return (L span (CmmT_Int  $! parseUnsignedInteger (offsetBytes 2 buf) (len-2) 16 hexDigit))

tok_float str = CmmT_Float $! readRational str

tok_string str = CmmT_String (read str)
		 -- urk, not quite right, but it'll do for now

-- -----------------------------------------------------------------------------
-- Line pragmas

setLine :: Int -> Action
setLine code span buf len = do
  let line = parseUnsignedInteger buf len 10 octDecDigit
  setSrcLoc (mkRealSrcLoc (srcSpanFile span) (fromIntegral line - 1) 1)
	-- subtract one: the line number refers to the *following* line
  -- trace ("setLine "  ++ show line) $ do
  popLexState
  pushLexState code
  lexToken

setFile :: Int -> Action
setFile code span buf len = do
  let file = lexemeToFastString (stepOn buf) (len-2)
  setSrcLoc (mkRealSrcLoc file (srcSpanEndLine span) (srcSpanEndCol span))
  popLexState
  pushLexState code
  lexToken

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

cmmlex :: (Located CmmToken -> P a) -> P a
cmmlex cont = do
  (L span tok) <- lexToken
  --trace ("token: " ++ show tok) $ do
  cont (L (RealSrcSpan span) tok)

lexToken :: P (RealLocated CmmToken)
lexToken = do
  inp@(loc1,buf) <- getInput
  sc <- getLexState
  case alexScan inp sc of
    AlexEOF -> do let span = mkRealSrcSpan loc1 loc1
		  setLastToken span 0
		  return (L span CmmT_EOF)
    AlexError (loc2,_) -> do failLocMsgP loc1 loc2 "lexical error"
    AlexSkip inp2 _ -> do
	setInput inp2
	lexToken
    AlexToken inp2@(end,buf2) len t -> do
	setInput inp2
	let span = mkRealSrcSpan loc1 end
	span `seq` setLastToken span len
	t span buf len

-- -----------------------------------------------------------------------------
-- Monad stuff

-- Stuff that Alex needs to know about our input type:
type AlexInput = (RealSrcLoc,StringBuffer)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,s) = prevChar s '\n'

-- backwards compatibility for Alex 2.x
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar inp = case alexGetByte inp of
                    Nothing    -> Nothing
                    Just (b,i) -> c `seq` Just (c,i)
                       where c = chr $ fromIntegral b

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (loc,s)
  | atEnd s   = Nothing
  | otherwise = b `seq` loc' `seq` s' `seq` Just (b, (loc', s'))
  where c    = currentChar s
        b    = fromIntegral $ ord $ c
        loc' = advanceSrcLoc loc c
	s'   = stepOn s

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (l,b)

setInput :: AlexInput -> P ()
setInput (l,b) = P $ \s -> POk s{ loc=l, buffer=b } ()
}
