-----------------------------------------------------------------------------
-- (c) The University of Glasgow, 2003
--
-- GHC's lexer.
--
-- This is a combination of an Alex-generated lexer from a regex
-- definition, with some hand-coded bits.
--
-- Completely accurate information about token-spans within the source
-- file is maintained.  Every token has a start and end SrcLoc attached to it.
--
-----------------------------------------------------------------------------

--   ToDo / known bugs:
--    - Unicode
--    - parsing integers is a bit slow
--    - readRational is a bit slow
--
--   Known bugs, that were also in the previous version:
--    - M... should be 3 tokens, not 1.
--    - pragma-end should be only valid in a pragma

{
module Lexer (
   Token(..), Token__(..), lexer, mkPState, showPFailed,
   P(..), ParseResult(..), setSrcLocFor, getSrcLoc, 
   failMsgP, failLocMsgP, srcParseFail,
   popContext, pushCurrentContext,
  ) where

#include "HsVersions.h"

import ForeignCall	( Safety(..) )
import ErrUtils		( Message )
import Outputable
import StringBuffer
import FastString
import FastTypes
import SrcLoc
import UniqFM
import CmdLineOpts
import Ctype
import Util		( maybePrefixMatch )

import DATA_BITS
import Char
import Ratio
import TRACE
}

$whitechar   = [\ \t\n\r\f\v]
$white_no_nl = $whitechar # \n

$ascdigit  = 0-9
$unidigit  = \x01
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = \x02
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$unilarge  = \x03
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge $unilarge]

$unismall  = \x04
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall $unismall \_]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit	   = 0-7
$hexit     = [$digit A-F a-f]
$symchar   = [$symbol \:]
$nl        = [\n\r]
$idchar    = [$small $large $digit \']

@varid     = $small $idchar*
@conid     = $large $idchar*

@varsym    = $symbol $symchar*
@consym    = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

-- we support the hierarchical module name extension:
@qual = (@conid \.)+

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

haskell :-

-- everywhere: skip whitespace and comments
$white_no_nl+ 				;

-- Everywhere: deal with nested comments.  We explicitly rule out
-- pragmas, "{-#", so that we don't accidentally treat them as comments.
-- (this can happen even though pragmas will normally take precedence due to
-- longest-match, because pragmas aren't valid in every state, but comments
-- are).
"{-" / { notFollowedBy '#' }		{ nested_comment }

-- Single-line comments are a bit tricky.  Haskell 98 says that two or
-- more dashes followed by a symbol should be parsed as a varsym, so we
-- have to exclude those.
-- The regex says: "munch all the characters after the dashes, as long as
-- the first one is not a symbol".
"--"\-* ([^$symbol] .*)?		;

-- 'bol' state: beginning of a line.  Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
--
-- One slight wibble here: what if the line begins with {-#? In
-- theory, we have to lex the pragma to see if it's one we recognise,
-- and if it is, then we backtrack and do_bol, otherwise we treat it
-- as a nested comment.  We don't bother with this: if the line begins
-- with {-#, then we'll assume it's a pragma we know about and go for do_bol.
<bol> {
  \n					;
  ^\# (line)?				{ begin line_prag1 }
  ^\# pragma .* \n			; -- GCC 3.3 CPP generated, apparently
  ()					{ do_bol }
}

-- after a layout keyword (let, where, do, of), we begin a new layout
-- context if the curly brace is missing.
-- Careful! This stuff is quite delicate.
<layout, layout_do> {
  \{ / { notFollowedBy '-' }		{ pop_and open_brace }
	-- we might encounter {-# here, but {- has been handled already
  \n					;
  ^\# (line)?				{ begin line_prag1 }
}

-- do is treated in a subtly different way, see new_layout_context
<layout>    ()				{ new_layout_context True }
<layout_do> ()				{ new_layout_context False }

-- after a new layout context which was found to be to the left of the
-- previous context, we have generated a '{' token, and we now need to
-- generate a matching '}' token.
<layout_left>  ()			{ do_layout_left }

<0,glaexts> \n				{ begin bol }

"{-#" $whitechar* (line|LINE) 		{ begin line_prag2 }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag1> $digit+			{ set_line line_prag1a }
<line_prag1a> \" [$graphic \ ]* \"	{ set_file line_prag1b }
<line_prag1b> .*			{ pop }

-- Haskell-style line pragmas, of the form
--    {-# LINE <line> "<file>" #-}
<line_prag2> $digit+			{ set_line line_prag2a }
<line_prag2a> \" [$graphic \ ]* \"	{ set_file line_prag2b }
<line_prag2b> "#-}"			{ pop }

<0,glaexts> {
  "{-#" $whitechar* (SPECIALI[SZ]E|speciali[sz]e)
  					{ token ITspecialise_prag }
  "{-#" $whitechar* (SOURCE|source)	{ token ITsource_prag }
  "{-#" $whitechar* (INLINE|inline)	{ token ITinline_prag }
  "{-#" $whitechar* (NO(T?)INLINE|no(t?)inline)
  					{ token ITnoinline_prag }
  "{-#" $whitechar* (RULES|rules)	{ token ITrules_prag }
  "{-#" $whitechar* (DEPRECATED|deprecated)
  					{ token ITdeprecated_prag }
  "{-#" $whitechar* (SCC|scc)		{ token ITscc_prag }
  "{-#" $whitechar* (CORE|core)		{ token ITcore_prag }
  
  "{-#" 				{ nested_comment }

  -- ToDo: should only be valid inside a pragma:
  "#-}" 				{ token ITclose_prag}
}


-- '0' state: ordinary lexemes
-- 'glaexts' state: glasgow extensions (postfix '#', etc.)

-- "special" symbols

<0,glaexts> {
  "[:" / { ifExtension parrEnabled }	{ token ITopabrack }
  ":]" / { ifExtension parrEnabled }	{ token ITcpabrack }
}
  
<0,glaexts> {
  "[|"	    / { ifExtension thEnabled }	{ token ITopenExpQuote }
  "[e|"	    / { ifExtension thEnabled }	{ token ITopenExpQuote }
  "[p|"	    / { ifExtension thEnabled }	{ token ITopenPatQuote }
  "[d|"	    / { ifExtension thEnabled }	{ layout_token ITopenDecQuote }
  "[t|"	    / { ifExtension thEnabled }	{ token ITopenTypQuote }
  "|]"	    / { ifExtension thEnabled }	{ token ITcloseQuote }
  \$ @varid / { ifExtension thEnabled }	{ skip_one_varid ITidEscape }
  "$("	    / { ifExtension thEnabled }	{ token ITparenEscape }
}

<0,glaexts> {
  "(|" / { ifExtension arrowsEnabled }  { special IToparenbar }
  "|)" / { ifExtension arrowsEnabled }  { special ITcparenbar }
}

<0,glaexts> {
  \? @varid / { ifExtension ipEnabled }	{ skip_one_varid ITdupipvarid }
  \% @varid / { ifExtension ipEnabled } { skip_one_varid ITsplitipvarid }
}

<glaexts> {
  "(#"					{ token IToubxparen }
  "#)"					{ token ITcubxparen }
  "{|"					{ token ITocurlybar }
  "|}"					{ token ITccurlybar }
}

<0,glaexts> {
  \(					{ special IToparen }
  \)					{ special ITcparen }
  \[					{ special ITobrack }
  \]					{ special ITcbrack }
  \,					{ special ITcomma }
  \;					{ special ITsemi }
  \`					{ special ITbackquote }
 				
  \{					{ open_brace }
  \}					{ close_brace }
}

<0,glaexts> {
  @qual @varid			{ check_qvarid }
  @qual @conid			{ idtoken qconid }
  @varid			{ varid }
  @conid			{ idtoken conid }
}

-- after an illegal qvarid, such as 'M.let', 
-- we back up and try again in the bad_qvarid state:
<bad_qvarid> {
  @conid			{ pop_and (idtoken conid) }
  @qual @conid			{ pop_and (idtoken qconid) }
}

<glaexts> {
  @qual @varid "#"+		{ idtoken qvarid }
  @qual @conid "#"+		{ idtoken qconid }
  @varid "#"+			{ varid }
  @conid "#"+			{ idtoken conid }
}

-- ToDo: M.(,,,)

<0,glaexts> {
  @qual @varsym			{ idtoken qvarsym }
  @qual @consym			{ idtoken qconsym }
  @varsym			{ varsym }
  @consym			{ consym }
}

<0,glaexts> {
  @decimal			{ tok_decimal }
  0[oO] @octal			{ tok_octal }
  0[xX] @hexadecimal		{ tok_hexadecimal }
}

<glaexts> {
  @decimal \#			{ prim_decimal }
  0[oO] @octal \#		{ prim_octal }
  0[xX] @hexadecimal \#		{ prim_hexadecimal }
}

<0,glaexts> @floating_point		{ strtoken tok_float }
<glaexts>   @floating_point \#		{ init_strtoken 1 prim_float }
<glaexts>   @floating_point \# \#	{ init_strtoken 2 prim_double }

-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0,glaexts> {
  \'				{ lex_char_tok }
  \" 				{ lex_string_tok }
}

<glaexts> "``" (([$graphic $whitechar] # \') | \' ([$graphic $whitechar] # \'))*
		"''"		{ clitlit }

{
-- work around bug in Alex 2.0
#if __GLASGOW_HASKELL__ < 503
unsafeAt arr i = arr ! i
#endif

-- -----------------------------------------------------------------------------
-- The token type

data Token = T SrcLoc{-start-} SrcLoc{-end-} Token__

data Token__
  = ITas  			-- Haskell keywords
  | ITcase
  | ITclass
  | ITdata
  | ITdefault
  | ITderiving
  | ITdo
  | ITelse
  | IThiding
  | ITif
  | ITimport
  | ITin
  | ITinfix
  | ITinfixl
  | ITinfixr
  | ITinstance
  | ITlet
  | ITmodule
  | ITnewtype
  | ITof
  | ITqualified
  | ITthen
  | ITtype
  | ITwhere
  | ITscc			-- ToDo: remove (we use {-# SCC "..." #-} now)

  | ITforall			-- GHC extension keywords
  | ITforeign
  | ITexport
  | ITlabel
  | ITdynamic
  | ITsafe
  | ITthreadsafe
  | ITunsafe
  | ITwith
  | ITstdcallconv
  | ITccallconv
  | ITdotnet
  | ITccall (Bool,Bool,Safety)	-- (is_dyn, is_casm, may_gc)
  | ITmdo

  | ITspecialise_prag		-- Pragmas
  | ITsource_prag
  | ITinline_prag
  | ITnoinline_prag
  | ITrules_prag
  | ITdeprecated_prag
  | ITline_prag
  | ITscc_prag
  | ITcore_prag                 -- hdaume: core annotations
  | ITclose_prag

  | ITdotdot  			-- reserved symbols
  | ITcolon
  | ITdcolon
  | ITequal
  | ITlam
  | ITvbar
  | ITlarrow
  | ITrarrow
  | ITat
  | ITtilde
  | ITdarrow
  | ITminus
  | ITbang
  | ITstar
  | ITdot

  | ITbiglam			-- GHC-extension symbols

  | ITocurly  			-- special symbols
  | ITccurly
  | ITocurlybar                 -- {|, for type applications
  | ITccurlybar                 -- |}, for type applications
  | ITvocurly
  | ITvccurly
  | ITobrack
  | ITopabrack			-- [:, for parallel arrays with -fparr
  | ITcpabrack			-- :], for parallel arrays with -fparr
  | ITcbrack
  | IToparen
  | ITcparen
  | IToubxparen
  | ITcubxparen
  | ITsemi
  | ITcomma
  | ITunderscore
  | ITbackquote

  | ITvarid   FastString	-- identifiers
  | ITconid   FastString
  | ITvarsym  FastString
  | ITconsym  FastString
  | ITqvarid  (FastString,FastString)
  | ITqconid  (FastString,FastString)
  | ITqvarsym (FastString,FastString)
  | ITqconsym (FastString,FastString)

  | ITdupipvarid   FastString	-- GHC extension: implicit param: ?x
  | ITsplitipvarid FastString	-- GHC extension: implicit param: %x

  | ITpragma StringBuffer

  | ITchar       Char
  | ITstring     FastString
  | ITinteger    Integer
  | ITrational   Rational

  | ITprimchar   Char
  | ITprimstring FastString
  | ITprimint    Integer
  | ITprimfloat  Rational
  | ITprimdouble Rational
  | ITlitlit     FastString

  -- MetaHaskell extension tokens
  | ITopenExpQuote  		-- [| or [e|
  | ITopenPatQuote		-- [p|
  | ITopenDecQuote		-- [d|
  | ITopenTypQuote		-- [t|         
  | ITcloseQuote		-- |]
  | ITidEscape   FastString	-- $x
  | ITparenEscape		-- $( 
  | ITreifyType
  | ITreifyDecl
  | ITreifyFixity

  -- Arrow notation extension
  | ITproc
  | ITrec
  | IToparenbar			-- (|
  | ITcparenbar			-- |)
  | ITlarrowtail		-- -<
  | ITrarrowtail		-- >-
  | ITLarrowtail		-- -<<
  | ITRarrowtail		-- >>-

  | ITunknown String		-- Used when the lexer can't make sense of it
  | ITeof			-- end of file token
#ifdef DEBUG
  deriving Show -- debugging
#endif

isSpecial :: Token__ -> Bool
-- If we see M.x, where x is a keyword, but
-- is special, we treat is as just plain M.x, 
-- not as a keyword.
isSpecial ITas        	= True
isSpecial IThiding    	= True
isSpecial ITqualified 	= True
isSpecial ITforall    	= True
isSpecial ITexport    	= True
isSpecial ITlabel     	= True
isSpecial ITdynamic   	= True
isSpecial ITsafe    	= True
isSpecial ITthreadsafe 	= True
isSpecial ITunsafe    	= True
isSpecial ITwith      	= True
isSpecial ITccallconv   = True
isSpecial ITstdcallconv = True
isSpecial ITmdo		= True
isSpecial _             = False

-- the bitmap provided as the third component indicates whether the
-- corresponding extension keyword is valid under the extension options
-- provided to the compiler; if the extension corresponding to *any* of the
-- bits set in the bitmap is enabled, the keyword is valid (this setup
-- facilitates using a keyword in two different extensions that can be
-- activated independently)
--
reservedWordsFM = listToUFM $
	map (\(x, y, z) -> (mkFastString x, (y, z)))
       [( "_",		ITunderscore, 	0 ),
	( "as",		ITas, 		0 ),
	( "case",	ITcase, 	0 ),     
	( "class",	ITclass, 	0 ),    
	( "data",	ITdata, 	0 ),     
	( "default",	ITdefault, 	0 ),  
	( "deriving",	ITderiving, 	0 ), 
	( "do",		ITdo, 		0 ),       
	( "else",	ITelse, 	0 ),     
	( "hiding",	IThiding, 	0 ),
	( "if",		ITif, 		0 ),       
	( "import",	ITimport, 	0 ),   
	( "in",		ITin, 		0 ),       
	( "infix",	ITinfix, 	0 ),    
	( "infixl",	ITinfixl, 	0 ),   
	( "infixr",	ITinfixr, 	0 ),   
	( "instance",	ITinstance, 	0 ), 
	( "let",	ITlet, 		0 ),      
	( "module",	ITmodule, 	0 ),   
	( "newtype",	ITnewtype, 	0 ),  
	( "of",		ITof, 		0 ),       
	( "qualified",	ITqualified, 	0 ),
	( "then",	ITthen, 	0 ),     
	( "type",	ITtype, 	0 ),     
	( "where",	ITwhere, 	0 ),
	( "_scc_",	ITscc, 		0 ),		-- ToDo: remove

      	( "forall",	ITforall,	 bit glaExtsBit),
	( "mdo",	ITmdo,		 bit glaExtsBit),
	( "reifyDecl",  ITreifyDecl,	 bit thBit),
	( "reifyType",  ITreifyType,	 bit thBit),
	( "reifyFixity",ITreifyFixity,	 bit thBit),

	( "foreign",	ITforeign,	 bit ffiBit),
	( "export",	ITexport,	 bit ffiBit),
	( "label",	ITlabel,	 bit ffiBit),
	( "dynamic",	ITdynamic,	 bit ffiBit),
	( "safe",	ITsafe,		 bit ffiBit),
	( "threadsafe",	ITthreadsafe,	 bit ffiBit),
	( "unsafe",	ITunsafe,	 bit ffiBit),
	( "stdcall",    ITstdcallconv,	 bit ffiBit),
	( "ccall",      ITccallconv,	 bit ffiBit),
	( "dotnet",     ITdotnet,	 bit ffiBit),

	( "with",	ITwith,		 bit withBit),

	( "rec",	ITrec,		 bit arrowsBit),
	( "proc",	ITproc,		 bit arrowsBit),

	-- On death row
        ("_ccall_",	ITccall (False, False, PlayRisky),
					 bit glaExtsBit),
        ("_ccall_GC_",	ITccall (False, False, PlaySafe False),
					 bit glaExtsBit),
        ("_casm_",	ITccall (False, True,  PlayRisky),
					 bit glaExtsBit),
        ("_casm_GC_",	ITccall (False, True,  PlaySafe False),
					 bit glaExtsBit)
     ]

reservedSymsFM = listToUFM $
	map (\ (x,y,z) -> (mkFastString x,(y,z)))
      [ ("..",	ITdotdot,	0)
       ,(":",	ITcolon,	0)	-- (:) is a reserved op, 
						-- meaning only list cons
       ,("::",	ITdcolon, 	0)
       ,("=",	ITequal, 	0)
       ,("\\",	ITlam, 		0)
       ,("|",	ITvbar, 	0)
       ,("<-",	ITlarrow, 	0)
       ,("->",	ITrarrow, 	0)
       ,("@",	ITat, 		0)
       ,("~",	ITtilde, 	0)
       ,("=>",	ITdarrow, 	0)
       ,("-",	ITminus, 	0)
       ,("!",	ITbang, 	0)

       ,("*",	ITstar,		bit glaExtsBit)	-- For data T (a::*) = MkT
       ,(".",	ITdot,		bit glaExtsBit)	-- For 'forall a . t'

       ,("-<",	ITlarrowtail,	bit arrowsBit)
       ,(">-",	ITrarrowtail,	bit arrowsBit)
       ,("-<<",	ITLarrowtail,	bit arrowsBit)
       ,(">>-",	ITRarrowtail,	bit arrowsBit)
       ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = SrcLoc -> SrcLoc -> StringBuffer -> Int -> P Token

special :: Token__ -> Action
special tok loc end _buf len = return (T loc end tok)

token, layout_token :: Token__ -> Action
token t loc end buf len = return (T loc end t)
layout_token t loc end buf len = pushLexState layout >> return (T loc end t)

idtoken :: (StringBuffer -> Int -> Token__) -> Action
idtoken f loc end buf len = return (T loc end $! (f buf len))

skip_one_varid :: (FastString -> Token__) -> Action
skip_one_varid f loc end buf len 
  = return (T loc end $! f (lexemeToFastString (stepOn buf) (len-1)))

strtoken :: (String -> Token__) -> Action
strtoken f loc end buf len = 
  return (T loc end $! (f $! lexemeToString buf len))

init_strtoken :: Int -> (String -> Token__) -> Action
-- like strtoken, but drops the last N character(s)
init_strtoken drop f loc end buf len = 
  return (T loc end $! (f $! lexemeToString buf (len-drop)))

begin :: Int -> Action
begin code _loc _end _str _len = do pushLexState code; lexToken

pop :: Action
pop _loc _end _buf _len = do popLexState; lexToken

pop_and :: Action -> Action
pop_and act loc end buf len = do popLexState; act loc end buf len

notFollowedBy char _ _ _ (_,buf) = atEnd buf || currentChar buf /= char

ifExtension pred bits _ _ _ = pred bits

{-
  nested comments require traversing by hand, they can't be parsed
  using regular expressions.
-}
nested_comment :: Action
nested_comment loc _end _str _len = do
  input <- getInput
  go 1 input
  where go 0 input = do setInput input; lexToken
	go n input = do
	  case alexGetChar input of
	    Nothing  -> err input
	    Just (c,input) -> do
	      case c of
	    	'-' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('\125',input) -> go (n-1) input
		    Just (c,_)          -> go n input
	     	'\123' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('-',input') -> go (n+1) input'
		    Just (c,input)    -> go n input
	    	c -> go n input

        err input = do failLocMsgP loc (fst input) "unterminated `{-'"

open_brace, close_brace :: Action
open_brace  loc end _str _len = do 
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (T loc end ITocurly)
close_brace loc end _str _len = do 
  popContext
  return (T loc end ITccurly)

-- We have to be careful not to count M.<varid> as a qualified name
-- when <varid> is a keyword.  We hack around this by catching 
-- the offending tokens afterward, and re-lexing in a different state.
check_qvarid loc end buf len = do
  case lookupUFM reservedWordsFM var of
	Just (keyword,exts)
	  | not (isSpecial keyword) ->
	  if exts == 0 
	     then try_again
	     else do
		b <- extension (\i -> exts .&. i /= 0)
		if b then try_again
		     else return token
	_other -> return token
  where
	(mod,var) = splitQualName buf len
	token     = T loc end (ITqvarid (mod,var))

	try_again = do
		setInput (loc,buf)
		pushLexState bad_qvarid
		lexToken

qvarid buf len = ITqvarid $! splitQualName buf len
qconid buf len = ITqconid $! splitQualName buf len

splitQualName :: StringBuffer -> Int -> (FastString,FastString)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
splitQualName orig_buf len = split orig_buf 0 0
  where
    split buf dot_off n
	| n == len		  = done dot_off
	| lookAhead buf n == '.'  = split2 buf n (n+1)
	| otherwise 		  = split buf dot_off (n+1)	
  
    -- careful, we might get names like M....
    -- so, if the character after the dot is not upper-case, this is
    -- the end of the qualifier part.
    split2 buf dot_off n
	| isUpper (lookAhead buf n) = split buf dot_off (n+1)
	| otherwise 		    = done dot_off

    done dot_off =
	(lexemeToFastString orig_buf dot_off, 
	 lexemeToFastString (stepOnBy (dot_off+1) orig_buf) (len - dot_off -1))

varid loc end buf len = 
  case lookupUFM reservedWordsFM fs of
	Just (keyword,0)    -> do
		maybe_layout keyword
		return (T loc end keyword)
	Just (keyword,exts) -> do
		b <- extension (\i -> exts .&. i /= 0)
		if b then do maybe_layout keyword
			     return (T loc end keyword)
		     else return (T loc end (ITvarid fs))
	_other -> return (T loc end (ITvarid fs))
  where
	fs = lexemeToFastString buf len

conid buf len = ITconid fs
  where fs = lexemeToFastString buf len

qvarsym buf len = ITqvarsym $! splitQualName buf len
qconsym buf len = ITqconsym $! splitQualName buf len

varsym = sym ITvarsym
consym = sym ITconsym

sym con loc end buf len = 
  case lookupUFM reservedSymsFM fs of
	Just (keyword,0)    -> return (T loc end keyword)
	Just (keyword,exts) -> do
		b <- extension (\i -> exts .&. i /= 0)
		if b then return (T loc end keyword)
		     else return (T loc end $! con fs)
	_other -> return (T loc end $! con fs)
  where
	fs = lexemeToFastString buf len

tok_decimal loc end buf len 
  = return (T loc end (ITinteger  $! parseInteger buf len 10 oct_or_dec))

tok_octal loc end buf len 
  = return (T loc end (ITinteger  $! parseInteger (stepOnBy 2 buf) (len-2) 8 oct_or_dec))

tok_hexadecimal loc end buf len 
  = return (T loc end (ITinteger  $! parseInteger (stepOnBy 2 buf) (len-2) 16 hex))

prim_decimal loc end buf len 
  = return (T loc end (ITprimint  $! parseInteger buf (len-1) 10 oct_or_dec))

prim_octal loc end buf len 
  = return (T loc end (ITprimint  $! parseInteger (stepOnBy 2 buf) (len-3) 8 oct_or_dec))

prim_hexadecimal loc end buf len 
  = return (T loc end (ITprimint  $! parseInteger (stepOnBy 2 buf) (len-3) 16 hex))

tok_float        str = ITrational $! readRational__ str
prim_float       str = ITprimfloat  $! readRational__ str
prim_double      str = ITprimdouble $! readRational__ str

parseInteger :: StringBuffer -> Int -> Integer -> (Char->Int) -> Integer
parseInteger buf len radix to_int 
  = go 0 0
  where go i x | i == len  = x
	       | otherwise = go (i+1) (x * radix + toInteger (to_int (lookAhead buf i)))

clitlit :: Action
clitlit loc end buf len = 
  return (T loc end (ITlitlit $! lexemeToFastString (stepOnBy 2 buf) (len-4)))

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol loc end _str _len = do
	pos <- getOffside end
	case pos of
	    LT -> do
                --trace "layout: inserting '}'" $ do
		popContext
		-- do NOT pop the lex state, we might have a ';' to insert
		return (T loc end ITvccurly)
	    EQ -> do
                --trace "layout: inserting ';'" $ do
		popLexState
		return (T loc end ITsemi)
	    GT -> do
		popLexState
		lexToken

-- certain keywords put us in the "layout" state, where we might
-- add an opening curly brace.
maybe_layout ITdo	= pushLexState layout_do
maybe_layout ITmdo	= pushLexState layout_do
maybe_layout ITof	= pushLexState layout
maybe_layout ITlet	= pushLexState layout
maybe_layout ITwhere	= pushLexState layout
maybe_layout ITrec	= pushLexState layout
maybe_layout _	        = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
--
new_layout_context strict loc end _buf _len = do
    popLexState
    let offset = srcLocCol loc
    ctx <- getContext
    case ctx of
	Layout prev_off : _  | 
	   (strict     && prev_off >= offset  ||
	    not strict && prev_off > offset) -> do
		-- token is indented to the left of the previous context.
		-- we must generate a {} sequence now.
		pushLexState layout_left
		return (T loc end ITvocurly)
	other -> do
		setContext (Layout offset : ctx)
		return (T loc end ITvocurly)

do_layout_left loc end _buf _len = do
    popLexState
    pushLexState bol  -- we must be at the start of a line
    return (T loc end ITvccurly)

-- -----------------------------------------------------------------------------
-- LINE pragmas

set_line :: Int -> Action
set_line code loc end buf len = do
  let line = parseInteger buf len 10 oct_or_dec
  setSrcLoc (mkSrcLoc (srcLocFile end) (fromIntegral line - 1) 0)
	-- subtract one: the line number refers to the *following* line
  popLexState
  pushLexState code
  lexToken

set_file :: Int -> Action
set_file code loc end buf len = do
  let file = lexemeToFastString (stepOn buf) (len-2)
  setSrcLoc (mkSrcLoc file (srcLocLine end) (srcLocCol end))
  popLexState
  pushLexState code
  lexToken

-- -----------------------------------------------------------------------------
-- Strings & Chars

-- This stuff is horrible.  I hates it.

lex_string_tok :: Action
lex_string_tok loc end buf len = do
  tok <- lex_string ""
  end <- getSrcLoc 
  return (T loc end tok)

lex_string :: String -> P Token__
lex_string s = do
  i <- getInput
  case alexGetChar i of
    Nothing -> lit_error

    Just ('"',i)  -> do
	setInput i
	glaexts <- extension glaExtsEnabled
	if glaexts
	  then do
	    i <- getInput
	    case alexGetChar i of
	      Just ('#',i) -> do
		   setInput i
		   if any (> '\xFF') s
                    then failMsgP "primitive string literal must contain only characters <= \'\\xFF\'"
                    else let s' = mkFastStringNarrow (reverse s) in
			 -- always a narrow string/byte array
			 return (ITprimstring s')
	      _other ->
		return (ITstring (mkFastString (reverse s)))
	  else
		return (ITstring (mkFastString (reverse s)))

    Just ('\\',i)
	| Just ('&',i) <- next -> do 
		setInput i; lex_string s
	| Just (c,i) <- next, is_space c -> do 
		setInput i; lex_stringgap s
	where next = alexGetChar i

    Just _ -> do
	c <- lex_char
	lex_string (c:s)


lex_stringgap s = do
  c <- getCharOrFail
  case c of
    '\\' -> lex_string s
    c | is_space c -> lex_stringgap s
    _other -> lit_error


lex_char_tok :: Action
lex_char_tok loc _end buf len = do
   c <- lex_char
   mc <- getCharOrFail
   case mc of
	'\'' -> do
	   glaexts <- extension glaExtsEnabled
	   if glaexts
		then do
		   i@(end,_) <- getInput
		   case alexGetChar i of
			Just ('#',i@(end,_)) -> do
				setInput i
				return (T loc end (ITprimchar c))
			_other ->
				return (T loc end (ITchar c))
	        else do
		   end <- getSrcLoc
		   return (T loc end (ITchar c))

	_other -> lit_error

lex_char :: P Char
lex_char = do
  mc <- getCharOrFail
  case mc of
      '\\' -> lex_escape
      c | is_any c -> return c
      _other -> lit_error

lex_escape :: P Char
lex_escape = do
  c <- getCharOrFail
  case c of
	'a'   -> return '\a'
	'b'   -> return '\b'
	'f'   -> return '\f'
	'n'   -> return '\n'
	'r'   -> return '\r'
	't'   -> return '\t'
	'v'   -> return '\v'
	'\\'  -> return '\\'
	'"'   -> return '\"'
	'\''  -> return '\''
	'^'   -> do c <- getCharOrFail
		    if c >= '@' && c <= '_'
			then return (chr (ord c - ord '@'))
			else lit_error

	'x'   -> readNum is_hexdigit 16 hex
	'o'   -> readNum is_octdigit  8 oct_or_dec
	x | is_digit x -> readNum2 is_digit 10 oct_or_dec (oct_or_dec x)

	c1 ->  do
	   i <- getInput
	   case alexGetChar i of
	    Nothing -> lit_error
	    Just (c2,i2) -> 
              case alexGetChar i2 of
		Nothing	-> lit_error
		Just (c3,i3) -> 
		   let str = [c1,c2,c3] in
		   case [ (c,rest) | (p,c) <- silly_escape_chars,
			      	     Just rest <- [maybePrefixMatch p str] ] of
			  (escape_char,[]):_ -> do
				setInput i3
				return escape_char
			  (escape_char,_:_):_ -> do
				setInput i2
				return escape_char
			  [] -> lit_error

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Char
readNum is_digit base conv = do
  c <- getCharOrFail
  if is_digit c 
	then readNum2 is_digit base conv (conv c)
	else lit_error

readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
	  case alexGetChar input of
	    Just (c,input') | is_digit c -> do
		read (i*base + conv c) input'
	    _other -> do
		setInput input
		if i >= 0 && i <= 0x10FFFF
		   then return (chr i)
		   else lit_error

is_hexdigit c
	=  is_digit c 
	|| (c >= 'a' && c <= 'f')
	|| (c >= 'A' && c <= 'F')

hex c | is_digit c = ord c - ord '0'
      | otherwise  = ord (to_lower c) - ord 'a' + 10

oct_or_dec c = ord c - ord '0'

is_octdigit c = c >= '0' && c <= '7'

to_lower c 
  | c >=  'A' && c <= 'Z' = chr (ord c - (ord 'A' - ord 'a'))
  | otherwise = c

silly_escape_chars = [
	("NUL", '\NUL'),
	("SOH", '\SOH'),
	("STX", '\STX'),
	("ETX", '\ETX'),
	("EOT", '\EOT'),
	("ENQ", '\ENQ'),
	("ACK", '\ACK'),
	("BEL", '\BEL'),
	("BS", '\BS'),
	("HT", '\HT'),
	("LF", '\LF'),
	("VT", '\VT'),
	("FF", '\FF'),
	("CR", '\CR'),
	("SO", '\SO'),
	("SI", '\SI'),
	("DLE", '\DLE'),
	("DC1", '\DC1'),
	("DC2", '\DC2'),
	("DC3", '\DC3'),
	("DC4", '\DC4'),
	("NAK", '\NAK'),
	("SYN", '\SYN'),
	("ETB", '\ETB'),
	("CAN", '\CAN'),
	("EM", '\EM'),
	("SUB", '\SUB'),
	("ESC", '\ESC'),
	("FS", '\FS'),
	("GS", '\GS'),
	("RS", '\RS'),
	("US", '\US'),
	("SP", '\SP'),
	("DEL", '\DEL')
	]

lit_error = lexError "lexical error in string/character literal"

getCharOrFail :: P Char
getCharOrFail =  do
  i <- getInput
  case alexGetChar i of
	Nothing -> lexError "unexpected end-of-file in string/character literal"
	Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------
-- Floats

readRational :: ReadS Rational -- NB: doesn't handle leading "-"
readRational r = do 
     (n,d,s) <- readFix r
     (k,t)   <- readExp s
     return ((n%1)*10^^(k-d), t)
 where
     readFix r = do
	(ds,s)  <- lexDecDigits r
	(ds',t) <- lexDotDigits s
	return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` "eE" = readExp' s
     readExp s			   = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do
			(k,t) <- readDec s
			return (-k,t)
     readExp' s	      = readDec s

     readDec s = do
        (ds,r) <- nonnull isDigit s
        return (foldl1 (\n d -> n * 10 + d) [ ord d - ord '0' | d <- ds ],
                r)

     lexDecDigits = nonnull isDigit

     lexDotDigits ('.':s) = return (span isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span p s)
                      return (cs,t)

readRational__ :: String -> Rational -- NB: *does* handle a leading "-"
readRational__ top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case (do { (x,"") <- readRational s ; return x }) of
	  [x] -> x
	  []  -> error ("readRational__: no parse:"        ++ top_s)
	  _   -> error ("readRational__: ambiguous parse:" ++ top_s)

-- -----------------------------------------------------------------------------
-- The Parse Monad

data LayoutContext
  = NoLayout
  | Layout !Int

data ParseResult a
  = POk PState a
  | PFailed 
	SrcLoc SrcLoc	-- The start and end of the text span related to
			-- the error.  Might be used in environments which can 
			-- show this span, e.g. by highlighting it.
	Message		-- The error message

showPFailed loc1 loc2 err
 = showSDoc (hcat [ppr loc1, text ": ", err])

data PState = PState { 
	buffer	   :: StringBuffer,
        last_loc   :: SrcLoc,		-- pos of previous token
	last_len   :: !Int,		-- len of previous token
        loc        :: SrcLoc,   -- current loc (end of prev token + 1)
	extsBitmap :: !Int,	-- bitmap that determines permitted extensions
	context	   :: [LayoutContext],
	lex_state  :: [Int]
     }
	-- last_loc and last_len are used when generating error messages,
	-- and in pushCurrentContext only.

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail = failP

returnP :: a -> P a
returnP a = P $ \s -> POk s a

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
	case m s of
		POk s1 a          -> (unP (k a)) s1
		PFailed l1 l2 err -> PFailed l1 l2 err

failP :: String -> P a
failP msg = P $ \s -> PFailed (last_loc s) (loc s) (text msg)

failMsgP :: String -> P a
failMsgP msg = P $ \s -> PFailed (last_loc s) (loc s) (text msg)

failLocMsgP :: SrcLoc -> SrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \s -> PFailed loc1 loc2 (text str)

extension :: (Int -> Bool) -> P Bool
extension p = P $ \s -> POk s (p $! extsBitmap s)

getExts :: P Int
getExts = P $ \s -> POk s (extsBitmap s)

setSrcLoc :: SrcLoc -> P ()
setSrcLoc new_loc = P $ \s -> POk s{loc=new_loc} ()

-- tmp, for supporting stuff in RdrHsSyn.  The scope better not include
-- any calls to the lexer, because it assumes things about the SrcLoc.
setSrcLocFor :: SrcLoc -> P a -> P a
setSrcLocFor new_loc scope = P $ \s@PState{ loc = old_loc } -> 
  case unP scope s{loc=new_loc} of
	PFailed l1 l2 msg -> PFailed l1 l2 msg
	POk _ r -> POk s r

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

setLastToken :: SrcLoc -> Int -> P ()
setLastToken loc len = P $ \s -> POk s{ last_loc=loc, last_len=len } ()

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

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:l } -> POk s ls

-- for reasons of efficiency, flags indicating language extensions (eg,
-- -fglasgow-exts or -fparr) are represented by a bitmap stored in an unboxed
-- integer

glaExtsBit, ffiBit, parrBit :: Int
glaExtsBit = 0
ffiBit	   = 1
parrBit	   = 2
withBit	   = 3
arrowsBit  = 4
thBit	   = 5
ipBit      = 6

glaExtsEnabled, ffiEnabled, parrEnabled :: Int -> Bool
glaExtsEnabled flags = testBit flags glaExtsBit
ffiEnabled     flags = testBit flags ffiBit
withEnabled    flags = testBit flags withBit
parrEnabled    flags = testBit flags parrBit
arrowsEnabled  flags = testBit flags arrowsBit
thEnabled      flags = testBit flags thBit
ipEnabled      flags = testBit flags ipBit

-- create a parse state
--
mkPState :: StringBuffer -> SrcLoc -> DynFlags -> PState
mkPState buf loc flags  = 
  PState {
      buffer	 = buf,
      last_loc   = loc,
      last_len   = 0,
      loc        = loc,
      extsBitmap = fromIntegral bitmap,
      context    = [],
      lex_state  = [bol, if glaExtsEnabled bitmap then glaexts else 0]
	-- we begin in the layout state if toplev_layout is set
    }
    where
      bitmap =     glaExtsBit `setBitIf` dopt Opt_GlasgowExts flags
	       .|. ffiBit     `setBitIf` dopt Opt_FFI         flags
	       .|. withBit    `setBitIf` dopt Opt_With	      flags
	       .|. parrBit    `setBitIf` dopt Opt_PArr        flags
	       .|. arrowsBit  `setBitIf` dopt Opt_Arrows      flags
	       .|. thBit      `setBitIf` dopt Opt_TH          flags
	       .|. ipBit      `setBitIf` dopt Opt_ImplicitParams flags
      --
      setBitIf :: Int -> Bool -> Int
      b `setBitIf` cond | cond      = bit b
			| otherwise = 0

getContext :: P [LayoutContext]
getContext = P $ \s@PState{context=ctx} -> POk s ctx

setContext :: [LayoutContext] -> P ()
setContext ctx = P $ \s -> POk s{context=ctx} ()

popContext :: P ()
popContext = P $ \ s@(PState{ buffer = buf, context = ctx, 
			   loc = loc, last_len = len, last_loc = last_loc }) ->
  case ctx of
	(_:tl) -> POk s{ context = tl } ()
	[]     -> PFailed last_loc loc (srcParseErr buf len)

-- Push a new layout context at the indentation of the last token read.
-- This is only used at the outer level of a module when the 'module'
-- keyword is missing.
pushCurrentContext :: P ()
pushCurrentContext = P $ \ s@PState{ last_loc=loc, context=ctx } ->
  POk s{ context = Layout (srcLocCol loc) : ctx} ()

getOffside :: SrcLoc -> P Ordering
getOffside loc = P $ \s@PState{context=stk} ->
		let ord = case stk of
			(Layout n:_) -> compare (srcLocCol loc) n
			_            -> GT
		in POk s ord

-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: StringBuffer	-- current buffer (placed just after the last token)
  -> Int		-- length of the previous token
  -> Message
srcParseErr buf len
  = hcat [ if null token 
	     then ptext SLIT("parse error (possibly incorrect indentation)")
	     else hcat [ptext SLIT("parse error on input "),
          	  	char '`', text token, char '\'']
    ]
  where token = lexemeToString (stepOnBy (-len) buf) len

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \PState{ buffer = buf, last_len = len, 	
				last_loc = last_loc, loc = loc } ->
    PFailed last_loc loc (srcParseErr buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.  TODO: this is slightly wrong, because we record
-- the error at the character position following the one which caused the
-- error.  We should somehow back up by one character.
lexError :: String -> P a
lexError str = do
  loc <- getSrcLoc
  failLocMsgP loc loc str

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: (Token -> P a) -> P a
lexer cont = do
  tok@(T _ _ tok__) <- lexToken
  --trace ("token: " ++ show tok__) $ do
  cont tok

lexToken :: P Token
lexToken = do
  inp@(loc1,buf) <- getInput
  sc <- getLexState
  exts <- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do setLastToken loc1 0
		  return (T loc1 loc1 ITeof)
    AlexError (loc2,_) -> do failLocMsgP loc1 loc2 "lexical error"
    AlexSkip inp2 _ -> do
	setInput inp2
	lexToken
    AlexToken inp2@(end,buf2) len t -> do
	setInput inp2
	setLastToken loc1 len
	t loc1 end buf len
}
