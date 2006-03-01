-----------------------------------------------------------------------------
-- (c) The University of Glasgow, 2006
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
   Token(..), lexer, mkPState, PState(..),
   P(..), ParseResult(..), getSrcLoc, 
   failLocMsgP, failSpanMsgP, srcParseFail,
   popContext, pushCurrentContext, setLastToken, setSrcLoc,
   getLexState, popLexState, pushLexState,
   extension, bangPatEnabled
  ) where

#include "HsVersions.h"

import ErrUtils		( Message )
import Outputable
import StringBuffer
import FastString
import FastTypes
import SrcLoc
import UniqFM
import DynFlags
import Ctype
import Util		( maybePrefixMatch, readRational )

import DATA_BITS
import Data.Char
import Ratio
--import TRACE

#if __GLASGOW_HASKELL__ >= 605
import Data.Char 	( GeneralCategory(..), generalCategory )
#else
import Compat.Unicode	( GeneralCategory(..), generalCategory )
#endif
}

$unispace    = \x05
$whitechar   = [\ \t\n\r\f\v\xa0 $unispace]
$white_no_nl = $whitechar # \n

$ascdigit  = 0-9
$unidigit  = \x03
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = \x04
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$unilarge  = \x01
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge $unilarge]

$unismall  = \x02
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall $unismall \_]

$unigraphic = \x06
$graphic   = [$small $large $symbol $digit $special $unigraphic \:\"\']

$octit	   = 0-7
$hexit     = [$decdigit A-F a-f]
$symchar   = [$symbol \:]
$nl        = [\n\r]
$idchar    = [$small $large $digit \']

@varid     = $small $idchar*
@conid     = $large $idchar*

@varsym    = $symbol $symchar*
@consym    = \: $symchar*

@decimal     = $decdigit+
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
"--"\-* [^$symbol :] .*			;
"--"\-* / { atEOL }			;

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
  ^\# \! .* \n				; -- #!, for scripts
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
<line_prag1> $decdigit+			{ setLine line_prag1a }
<line_prag1a> \" [$graphic \ ]* \"	{ setFile line_prag1b }
<line_prag1b> .*			{ pop }

-- Haskell-style line pragmas, of the form
--    {-# LINE <line> "<file>" #-}
<line_prag2> $decdigit+			{ setLine line_prag2a }
<line_prag2a> \" [$graphic \ ]* \"	{ setFile line_prag2b }
<line_prag2b> "#-}"|"-}"		{ pop }
   -- NOTE: accept -} at the end of a LINE pragma, for compatibility
   -- with older versions of GHC which generated these.

-- We only want RULES pragmas to be picked up when -fglasgow-exts
-- is on, because the contents of the pragma is always written using
-- glasgow-exts syntax (using forall etc.), so if glasgow exts are not
-- enabled, we're sure to get a parse error.
-- (ToDo: we should really emit a warning when ignoring pragmas)
<glaexts>
  "{-#" $whitechar* (RULES|rules)	{ token ITrules_prag }

<0,glaexts> {
  "{-#" $whitechar* (INLINE|inline)	{ token (ITinline_prag True) }
  "{-#" $whitechar* (NO(T?)INLINE|no(t?)inline)
  					{ token (ITinline_prag False) }
  "{-#" $whitechar* (SPECIALI[SZ]E|speciali[sz]e)
  					{ token ITspec_prag }
  "{-#" $whitechar* (SPECIALI[SZ]E|speciali[sz]e)
	$whitechar* (INLINE|inline)	{ token (ITspec_inline_prag True) }
  "{-#" $whitechar* (SPECIALI[SZ]E|speciali[sz]e)
	$whitechar* (NO(T?)INLINE|no(t?)inline)
					{ token (ITspec_inline_prag False) }
  "{-#" $whitechar* (SOURCE|source)	{ token ITsource_prag }
  "{-#" $whitechar* (DEPRECATED|deprecated)
  					{ token ITdeprecated_prag }
  "{-#" $whitechar* (SCC|scc)		{ token ITscc_prag }
  "{-#" $whitechar* (CORE|core)		{ token ITcore_prag }
  "{-#" $whitechar* (UNPACK|unpack)	{ token ITunpack_prag }
  
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
  "(|" / { ifExtension arrowsEnabled `alexAndPred` notFollowedBySymbol }
					{ special IToparenbar }
  "|)" / { ifExtension arrowsEnabled }  { special ITcparenbar }
}

<0,glaexts> {
  \? @varid / { ifExtension ipEnabled }	{ skip_one_varid ITdupipvarid }
  \% @varid / { ifExtension ipEnabled } { skip_one_varid ITsplitipvarid }
}

<glaexts> {
  "(#" / { notFollowedBySymbol }	{ token IToubxparen }
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

{
-- work around bug in Alex 2.0
#if __GLASGOW_HASKELL__ < 503
unsafeAt arr i = arr ! i
#endif

-- -----------------------------------------------------------------------------
-- The token type

data Token
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
  | ITstdcallconv
  | ITccallconv
  | ITdotnet
  | ITmdo

	-- Pragmas
  | ITinline_prag Bool		-- True <=> INLINE, False <=> NOINLINE
  | ITspec_prag			-- SPECIALISE	
  | ITspec_inline_prag Bool	-- SPECIALISE INLINE (or NOINLINE)
  | ITsource_prag
  | ITrules_prag
  | ITdeprecated_prag
  | ITline_prag
  | ITscc_prag
  | ITcore_prag                 -- hdaume: core annotations
  | ITunpack_prag
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

  -- MetaHaskell extension tokens
  | ITopenExpQuote  		--  [| or [e|
  | ITopenPatQuote		--  [p|
  | ITopenDecQuote		--  [d|
  | ITopenTypQuote		--  [t|         
  | ITcloseQuote		--  |]
  | ITidEscape   FastString	--  $x
  | ITparenEscape		--  $( 
  | ITvarQuote			--  '
  | ITtyQuote			--  ''

  -- Arrow notation extension
  | ITproc
  | ITrec
  | IToparenbar			--  (|
  | ITcparenbar			--  |)
  | ITlarrowtail		--  -<
  | ITrarrowtail		--  >-
  | ITLarrowtail		--  -<<
  | ITRarrowtail		--  >>-

  | ITunknown String		-- Used when the lexer can't make sense of it
  | ITeof			-- end of file token
#ifdef DEBUG
  deriving Show -- debugging
#endif

isSpecial :: Token -> Bool
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

      	( "forall",	ITforall,	 bit tvBit),
	( "mdo",	ITmdo,		 bit glaExtsBit),

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

	( "rec",	ITrec,		 bit arrowsBit),
	( "proc",	ITproc,		 bit arrowsBit)
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
       ,(".",	ITdot,		bit tvBit)	-- For 'forall a . t'

       ,("-<",	ITlarrowtail,	bit arrowsBit)
       ,(">-",	ITrarrowtail,	bit arrowsBit)
       ,("-<<",	ITLarrowtail,	bit arrowsBit)
       ,(">>-",	ITRarrowtail,	bit arrowsBit)

#if __GLASGOW_HASKELL__ >= 605
       ,("λ",	ITlam,          bit glaExtsBit)
       ,("∷",   ITdcolon,       bit glaExtsBit)
       ,("⇒",   ITdarrow,	bit glaExtsBit)
       ,("∀",	ITforall,	bit glaExtsBit)
       ,("→",   ITrarrow,	bit glaExtsBit)
       ,("←",   ITlarrow,	bit glaExtsBit)
       ,("⋯", 	ITdotdot,	bit glaExtsBit)
#endif
       ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = SrcSpan -> StringBuffer -> Int -> P (Located Token)

special :: Token -> Action
special tok span _buf len = return (L span tok)

token, layout_token :: Token -> Action
token t span buf len = return (L span t)
layout_token t span buf len = pushLexState layout >> return (L span t)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

skip_one_varid :: (FastString -> Token) -> Action
skip_one_varid f span buf len 
  = return (L span $! f (lexemeToFastString (stepOn buf) (len-1)))

strtoken :: (String -> Token) -> Action
strtoken f span buf len = 
  return (L span $! (f $! lexemeToString buf len))

init_strtoken :: Int -> (String -> Token) -> Action
-- like strtoken, but drops the last N character(s)
init_strtoken drop f span buf len = 
  return (L span $! (f $! lexemeToString buf (len-drop)))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = do popLexState; lexToken

pop_and :: Action -> Action
pop_and act span buf len = do popLexState; act span buf len

notFollowedBy char _ _ _ (AI _ _ buf) = atEnd buf || currentChar buf /= char

notFollowedBySymbol _ _ _ (AI _ _ buf)
  = atEnd buf || currentChar buf `notElem` "!#$%&*+./<=>?@\\^|-~"

atEOL _ _ _ (AI _ _ buf) = atEnd buf || currentChar buf == '\n'

ifExtension pred bits _ _ _ = pred bits

{-
  nested comments require traversing by hand, they can't be parsed
  using regular expressions.
-}
nested_comment :: Action
nested_comment span _str _len = do
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

        err (AI end _ _) = failLocMsgP (srcSpanStart span) end "unterminated `{-'"

open_brace, close_brace :: Action
open_brace span _str _len = do 
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L span ITocurly)
close_brace span _str _len = do 
  popContext
  return (L span ITccurly)

-- We have to be careful not to count M.<varid> as a qualified name
-- when <varid> is a keyword.  We hack around this by catching 
-- the offending tokens afterward, and re-lexing in a different state.
check_qvarid span buf len = do
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
	token     = L span (ITqvarid (mod,var))

	try_again = do
		(AI _ offs _) <- getInput	
		setInput (AI (srcSpanStart span) (offs-len) buf)
		pushLexState bad_qvarid
		lexToken

qvarid buf len = ITqvarid $! splitQualName buf len
qconid buf len = ITqconid $! splitQualName buf len

splitQualName :: StringBuffer -> Int -> (FastString,FastString)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
splitQualName orig_buf len = split orig_buf orig_buf
  where
    split buf dot_buf
	| orig_buf `byteDiff` buf >= len  = done dot_buf
	| c == '.'                	  = found_dot buf'
	| otherwise               	  = split buf' dot_buf
      where
       (c,buf') = nextChar buf
  
    -- careful, we might get names like M....
    -- so, if the character after the dot is not upper-case, this is
    -- the end of the qualifier part.
    found_dot buf -- buf points after the '.'
	| isUpper c    = split buf' buf
	| otherwise    = done buf
      where
       (c,buf') = nextChar buf

    done dot_buf =
	(lexemeToFastString orig_buf (qual_size - 1),
	 lexemeToFastString dot_buf (len - qual_size))
      where
	qual_size = orig_buf `byteDiff` dot_buf

varid span buf len = 
  case lookupUFM reservedWordsFM fs of
	Just (keyword,0)    -> do
		maybe_layout keyword
		return (L span keyword)
	Just (keyword,exts) -> do
		b <- extension (\i -> exts .&. i /= 0)
		if b then do maybe_layout keyword
			     return (L span keyword)
		     else return (L span (ITvarid fs))
	_other -> return (L span (ITvarid fs))
  where
	fs = lexemeToFastString buf len

conid buf len = ITconid fs
  where fs = lexemeToFastString buf len

qvarsym buf len = ITqvarsym $! splitQualName buf len
qconsym buf len = ITqconsym $! splitQualName buf len

varsym = sym ITvarsym
consym = sym ITconsym

sym con span buf len = 
  case lookupUFM reservedSymsFM fs of
	Just (keyword,0)    -> return (L span keyword)
	Just (keyword,exts) -> do
		b <- extension (\i -> exts .&. i /= 0)
		if b then return (L span keyword)
		     else return (L span $! con fs)
	_other -> return (L span $! con fs)
  where
	fs = lexemeToFastString buf len

tok_decimal span buf len 
  = return (L span (ITinteger  $! parseInteger buf len 10 octDecDigit))

tok_octal span buf len 
  = return (L span (ITinteger  $! parseInteger (offsetBytes 2 buf) (len-2) 8 octDecDigit))

tok_hexadecimal span buf len 
  = return (L span (ITinteger  $! parseInteger (offsetBytes 2 buf) (len-2) 16 hexDigit))

prim_decimal span buf len 
  = return (L span (ITprimint  $! parseInteger buf (len-1) 10 octDecDigit))

prim_octal span buf len 
  = return (L span (ITprimint  $! parseInteger (offsetBytes 2 buf) (len-3) 8 octDecDigit))

prim_hexadecimal span buf len 
  = return (L span (ITprimint  $! parseInteger (offsetBytes 2 buf) (len-3) 16 hexDigit))

tok_float        str = ITrational   $! readRational str
prim_float       str = ITprimfloat  $! readRational str
prim_double      str = ITprimdouble $! readRational str

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len = do
	pos <- getOffside
	case pos of
	    LT -> do
                --trace "layout: inserting '}'" $ do
		popContext
		-- do NOT pop the lex state, we might have a ';' to insert
		return (L span ITvccurly)
	    EQ -> do
                --trace "layout: inserting ';'" $ do
		popLexState
		return (L span ITsemi)
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
new_layout_context strict span _buf _len = do
    popLexState
    (AI _ offset _) <- getInput
    ctx <- getContext
    case ctx of
	Layout prev_off : _  | 
	   (strict     && prev_off >= offset  ||
	    not strict && prev_off > offset) -> do
		-- token is indented to the left of the previous context.
		-- we must generate a {} sequence now.
		pushLexState layout_left
		return (L span ITvocurly)
	other -> do
		setContext (Layout offset : ctx)
		return (L span ITvocurly)

do_layout_left span _buf _len = do
    popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- LINE pragmas

setLine :: Int -> Action
setLine code span buf len = do
  let line = parseInteger buf len 10 octDecDigit
  setSrcLoc (mkSrcLoc (srcSpanFile span) (fromIntegral line - 1) 0)
	-- subtract one: the line number refers to the *following* line
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
-- Strings & Chars

-- This stuff is horrible.  I hates it.

lex_string_tok :: Action
lex_string_tok span buf len = do
  tok <- lex_string ""
  end <- getSrcLoc 
  return (L (mkSrcSpan (srcSpanStart span) end) tok)

lex_string :: String -> P Token
lex_string s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error

    Just ('"',i)  -> do
	setInput i
	glaexts <- extension glaExtsEnabled
	if glaexts
	  then do
	    i <- getInput
	    case alexGetChar' i of
	      Just ('#',i) -> do
		   setInput i
		   if any (> '\xFF') s
                    then failMsgP "primitive string literal must contain only characters <= \'\\xFF\'"
                    else let s' = mkZFastString (reverse s) in
			 return (ITprimstring s')
			-- mkZFastString is a hack to avoid encoding the
			-- string in UTF-8.  We just want the exact bytes.
	      _other ->
		return (ITstring (mkFastString (reverse s)))
	  else
		return (ITstring (mkFastString (reverse s)))

    Just ('\\',i)
	| Just ('&',i) <- next -> do 
		setInput i; lex_string s
	| Just (c,i) <- next, is_space c -> do 
		setInput i; lex_stringgap s
	where next = alexGetChar' i

    Just (c, i) -> do
	c' <- lex_char c i
	lex_string (c':s)

lex_stringgap s = do
  c <- getCharOrFail
  case c of
    '\\' -> lex_string s
    c | is_space c -> lex_stringgap s
    _other -> lit_error


lex_char_tok :: Action
-- Here we are basically parsing character literals, such as 'x' or '\n'
-- but, when Template Haskell is on, we additionally spot
-- 'x and ''T, returning ITvarQuote and ITtyQuote respectively, 
-- but WIHTOUT CONSUMING the x or T part  (the parser does that).
-- So we have to do two characters of lookahead: when we see 'x we need to
-- see if there's a trailing quote
lex_char_tok span buf len = do	-- We've seen '
   i1 <- getInput	-- Look ahead to first character
   let loc = srcSpanStart span
   case alexGetChar' i1 of
	Nothing -> lit_error 

	Just ('\'', i2@(AI end2 _ _)) -> do 	-- We've seen ''
		  th_exts <- extension thEnabled
		  if th_exts then do
			setInput i2
			return (L (mkSrcSpan loc end2)  ITtyQuote)
		   else lit_error

	Just ('\\', i2@(AI end2 _ _)) -> do 	-- We've seen 'backslash 
		  setInput i2
		  lit_ch <- lex_escape
		  mc <- getCharOrFail	-- Trailing quote
		  if mc == '\'' then finish_char_tok loc lit_ch
			        else do setInput i2; lit_error 

        Just (c, i2@(AI end2 _ _)) 
		| not (isAny c) -> lit_error
		| otherwise ->

		-- We've seen 'x, where x is a valid character
		--  (i.e. not newline etc) but not a quote or backslash
	   case alexGetChar' i2 of	-- Look ahead one more character
		Nothing -> lit_error
		Just ('\'', i3) -> do 	-- We've seen 'x'
			setInput i3 
			finish_char_tok loc c
		_other -> do 		-- We've seen 'x not followed by quote
					-- If TH is on, just parse the quote only
			th_exts <- extension thEnabled	
			let (AI end _ _) = i1
			if th_exts then return (L (mkSrcSpan loc end) ITvarQuote)
				   else do setInput i2; lit_error

finish_char_tok :: SrcLoc -> Char -> P (Located Token)
finish_char_tok loc ch	-- We've already seen the closing quote
			-- Just need to check for trailing #
  = do	glaexts <- extension glaExtsEnabled
	i@(AI end _ _) <- getInput
	if glaexts then do
		case alexGetChar' i of
			Just ('#',i@(AI end _ _)) -> do
				setInput i
				return (L (mkSrcSpan loc end) (ITprimchar ch))
			_other ->
				return (L (mkSrcSpan loc end) (ITchar ch))
	        else do
		   return (L (mkSrcSpan loc end) (ITchar ch))

lex_char :: Char -> AlexInput -> P Char
lex_char c inp = do
  case c of
      '\\' -> do setInput inp; lex_escape
      c | isAny c -> do setInput inp; return c
      _other -> lit_error

isAny c | c > '\xff' = isPrint c
	| otherwise  = is_any c

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

	'x'   -> readNum is_hexdigit 16 hexDigit
	'o'   -> readNum is_octdigit  8 octDecDigit
	x | is_digit x -> readNum2 is_digit 10 octDecDigit (octDecDigit x)

	c1 ->  do
	   i <- getInput
	   case alexGetChar' i of
	    Nothing -> lit_error
	    Just (c2,i2) -> 
              case alexGetChar' i2 of
		Nothing	-> do setInput i2; lit_error
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
  i <- getInput
  c <- getCharOrFail
  if is_digit c 
	then readNum2 is_digit base conv (conv c)
	else do setInput i; lit_error

readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
	  case alexGetChar' input of
	    Just (c,input') | is_digit c -> do
		read (i*base + conv c) input'
	    _other -> do
		if i >= 0 && i <= 0x10FFFF
		   then do setInput input; return (chr i)
		   else lit_error

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

-- before calling lit_error, ensure that the current input is pointing to
-- the position of the error in the buffer.  This is so that we can report
-- a correct location to the user, but also so we can detect UTF-8 decoding
-- errors if they occur.
lit_error = lexError "lexical error in string/character literal"

getCharOrFail :: P Char
getCharOrFail =  do
  i <- getInput
  case alexGetChar' i of
	Nothing -> lexError "unexpected end-of-file in string/character literal"
	Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------
-- The Parse Monad

data LayoutContext
  = NoLayout
  | Layout !Int

data ParseResult a
  = POk PState a
  | PFailed 
	SrcSpan		-- The start and end of the text span related to
			-- the error.  Might be used in environments which can 
			-- show this span, e.g. by highlighting it.
	Message		-- The error message

data PState = PState { 
	buffer	   :: StringBuffer,
        last_loc   :: SrcSpan,	-- pos of previous token
        last_offs  :: !Int, 	-- offset of the previous token from the
				-- beginning of  the current line.
				-- \t is equal to 8 spaces.
	last_len   :: !Int,	-- len of previous token
        loc        :: SrcLoc,   -- current loc (end of prev token + 1)
	extsBitmap :: !Int,	-- bitmap that determines permitted extensions
	context	   :: [LayoutContext],
	lex_state  :: [Int]
     }
	-- last_loc and last_len are used when generating error messages,
	-- and in pushCurrentContext only.  Sigh, if only Happy passed the
	-- current token to happyError, we could at least get rid of last_len.
	-- Getting rid of last_loc would require finding another way to 
	-- implement pushCurrentContext (which is only called from one place).

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
		POk s1 a         -> (unP (k a)) s1
		PFailed span err -> PFailed span err

failP :: String -> P a
failP msg = P $ \s -> PFailed (last_loc s) (text msg)

failMsgP :: String -> P a
failMsgP msg = P $ \s -> PFailed (last_loc s) (text msg)

failLocMsgP :: SrcLoc -> SrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \s -> PFailed (mkSrcSpan loc1 loc2) (text str)

failSpanMsgP :: SrcSpan -> String -> P a
failSpanMsgP span msg = P $ \s -> PFailed span (text msg)

extension :: (Int -> Bool) -> P Bool
extension p = P $ \s -> POk s (p $! extsBitmap s)

getExts :: P Int
getExts = P $ \s -> POk s (extsBitmap s)

setSrcLoc :: SrcLoc -> P ()
setSrcLoc new_loc = P $ \s -> POk s{loc=new_loc} ()

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

setLastToken :: SrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s{ last_loc=loc, last_len=len } ()

data AlexInput = AI SrcLoc {-#UNPACK#-}!Int StringBuffer

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ _ buf) = prevChar buf '\n'

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AI loc ofs s) 
  | atEnd s   = Nothing
  | otherwise = adj_c `seq` loc' `seq` ofs' `seq` s' `seq` 
		Just (adj_c, (AI loc' ofs' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        ofs'   = advanceOffs c ofs

	non_graphic     = '\x0'
	upper 		= '\x1'
	lower		= '\x2'
	digit		= '\x3'
	symbol		= '\x4'
	space		= '\x5'
	other_graphic	= '\x6'

	adj_c 
	  | c <= '\x06' = non_graphic
	  | c <= '\xff' = c
	  | otherwise = 
		case generalCategory c of
		  UppercaseLetter       -> upper
		  LowercaseLetter       -> lower
		  TitlecaseLetter       -> upper
		  ModifierLetter        -> other_graphic
		  OtherLetter           -> other_graphic
		  NonSpacingMark        -> other_graphic
		  SpacingCombiningMark  -> other_graphic
		  EnclosingMark         -> other_graphic
		  DecimalNumber         -> digit
		  LetterNumber          -> other_graphic
		  OtherNumber           -> other_graphic
		  ConnectorPunctuation  -> other_graphic
		  DashPunctuation       -> other_graphic
		  OpenPunctuation       -> other_graphic
		  ClosePunctuation      -> other_graphic
		  InitialQuote          -> other_graphic
		  FinalQuote            -> other_graphic
		  OtherPunctuation      -> other_graphic
		  MathSymbol            -> symbol
		  CurrencySymbol        -> symbol
		  ModifierSymbol        -> symbol
		  OtherSymbol           -> symbol
		  Space                 -> space
		  _other		-> non_graphic

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc ofs s) 
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` ofs' `seq` s' `seq` 
		Just (c, (AI loc' ofs' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        ofs'   = advanceOffs c ofs

advanceOffs :: Char -> Int -> Int
advanceOffs '\n' offs = 0
advanceOffs '\t' offs = (offs `quot` 8 + 1) * 8
advanceOffs _    offs = offs + 1

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, last_offs=o, buffer=b } -> POk s (AI l o b)

setInput :: AlexInput -> P ()
setInput (AI l o b) = P $ \s -> POk s{ loc=l, last_offs=o, buffer=b } ()

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
arrowsBit  = 4
thBit	   = 5
ipBit      = 6
tvBit	   = 7	-- Scoped type variables enables 'forall' keyword
bangPatBit = 8	-- Tells the parser to understand bang-patterns
		-- (doesn't affect the lexer)

glaExtsEnabled, ffiEnabled, parrEnabled :: Int -> Bool
glaExtsEnabled flags = testBit flags glaExtsBit
ffiEnabled     flags = testBit flags ffiBit
parrEnabled    flags = testBit flags parrBit
arrowsEnabled  flags = testBit flags arrowsBit
thEnabled      flags = testBit flags thBit
ipEnabled      flags = testBit flags ipBit
tvEnabled      flags = testBit flags tvBit
bangPatEnabled flags = testBit flags bangPatBit

-- create a parse state
--
mkPState :: StringBuffer -> SrcLoc -> DynFlags -> PState
mkPState buf loc flags  = 
  PState {
      buffer	 = buf,
      last_loc   = mkSrcSpan loc loc,
      last_offs  = 0,
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
	       .|. parrBit    `setBitIf` dopt Opt_PArr        flags
	       .|. arrowsBit  `setBitIf` dopt Opt_Arrows      flags
	       .|. thBit      `setBitIf` dopt Opt_TH          flags
	       .|. ipBit      `setBitIf` dopt Opt_ImplicitParams flags
	       .|. tvBit      `setBitIf` dopt Opt_ScopedTypeVariables flags
	       .|. bangPatBit `setBitIf` dopt Opt_BangPatterns flags
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
	[]     -> PFailed last_loc (srcParseErr buf len)

-- Push a new layout context at the indentation of the last token read.
-- This is only used at the outer level of a module when the 'module'
-- keyword is missing.
pushCurrentContext :: P ()
pushCurrentContext = P $ \ s@PState{ last_offs=offs, last_len=len, context=ctx } ->
  POk s{context = Layout (offs-len) : ctx} ()

getOffside :: P Ordering
getOffside = P $ \s@PState{last_offs=offs, context=stk} ->
		let ord = case stk of
			(Layout n:_) -> compare offs n
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
  where token = lexemeToString (offsetBytes (-len) buf) len

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \PState{ buffer = buf, last_len = len, 	
			    last_loc = last_loc } ->
    PFailed last_loc (srcParseErr buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: String -> P a
lexError str = do
  loc <- getSrcLoc
  i@(AI end _ buf) <- getInput
  reportLexError loc end buf str

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  tok@(L _ tok__) <- lexToken
  --trace ("token: " ++ show tok__) $ do
  cont tok

lexToken :: P (Located Token)
lexToken = do
  inp@(AI loc1 _ buf) <- getInput
  sc <- getLexState
  exts <- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do let span = mkSrcSpan loc1 loc1
		  setLastToken span 0
		  return (L span ITeof)
    AlexError (AI loc2 _ buf) -> do 
	reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
	setInput inp2
	lexToken
    AlexToken inp2@(AI end _ buf2) len t -> do
	setInput inp2
	let span = mkSrcSpan loc1 end
	let bytes = byteDiff buf buf2
	span `seq` setLastToken span bytes
	t span buf bytes

-- ToDo: Alex reports the buffer at the start of the erroneous lexeme,
-- but it would be more informative to report the location where the
-- error was actually discovered, especially if this is a decoding
-- error.
reportLexError loc1 loc2 buf str = 
  let 
	c = fst (nextChar buf)
  in
  if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
    then failLocMsgP loc2 loc2 "UTF-8 decoding error"
    else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)
}
