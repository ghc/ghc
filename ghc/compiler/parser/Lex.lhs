%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Lexical analysis]{Lexical analysis}

--------------------------------------------------------
[Jan 98]
There's a known bug in here:

	If an interface file ends prematurely, Lex tries to
	do headFS of an empty FastString.

An example that provokes the error is

	f _:_ _forall_ [a] <<<END OF FILE>>>
--------------------------------------------------------

\begin{code}

module Lex (

	ifaceParseErr, srcParseErr,

	-- Monad for parser
	Token(..), lexer, ParseResult(..), PState(..),
	checkVersion, 
	StringBuffer,

	P, thenP, thenP_, returnP, mapP, failP, failMsgP,
	getSrcLocP, getSrcFile,
	layoutOn, layoutOff, pushContext, popContext
    ) where

#include "HsVersions.h"

import Char 		( ord, isSpace, toUpper )
import List             ( isSuffixOf )

import IdInfo		( InlinePragInfo(..), CprInfo(..) )
import Name		( isLowerISO, isUpperISO )
import PrelMods		( mkTupNameStr, mkUbxTupNameStr )
import CmdLineOpts	( opt_IgnoreIfacePragmas, opt_HiVersion, opt_NoHiCheck )
import Demand		( Demand(..) {- instance Read -} )
import UniqFM           ( UniqFM, listToUFM, lookupUFM)
import BasicTypes	( NewOrData(..) )
import SrcLoc		( SrcLoc, incSrcLine, srcLocFile, srcLocLine,
			  replaceSrcLine, mkSrcLoc )

import Maybes		( MaybeErr(..) )
import ErrUtils		( Message )
import Outputable

import FastString
import StringBuffer
import GlaExts
import ST		( runST )
import Ctype
import Char		( chr )
import Addr
import PrelRead 	( readRational__ ) -- Glasgow non-std
\end{code}

%************************************************************************
%*									*
\subsection{Data types}
%*									*
%************************************************************************

The token data type, fairly un-interesting except from one
constructor, @ITidinfo@, which is used to lazily lex id info (arity,
strictness, unfolding etc).

The Idea/Observation here is that the renamer needs to scan through
all of an interface file before it can continue. But only a fraction
of the information contained in the file turns out to be useful, so
delaying as much as possible of the scanning and parsing of an
interface file Makes Sense (Heap profiles of the compiler 
show a reduction in heap usage by at least a factor of two,
post-renamer). 

Hence, the interface file lexer spots when value declarations are
being scanned and return the @ITidinfo@ and @ITtype@ constructors
for the type and any other id info for that binding (unfolding, strictness
etc). These constructors are applied to the result of lexing these sub-chunks.

The lexing of the type and id info is all done lazily, of course, so
the scanning (and subsequent parsing) will be done *only* on the ids the
renamer finds out that it is interested in. The rest will just be junked.
Laziness, you know it makes sense :-)

\begin{code}
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
  | ITscc

  | ITforall			-- GHC extension keywords
  | ITforeign
  | ITexport
  | ITlabel
  | ITdynamic
  | ITunsafe
  | ITwith
  | ITstdcallconv
  | ITccallconv

  | ITinterface			-- interface keywords
  | IT__export
  | ITdepends
  | IT__forall
  | ITletrec 
  | ITcoerce
  | ITinlineMe
  | ITinlineCall
  | ITccall (Bool,Bool,Bool)	-- (is_dyn, is_casm, may_gc)
  | ITdefaultbranch
  | ITbottom
  | ITinteger_lit 
  | ITfloat_lit
  | ITword_lit
  | ITword64_lit
  | ITint64_lit
  | ITrational_lit
  | ITaddr_lit
  | ITlit_lit
  | ITstring_lit
  | ITtypeapp
  | ITusage
  | ITfuall
  | ITarity 
  | ITspecialise
  | ITnocaf
  | ITunfold InlinePragInfo
  | ITstrict ([Demand], Bool)
  | ITrules
  | ITcprinfo
  | ITdeprecated
  | IT__scc
  | ITsccAllCafs

  | ITspecialise_prag		-- Pragmas
  | ITsource_prag
  | ITinline_prag
  | ITnoinline_prag
  | ITrules_prag
  | ITdeprecated_prag
  | ITline_prag
  | ITclose_prag

  | ITdotdot  			-- reserved symbols
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
  | ITdot

  | ITbiglam			-- GHC-extension symbols

  | ITocurly  			-- special symbols
  | ITccurly
  | ITvccurly
  | ITobrack
  | ITcbrack
  | IToparen
  | ITcparen
  | IToubxparen
  | ITcubxparen
  | ITsemi
  | ITcomma
  | ITunderscore
  | ITbackquote

  | ITvarid   FAST_STRING	-- identifiers
  | ITconid   FAST_STRING
  | ITvarsym  FAST_STRING
  | ITconsym  FAST_STRING
  | ITqvarid  (FAST_STRING,FAST_STRING)
  | ITqconid  (FAST_STRING,FAST_STRING)
  | ITqvarsym (FAST_STRING,FAST_STRING)
  | ITqconsym (FAST_STRING,FAST_STRING)

  | ITipvarid FAST_STRING	-- GHC extension: implicit param: ?x

  | ITpragma StringBuffer

  | ITchar       Char 
  | ITstring     FAST_STRING
  | ITinteger    Integer 
  | ITrational   Rational

  | ITprimchar   Char
  | ITprimstring FAST_STRING
  | ITprimint    Integer
  | ITprimfloat  Rational
  | ITprimdouble Rational
  | ITlitlit     FAST_STRING

  | ITunknown String		-- Used when the lexer can't make sense of it
  | ITeof			-- end of file token
  deriving Show -- debugging
\end{code}

-----------------------------------------------------------------------------
Keyword Lists

\begin{code}
pragmaKeywordsFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
       [( "SPECIALISE", ITspecialise_prag ),
	( "SPECIALIZE", ITspecialise_prag ),
	( "SOURCE",	ITsource_prag ),
	( "INLINE",     ITinline_prag ),
	( "NOINLINE",   ITnoinline_prag ),
	( "NOTINLINE",	ITnoinline_prag ),
	( "LINE",       ITline_prag ),
	( "RULES",	ITrules_prag ),
	( "RULEZ",	ITrules_prag ),	-- american spelling :-)
	( "DEPRECATED",	ITdeprecated_prag )
 	]

haskellKeywordsFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
       [( "_",		ITunderscore ),
	( "as",		ITas ),
	( "case",	ITcase ),     
	( "class",	ITclass ),    
	( "data",	ITdata ),     
	( "default",	ITdefault ),  
	( "deriving",	ITderiving ), 
	( "do",		ITdo ),       
	( "else",	ITelse ),     
	( "hiding",	IThiding ),
	( "if",		ITif ),       
	( "import",	ITimport ),   
	( "in",		ITin ),       
	( "infix",	ITinfix ),    
	( "infixl",	ITinfixl ),   
	( "infixr",	ITinfixr ),   
	( "instance",	ITinstance ), 
	( "let",	ITlet ),      
	( "module",	ITmodule ),   
	( "newtype",	ITnewtype ),  
	( "of",		ITof ),       
	( "qualified",	ITqualified ),
	( "then",	ITthen ),     
	( "type",	ITtype ),     
	( "where",	ITwhere ),
	( "_scc_",	ITscc )
     ]

-- IMPORTANT: Keep this in synch with ParseIface.y's var_fs production! (SUP)
ghcExtensionKeywordsFM = listToUFM $
	map (\ (x,y) -> (_PK_ x,y))
     [	( "forall",	ITforall ),
	( "foreign",	ITforeign ),
	( "export",	ITexport ),
	( "label",	ITlabel ),
	( "dynamic",	ITdynamic ),
	( "unsafe",	ITunsafe ),
	( "with",	ITwith ),
	( "stdcall",    ITstdcallconv),
	( "ccall",      ITccallconv),
        ("_ccall_",	ITccall (False, False, False)),
        ("_ccall_GC_",	ITccall (False, False, True)),
        ("_casm_",	ITccall (False, True,  False)),
        ("_casm_GC_",	ITccall (False, True,  True)),

	-- interface keywords
        ("__interface",		ITinterface),
	("__export",		IT__export),
	("__depends",		ITdepends),
	("__forall",		IT__forall),
	("__letrec",		ITletrec),
	("__coerce",		ITcoerce),
	("__inline_me",		ITinlineMe),
	("__inline_call",	ITinlineCall),
	("__depends",		ITdepends),
	("__DEFAULT",		ITdefaultbranch),
	("__bot",		ITbottom),
	("__integer",		ITinteger_lit),
	("__float",		ITfloat_lit),
	("__int64",		ITint64_lit),
	("__word",		ITword_lit),
	("__word64",		ITword64_lit),
	("__rational",		ITrational_lit),
	("__addr",		ITaddr_lit),
	("__litlit",		ITlit_lit),
	("__string",		ITstring_lit),
	("__a",			ITtypeapp),
	("__u",			ITusage),
	("__fuall",		ITfuall),
	("__A",			ITarity),
	("__P",			ITspecialise),
	("__C",			ITnocaf),
	("__R",			ITrules),
        ("__D",			ITdeprecated),
        ("__U",			ITunfold NoInlinePragInfo),
	
        ("__ccall",		ITccall (False, False, False)),
        ("__ccall_GC",		ITccall (False, False, True)),
        ("__dyn_ccall",		ITccall (True,  False, False)),
        ("__dyn_ccall_GC",	ITccall (True,  False, True)),
        ("__casm",		ITccall (False, True,  False)),
        ("__dyn_casm",		ITccall (True,  True,  False)),
        ("__casm_GC",		ITccall (False, True,  True)),
        ("__dyn_casm_GC",	ITccall (True,  True,  True)),

        ("/\\",			ITbiglam)
     ]


haskellKeySymsFM = listToUFM $
	map (\ (x,y) -> (_PK_ x,y))
      [ ("..",		ITdotdot)
       ,("::",		ITdcolon)
       ,("=",		ITequal)
       ,("\\",		ITlam)
       ,("|",		ITvbar)
       ,("<-",		ITlarrow)
       ,("->",		ITrarrow)
       ,("@",		ITat)
       ,("~",		ITtilde)
       ,("=>",		ITdarrow)
       ,("-",		ITminus)
       ,("!",		ITbang)
       ,(".",		ITdot)		-- sadly, for 'forall a . t'
       ]
\end{code}

-----------------------------------------------------------------------------
The lexical analyser

Lexer state:

	- (glaexts) lexing an interface file or -fglasgow-exts
	- (bol)   pointer to beginning of line (for column calculations)
	- (buf)   pointer to beginning of token
	- (buf)   pointer to current char
	- (atbol) flag indicating whether we're at the beginning of a line

\begin{code}
lexer :: (Token -> P a) -> P a
lexer cont buf s@(PState{
		    loc = loc,
		    glasgow_exts = glaexts,
		    bol = bol,
		    atbol = atbol,
		    context = ctx
		})

	-- first, start a new lexeme and lose all the whitespace
  =  _scc_ "Lexer" 
  tab line bol atbol (stepOverLexeme buf)
  where
	line = srcLocLine loc

	tab y bol atbol buf = --trace ("tab: " ++ show (I# y) ++ " : " ++ show (currentChar buf)) $
	  case currentChar# buf of

	    '\NUL'# ->
	 	   if bufferExhausted (stepOn buf)
	       		then cont ITeof buf s'
			else trace "lexer: misplaced NUL?" $ 
			     tab y bol atbol (stepOn buf)

	    '\n'# -> let buf' = stepOn buf
		     in tab (y +# 1#) (currentIndex# buf') 1# buf'

		-- find comments.  This got harder in Haskell 98.
	    '-'# ->  let trundle n = 
			  let next = lookAhead# buf n in
			  if next `eqChar#` '-'# then trundle (n +# 1#)
			  else if is_symbol next || n <# 2#
				then is_a_token
			        else case untilChar# (stepOnBy# buf n) '\n'# of 
				    { buf' -> tab y bol atbol (stepOverLexeme buf')
				    }
		    in trundle 1#

		-- comments and pragmas.  We deal with LINE pragmas here,
		-- and throw out any unrecognised pragmas as comments.  Any
		-- pragmas we know about are dealt with later (after any layout
		-- processing if necessary).

	    '{'# | lookAhead# buf 1# `eqChar#` '-'# ->
		if lookAhead# buf 2# `eqChar#` '#'# then
		  if lookAhead# buf 3# `eqChar#` '#'# then is_a_token else
		  case expandWhile# is_space (setCurrentPos# buf 3#) of { buf1->
		  case expandWhile# is_ident (stepOverLexeme buf1)   of { buf2->
		  let lexeme = mkFastString -- ToDo: too slow
			          (map toUpper (lexemeToString buf2)) in
		  case lookupUFM pragmaKeywordsFM lexeme of
			Just ITline_prag -> line_prag (lexer cont) buf2 s'
			Just other -> is_a_token
			Nothing -> skip_to_end (stepOnBy# buf 2#)
		  }}
		
		else skip_to_end (stepOnBy# buf 2#)
		where
		    skip_to_end buf = nested_comment (lexer cont) buf s'

		-- tabs have been expanded beforehand
	    c | is_space c -> tab y bol atbol (stepOn buf)
	      | otherwise  -> is_a_token

	   where s' = s{loc = replaceSrcLine loc y, 
		        bol = bol,
		       atbol = atbol}

	      	 is_a_token | atbol /=# 0# = lexBOL cont buf s'
	      		    | otherwise    = lexToken cont glaexts buf s'

-- {-# LINE .. #-} pragmas.  yeuch.
line_prag cont buf =
  case expandWhile# is_space buf 		of { buf1 ->
  case scanNumLit 0 (stepOverLexeme buf1) 	of { (line,buf2) ->
  -- subtract one: the line number refers to the *following* line.
  let real_line = line - 1 in
  case fromInteger real_line 			of { i@(I# l) -> 
  case expandWhile# is_space buf2 		of { buf3 ->
  case currentChar# buf3 			of
     '\"'#{-"-} -> 
	case untilEndOfString# (stepOn (stepOverLexeme buf3)) of { buf4 ->
	let file = lexemeToFastString buf4 in
	\s@PState{loc=loc} -> skipToEnd buf4 s{loc = mkSrcLoc file i}
	}
     other -> \s@PState{loc=loc} -> skipToEnd buf3 s{loc = replaceSrcLine loc l}
  }}}}
  where
	skipToEnd buf = nested_comment cont buf

nested_comment :: P a -> P a
nested_comment cont buf = loop buf
 where
   loop buf = 
     case currentChar# buf of
	'\NUL'# | bufferExhausted (stepOn buf) -> 
		lexError "unterminated `{-'" buf

	'-'# | lookAhead# buf 1# `eqChar#` '}'# ->
		cont (stepOnBy# buf 2#)

	'{'# | lookAhead# buf 1# `eqChar#` '-'# ->
	      nested_comment (nested_comment cont) (stepOnBy# buf 2#)

	'\n'# -> \ s@PState{loc=loc} ->
		 let buf' = stepOn buf in
		 nested_comment cont buf'
			s{loc = incSrcLine loc, bol = currentIndex# buf',
			  atbol = 1#}

	_   -> nested_comment cont (stepOn buf)

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: (Token -> P a) -> P a
lexBOL cont buf s@(PState{
		    loc = loc,
		    glasgow_exts = glaexts,
		    bol = bol,
		    atbol = atbol,
		    context = ctx
		  }) =
	if need_close_curly then 
	        --trace ("col = " ++ show (I# col) ++ ", layout: inserting '}'") $
		cont ITvccurly buf s{atbol = 1#, context = tail ctx}
	else if need_semi_colon then
	        --trace ("col = " ++ show (I# col) ++ ", layout: inserting ';'") $
		cont ITsemi buf s{atbol = 0#}
	else
		lexToken cont glaexts buf s{atbol = 0#}
  where
	col = currentIndex# buf -# bol

	need_close_curly =
		case ctx of
			[] -> False
			(i:_) -> case i of
				    NoLayout -> False
				    Layout n -> col <# n
	need_semi_colon =
		case ctx of
			[] -> False
			(i:_) -> case i of
				    NoLayout -> False
				    Layout n -> col ==# n


lexToken :: (Token -> P a) -> Int# -> P a
lexToken cont glaexts buf =
 --trace "lexToken" $
  case currentChar# buf of

    -- special symbols ----------------------------------------------------
    '('# | flag glaexts && lookAhead# buf 1# `eqChar#` '#'# 
		-> cont IToubxparen (setCurrentPos# buf 2#)
	 | otherwise
		-> cont IToparen (incLexeme buf)

    ')'# -> cont ITcparen    (incLexeme buf)
    '['# -> cont ITobrack    (incLexeme buf)
    ']'# -> cont ITcbrack    (incLexeme buf)
    ','# -> cont ITcomma     (incLexeme buf)
    ';'# -> cont ITsemi      (incLexeme buf)

    '}'# -> \ s@PState{context = ctx} ->
	    case ctx of	
		(_:ctx') -> cont ITccurly (incLexeme buf) s{context=ctx'}
		_  	 -> lexError "too many '}'s" buf s

    '#'# -> case lookAhead# buf 1# of
		')'#  | flag glaexts -> cont ITcubxparen (setCurrentPos# buf 2#)
		'-'# -> case lookAhead# buf 2# of
			   '}'# -> cont ITclose_prag (setCurrentPos# buf 3#)
			   _    -> lex_sym cont (incLexeme buf)
		_    -> lex_sym cont (incLexeme buf)

    '`'# | flag glaexts && lookAhead# buf 1# `eqChar#` '`'#
		-> lex_cstring cont (setCurrentPos# buf 2#)
	 | otherwise
	   	-> cont ITbackquote (incLexeme buf)

    '{'# ->	-- look for "{-##" special iface pragma
	case lookAhead# buf 1# of
	   '-'# -> case lookAhead# buf 2# of
		    '#'# -> case lookAhead# buf 3# of
				'#'# ->  
				   let (lexeme, buf') 
					  = doDiscard False (stepOnBy# (stepOverLexeme buf) 4#) in
				   cont (ITpragma lexeme) buf'
				_ -> lex_prag cont (setCurrentPos# buf 3#)
	   	    _    -> cont ITocurly (incLexeme buf)
	   _ -> (layoutOff `thenP_` cont ITocurly)  (incLexeme buf)

    -- strings/characters -------------------------------------------------
    '\"'#{-"-} -> lex_string cont glaexts "" (incLexeme buf)
    '\''#      -> lex_char (char_end cont) glaexts (incLexeme buf)

    -- strictness and cpr pragmas and __scc treated specially.
    '_'# | flag glaexts ->
	 case lookAhead# buf 1# of
	   '_'# -> case lookAhead# buf 2# of
	   	    'S'# -> 
			lex_demand cont (stepOnUntil (not . isSpace) 
		                        (stepOnBy# buf 3#)) -- past __S
	   	    'M'# -> 
			cont ITcprinfo (stepOnBy# buf 3#)	-- past __M

	   	    's'# -> 
			case prefixMatch (stepOnBy# buf 3#) "cc" of
		               Just buf' -> lex_scc cont (stepOverLexeme buf')
		     	       Nothing   -> lex_id cont glaexts buf
		    _ -> lex_id cont glaexts buf
	   _    -> lex_id cont glaexts buf

	-- Hexadecimal and octal constants
    '0'# | (ch `eqChar#` 'x'# || ch `eqChar#` 'X'#) && is_hexdigit ch2
		-> readNum (after_lexnum cont glaexts) buf' is_hexdigit 16 hex
	 | (ch `eqChar#` 'o'# || ch `eqChar#` 'O'#) && is_octdigit ch2
		-> readNum (after_lexnum cont glaexts) buf' is_octdigit  8 oct_or_dec
	where ch   = lookAhead# buf 1#
	      ch2  = lookAhead# buf 2#
	      buf' = setCurrentPos# buf 2#

    '\NUL'# ->
	    if bufferExhausted (stepOn buf) then
	       cont ITeof buf
	    else
	       trace "lexIface: misplaced NUL?" $ 
	       cont (ITunknown "\NUL") (stepOn buf)

    '?'# | flag glaexts && is_lower (lookAhead# buf 1#) ->
	    lex_ip cont (incLexeme buf)
    c | is_digit  c -> lex_num cont glaexts 0 buf
      | is_symbol c -> lex_sym cont buf
      | is_upper  c -> lex_con cont glaexts buf
      | is_ident  c -> lex_id  cont glaexts buf
      | otherwise   -> lexError "illegal character" buf

-- Int# is unlifted, and therefore faster than Bool for flags.
{-# INLINE flag #-}
flag :: Int# -> Bool
flag 0# = False
flag _  = True

-------------------------------------------------------------------------------
-- Pragmas

lex_prag cont buf
  = case expandWhile# is_space buf of { buf1 ->
    case expandWhile# is_ident (stepOverLexeme buf1) of { buf2 -> 
    let lexeme = mkFastString (map toUpper (lexemeToString buf2)) in
    case lookupUFM pragmaKeywordsFM lexeme of
	Just kw -> cont kw (mergeLexemes buf buf2)
	Nothing -> panic "lex_prag"
  }}

-------------------------------------------------------------------------------
-- Strings & Chars

lex_string cont glaexts s buf
  = case currentChar# buf of
	'"'#{-"-} -> 
	   let buf' = incLexeme buf; s' = mkFastString (reverse s) in
	   case currentChar# buf' of
		'#'# | flag glaexts -> cont (ITprimstring s') (incLexeme buf')
		_                   -> cont (ITstring s') buf'

	-- ignore \& in a string, deal with string gaps
	'\\'# | next_ch `eqChar#` '&'# 
		-> lex_string cont glaexts s buf'
	      | is_space next_ch
		-> lex_stringgap cont glaexts s (incLexeme buf)

	    where next_ch = lookAhead# buf 1#
		  buf' = setCurrentPos# buf 2#

	_ -> lex_char (lex_next_string cont s) glaexts buf

lex_stringgap cont glaexts s buf
  = let buf' = incLexeme buf in
    case currentChar# buf of
	'\n'# -> \st@PState{loc = loc} -> lex_stringgap cont glaexts s buf' 
		  st{loc = incSrcLine loc}
	'\\'# -> lex_string cont glaexts s buf'
	c | is_space c -> lex_stringgap cont glaexts s buf'
	other -> charError buf'

lex_next_string cont s glaexts c buf = lex_string cont glaexts (c:s) buf

lex_char :: (Int# -> Char -> P a) -> Int# -> P a
lex_char cont glaexts buf
  = case currentChar# buf of
	'\\'# -> lex_escape (cont glaexts) (incLexeme buf)
	c | is_any c -> cont glaexts (C# c) (incLexeme buf)
	other -> charError buf

char_end cont glaexts c buf
  = case currentChar# buf of
	'\''# -> let buf' = incLexeme buf in
		 case currentChar# buf' of
			'#'# | flag glaexts 
				-> cont (ITprimchar c) (incLexeme buf')
			_   	-> cont (ITchar c) buf'
	_     -> charError buf

lex_escape cont buf
  = let buf' = incLexeme buf in
    case currentChar# buf of
	'a'#	   -> cont '\a' buf'
	'b'#	   -> cont '\b' buf'
	'f'#	   -> cont '\f' buf'
	'n'#	   -> cont '\n' buf'
	'r'#	   -> cont '\r' buf'
	't'#	   -> cont '\t' buf'
	'v'#	   -> cont '\v' buf'
	'\\'#      -> cont '\\' buf'
	'"'#       -> cont '\"' buf'
	'\''#      -> cont '\'' buf'
	'^'#	   -> let c = currentChar# buf' in
		      if c `geChar#` '@'# && c `leChar#` '_'#
			then cont (C# (chr# (ord# c -# ord# '@'#))) (incLexeme buf')
			else charError buf'

	'x'#      -> readNum (after_charnum cont) buf' is_hexdigit 16 hex
	'o'#      -> readNum (after_charnum cont) buf' is_octdigit  8 oct_or_dec
	x | is_digit x 
		  -> readNum (after_charnum cont) buf is_digit    10 oct_or_dec

	_          -> case [ (c,buf2) | (p,c) <- silly_escape_chars,
				       Just buf2 <- [prefixMatch buf p] ] of
			    (c,buf2):_ -> cont c buf2
			    [] -> charError buf'

after_charnum cont i buf 
  = let int = fromInteger i in
    if i >= 0 && i <= 255 
	then cont (chr int) buf
	else charError buf

readNum cont buf is_digit base conv = read buf 0
  where read buf i 
	  = case currentChar# buf of { c ->
	    if is_digit c
		then read (incLexeme buf) (i*base + (toInteger (I# (conv c))))
		else cont i buf
	    }

is_hexdigit c 
	=  is_digit c 
	|| (c `geChar#` 'a'# && c `leChar#` 'f'#)
	|| (c `geChar#` 'A'# && c `leChar#` 'F'#)

hex c | is_digit c = ord# c -# ord# '0'#
      | otherwise  = ord# (to_lower c) -# ord# 'a'# +# 10#
oct_or_dec c = ord# c -# ord# '0'#

is_octdigit c = c `geChar#` '0'# && c `leChar#` '7'#

to_lower c 
  | c `geChar#` 'A'# && c `leChar#` 'Z'#  
	= chr# (ord# c -# (ord# 'A'# -# ord# 'a'#))
  | otherwise = c

charError buf = lexError "error in character literal" buf

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

-------------------------------------------------------------------------------

lex_demand cont buf = 
 case read_em [] buf of { (ls,buf') -> 
 case currentChar# buf' of
   'B'# -> cont (ITstrict (ls, True )) (incLexeme buf')
   _    -> cont (ITstrict (ls, False)) buf'
 }
 where
   -- code snatched from Demand.lhs
  read_em acc buf = 
   case currentChar# buf of
    'L'# -> read_em (WwLazy False : acc) (stepOn buf)
    'A'# -> read_em (WwLazy True  : acc) (stepOn buf)
    'S'# -> read_em (WwStrict     : acc) (stepOn buf)
    'P'# -> read_em (WwPrim       : acc) (stepOn buf)
    'E'# -> read_em (WwEnum       : acc) (stepOn buf)
    ')'# -> (reverse acc, stepOn buf)
    'U'# -> do_unpack DataType True  acc (stepOnBy# buf 2#)
    'u'# -> do_unpack DataType False acc (stepOnBy# buf 2#)
    'N'# -> do_unpack NewType True  acc (stepOnBy# buf 2#)
    'n'# -> do_unpack NewType False acc (stepOnBy# buf 2#)
    _    -> (reverse acc, buf)

  do_unpack new_or_data wrapper_unpacks acc buf
   = case read_em [] buf of
      (stuff, rest) -> read_em (WwUnpack new_or_data wrapper_unpacks stuff : acc) rest


------------------
lex_scc cont buf =
 case currentChar# buf of
  'C'# -> cont ITsccAllCafs (incLexeme buf)
  other -> cont ITscc buf

-----------------------------------------------------------------------------
-- Numbers

lex_num :: (Token -> P a) -> Int# -> Integer -> P a
lex_num cont glaexts acc buf =
 case scanNumLit acc buf of
     (acc',buf') ->
       case currentChar# buf' of
         '.'# | is_digit (lookAhead# buf' 1#) ->
             -- this case is not optimised at all, as the
             -- presence of floating point numbers in interface
             -- files is not that common. (ToDo)
	    case expandWhile# is_digit (incLexeme buf') of
              buf2 -> -- points to first non digit char

		let l = case currentChar# buf2 of
			  'E'# -> do_exponent
		          'e'# -> do_exponent
		          _ -> buf2

		    do_exponent 
			= let buf3 = incLexeme buf2 in
			  case currentChar# buf3 of
				'-'# -> expandWhile# is_digit (incLexeme buf3)
				'+'# -> expandWhile# is_digit (incLexeme buf3)
				x | is_digit x -> expandWhile# is_digit buf3
				_ -> buf2

		    v = readRational__ (lexemeToString l)

		in case currentChar# l of -- glasgow exts only
		      '#'# | flag glaexts -> let l' = incLexeme l in
			      case currentChar# l' of
				'#'# -> cont (ITprimdouble v) (incLexeme l')
				_    -> cont (ITprimfloat  v) l'
		      _ -> cont (ITrational v) l

         _ -> after_lexnum cont glaexts acc' buf'
		
after_lexnum cont glaexts i buf
  = case currentChar# buf of
	'#'# | flag glaexts -> cont (ITprimint i) (incLexeme buf)
	_    -> cont (ITinteger i) buf

-----------------------------------------------------------------------------
-- C "literal literal"s  (i.e. things like ``NULL'', ``stdout'' etc.)

-- we lexemeToFastString on the bit between the ``''s, but include the
-- quotes in the full lexeme.

lex_cstring cont buf =
 case expandUntilMatch (stepOverLexeme buf) "\'\'" of
   Just buf' -> cont (ITlitlit (lexemeToFastString 
				(setCurrentPos# buf' (negateInt# 2#))))
           	   (mergeLexemes buf buf')
   Nothing   -> lexError "unterminated ``" buf

-----------------------------------------------------------------------------
-- identifiers, symbols etc.

lex_ip cont buf =
 case expandWhile# is_ident buf of
   buf' -> cont (ITipvarid lexeme) buf'
	   where lexeme = lexemeToFastString buf'

lex_id cont glaexts buf =
 case expandWhile# is_ident buf of { buf1 -> 

 case (if flag glaexts 
	then expandWhile# (eqChar# '#'#) buf1 -- slurp trailing hashes
	else buf1) 				of { buf' ->

 let lexeme  = lexemeToFastString buf' in

 case _scc_ "Lex.haskellKeyword" lookupUFM haskellKeywordsFM lexeme of {
 	Just kwd_token -> --trace ("hkeywd: "++_UNPK_(lexeme)) $
			  cont kwd_token buf';
 	Nothing        -> 

 let var_token = cont (mk_var_token lexeme) buf' in

 if not (flag glaexts)
   then var_token
   else

 case lookupUFM ghcExtensionKeywordsFM lexeme of {
	Just kwd_token -> cont kwd_token buf';
	Nothing        -> var_token

 }}}}

lex_sym cont buf =
 case expandWhile# is_symbol buf of
   buf' -> case lookupUFM haskellKeySymsFM lexeme of {
	 	Just kwd_token -> --trace ("keysym: "++unpackFS lexeme) $
				  cont kwd_token buf' ;
	 	Nothing        -> --trace ("sym: "++unpackFS lexeme) $
				  cont (mk_var_token lexeme) buf'
           }
   	where lexeme = lexemeToFastString buf'


lex_con cont glaexts buf = 
 case expandWhile# is_ident buf          of { buf1 ->
 case slurp_trailing_hashes buf1 glaexts of { buf' ->

 case currentChar# buf' of
     '.'# -> munch
     _    -> just_a_conid
 
   where
    just_a_conid = --trace ("con: "++unpackFS lexeme) $
		   cont (ITconid lexeme) buf'
    lexeme = lexemeToFastString buf'
    munch = lex_qid cont glaexts lexeme (incLexeme buf') just_a_conid
 }}

lex_qid cont glaexts mod buf just_a_conid =
 case currentChar# buf of
  '['# -> 	-- Special case for []
    case lookAhead# buf 1# of
     ']'# -> cont (ITqconid  (mod,SLIT("[]"))) (setCurrentPos# buf 2#)
     _    -> just_a_conid

  '('# ->  -- Special case for (,,,)
	   -- This *is* necessary to deal with e.g. "instance C PrelBase.(,,)"
    case lookAhead# buf 1# of
     '#'# | flag glaexts -> case lookAhead# buf 2# of
		','# -> lex_ubx_tuple cont mod (setCurrentPos# buf 3#) 
				just_a_conid
		_    -> just_a_conid
     ')'# -> cont (ITqconid (mod,SLIT("()"))) (setCurrentPos# buf 2#)
     ','# -> lex_tuple cont mod (setCurrentPos# buf 2#) just_a_conid
     _    -> just_a_conid

  '-'# -> case lookAhead# buf 1# of
            '>'# -> cont (ITqconid (mod,SLIT("->"))) (setCurrentPos# buf 2#)
            _    -> lex_id3 cont glaexts mod buf just_a_conid
  _    -> lex_id3 cont glaexts mod buf just_a_conid

lex_id3 cont glaexts mod buf just_a_conid
  | is_symbol (currentChar# buf) =
     let 
	start_new_lexeme = stepOverLexeme buf
     in
     case expandWhile# is_symbol start_new_lexeme of { buf' ->
     let
       lexeme  = lexemeToFastString buf'
	-- real lexeme is M.<sym>
       new_buf = mergeLexemes buf buf'
     in
     cont (mk_qvar_token mod lexeme) new_buf
	-- wrong, but arguably morally right: M... is now a qvarsym
     }

  | otherwise   =
     let 
	start_new_lexeme = stepOverLexeme buf
     in
     case expandWhile# is_ident start_new_lexeme of { buf1 ->
     if emptyLexeme buf1 
    	    then just_a_conid
    	    else

     case slurp_trailing_hashes buf1 glaexts of { buf' ->

     let
      lexeme  = lexemeToFastString buf'
      new_buf = mergeLexemes buf buf'
      is_a_qvarid = cont (mk_qvar_token mod lexeme) new_buf
     in
     case _scc_ "Lex.haskellKeyword" lookupUFM haskellKeywordsFM lexeme of {
    	    Just kwd_token -> just_a_conid; -- avoid M.where etc.
    	    Nothing        -> is_a_qvarid
	-- TODO: special ids (as, qualified, hiding) shouldn't be
	-- recognised as keywords here.  ie.  M.as is a qualified varid.
     }}}


slurp_trailing_hashes buf glaexts
  | flag glaexts = expandWhile# (`eqChar#` '#'#) buf
  | otherwise    = buf


mk_var_token pk_str
  | is_upper f		= ITconid pk_str
  | is_ident f		= ITvarid pk_str
  | f `eqChar#` ':'#	= ITconsym pk_str
  | otherwise		= ITvarsym pk_str
  where
      (C# f) = _HEAD_ pk_str
      tl     = _TAIL_ pk_str

mk_qvar_token m token =
 case mk_var_token token of
   ITconid n  -> ITqconid  (m,n)
   ITvarid n  -> ITqvarid  (m,n)
   ITconsym n -> ITqconsym (m,n)
   ITvarsym n -> ITqvarsym (m,n)
   _	      -> ITunknown (show token)
\end{code}

----------------------------------------------------------------------------
Horrible stuff for dealing with M.(,,,)

\begin{code}
lex_tuple cont mod buf back_off =
  go 2 buf
  where
   go n buf =
    case currentChar# buf of
      ','# -> go (n+1) (stepOn buf)
      ')'# -> cont (ITqconid (mod, snd (mkTupNameStr n))) (stepOn buf)
      _    -> back_off

lex_ubx_tuple cont mod buf back_off =
  go 2 buf
  where
   go n buf =
    case currentChar# buf of
      ','# -> go (n+1) (stepOn buf)
      '#'# -> case lookAhead# buf 1# of
		')'# -> cont (ITqconid (mod, snd (mkUbxTupNameStr n)))
				 (stepOnBy# buf 2#)
	        _    -> back_off
      _    -> back_off
\end{code}

-----------------------------------------------------------------------------
doDiscard rips along really fast, looking for a '#-}', 
indicating the end of the pragma we're skipping

\begin{code}
doDiscard inStr buf =
 case currentChar# buf of
   '#'# | not inStr ->
       case lookAhead# buf 1# of { '#'# -> 
       case lookAhead# buf 2# of { '-'# ->
       case lookAhead# buf 3# of { '}'# -> 
	   (lexemeToBuffer buf, stepOverLexeme (setCurrentPos# buf 4#));
	_    -> doDiscard inStr (incLexeme buf) };
        _    -> doDiscard inStr (incLexeme buf) };
        _    -> doDiscard inStr (incLexeme buf) }
   '"'# ->
       let
        odd_slashes buf flg i# =
          case lookAhead# buf i# of
	   '\\'# -> odd_slashes buf (not flg) (i# -# 1#)
	   _     -> flg
       in
       case lookAhead# buf (negateInt# 1#) of --backwards, actually
	 '\\'# -> -- escaping something..
	   if odd_slashes buf True (negateInt# 2#) then
	       -- odd number of slashes, " is escaped.
	      doDiscard inStr (incLexeme buf)
	   else
	       -- even number of slashes, \ is escaped.
	      doDiscard (not inStr) (incLexeme buf)
         _ -> case inStr of -- forced to avoid build-up
	       True  -> doDiscard False (incLexeme buf)
               False -> doDiscard True  (incLexeme buf)
   _ -> doDiscard inStr (incLexeme buf)

\end{code}

-----------------------------------------------------------------------------

\begin{code}
data LayoutContext
  = NoLayout
  | Layout Int#

data ParseResult a
  = POk PState a
  | PFailed Message

data PState = PState { 
	loc           :: SrcLoc,
	glasgow_exts  :: Int#,
	bol           :: Int#,
	atbol         :: Int#,
	context	      :: [LayoutContext]
     }

type P a = StringBuffer		-- Input string
	-> PState
	-> ParseResult a

returnP   :: a -> P a
returnP a buf s = POk s a

thenP	   :: P a -> (a -> P b) -> P b
m `thenP` k = \ buf s ->
	case m buf s of
		POk s1 a -> k a buf s1
		PFailed err  -> PFailed err

thenP_ 	   :: P a -> P b -> P b
m `thenP_` k = m `thenP` \_ -> k

mapP :: (a -> P b) -> [a] -> P [b]
mapP f [] = returnP []
mapP f (a:as) = 
     f a `thenP` \b ->
     mapP f as `thenP` \bs ->
     returnP (b:bs)

failP :: String -> P a
failP msg buf s = PFailed (text msg)

failMsgP :: Message -> P a
failMsgP msg buf s = PFailed msg

lexError :: String -> P a
lexError str buf s@PState{ loc = loc } 
  = failMsgP (hcat [ppr loc, text ": ", text str]) buf s

getSrcLocP :: P SrcLoc
getSrcLocP buf s@(PState{ loc = loc }) = POk s loc

getSrcFile :: P FAST_STRING
getSrcFile buf s@(PState{ loc = loc }) = POk s (srcLocFile loc)

getContext :: P [LayoutContext]
getContext buf s@(PState{ context = ctx }) = POk s ctx

pushContext :: LayoutContext -> P ()
pushContext ctxt buf s@(PState{ context = ctx }) = POk s{context = ctxt:ctx} ()

{-

This special case in layoutOn is to handle layout contexts with are
indented the same or less than the current context.  This is illegal
according to the Haskell spec, so we have to arrange to close the
current context.  eg.

class Foo a where
class Bar a

after the first 'where', the sequence of events is:

	- layout system inserts a ';' (column 0)
	- parser begins a new context at column 0
	- parser shifts ';' (legal empty declaration)
	- parser sees 'class': parse error (we're still in the inner context)

trouble is, by the time we know we need a new context, the lexer has
already generated the ';'.  Hacky solution is as follows: since we
know the column of the next token (it's the column number of the new
context), we set the ACTUAL column number of the new context to this
numer plus one.  Hence the next time the lexer is called, a '}' will
be generated to close the new context straight away.  Furthermore, we
have to set the atbol flag so that the ';' that the parser shifted as
part of the new context is re-generated.

when the new context is *less* indented than the current one:

f = f where g = g where
h = h

	- current context: column 12.
	- on seeing 'h' (column 0), the layout system inserts '}'
	- parser starts a new context, column 0
	- parser sees '}', uses it to close new context
	- we still need to insert another '}' followed by a ';',
	  hence the atbol trick.

There's also a special hack in here to deal with

	do
	   ....
	   e $ do
	   blah

i.e. the inner context is at the same indentation level as the outer
context.  This is strictly illegal according to Haskell 98, but
there's a lot of existing code using this style and it doesn't make
any sense to disallow it, since empty 'do' lists don't make sense.
-}

layoutOn :: Bool -> P ()
layoutOn strict buf s@(PState{ bol = bol, context = ctx }) =
    let offset = lexemeIndex buf -# bol in
    case ctx of
	Layout prev_off : _ 
	   | if strict then prev_off >=# offset else prev_off ># offset ->
		--trace ("layout on, column: " ++  show (I# offset)) $
		POk s{ context = Layout (offset +# 1#) : ctx, atbol = 1# } ()
	other -> 
		--trace ("layout on, column: " ++  show (I# offset)) $
    		POk s{ context = Layout offset : ctx } ()

layoutOff :: P ()
layoutOff buf s@(PState{ context = ctx }) =
    POk s{ context = NoLayout:ctx } ()

popContext :: P ()
popContext = \ buf s@(PState{ context = ctx, loc = loc }) ->
  case ctx of
	(_:tl) -> POk s{ context = tl } ()
	[]     -> PFailed (srcParseErr buf loc)

{- 
 Note that if the name of the file we're processing ends
 with `hi-boot', we accept it on faith as having the right
 version. This is done so that .hi-boot files that comes
 with hsc don't have to be updated before every release,
 *and* it allows us to share .hi-boot files with versions
 of hsc that don't have .hi version checking (e.g., ghc-2.10's)

 If the version number is 0, the checking is also turned off.
 (needed to deal with GHC.hi only!)

 Once we can assume we're compiling with a version of ghc that
 supports interface file checking, we can drop the special
 pleading
-}
checkVersion :: Maybe Integer -> P ()
checkVersion mb@(Just v) buf s@(PState{loc = loc})
 | (v==0) || (v == fromInt opt_HiVersion) || opt_NoHiCheck = POk s ()
 | otherwise = PFailed (ifaceVersionErr mb loc ([]::[Token]){-Todo-})
checkVersion mb@Nothing  buf s@(PState{loc = loc})
 | "hi-boot" `isSuffixOf` (_UNPK_ (srcLocFile loc)) = POk s ()
 | otherwise = PFailed (ifaceVersionErr mb loc ([]::[Token]){-Todo-})

-----------------------------------------------------------------

ifaceParseErr :: StringBuffer -> SrcLoc -> Message
ifaceParseErr s l
  = hsep [ppr l, ptext SLIT("Interface file parse error; on input `"),
          text (lexemeToString s), char '\'']

ifaceVersionErr hi_vers l toks
  = hsep [ppr l, ptext SLIT("Interface file version error;"),
          ptext SLIT("Expected"), int opt_HiVersion, 
	  ptext SLIT("found "), pp_version]
    where
     pp_version =
      case hi_vers of
        Nothing -> ptext SLIT("pre ghc-3.02 version")
	Just v  -> ptext SLIT("version") <+> integer v

-----------------------------------------------------------------------------

srcParseErr :: StringBuffer -> SrcLoc -> Message
srcParseErr s l
  = hcat [ppr l, 
	  if null token 
	     then ptext SLIT(": parse error (possibly incorrect indentation)")
	     else hcat [ptext SLIT(": parse error on input "),
          	  	char '`', text token, char '\'']
    ]
  where 
	token = lexemeToString s

\end{code}
