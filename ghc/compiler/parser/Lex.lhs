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

	srcParseErr,

	-- Monad for parser
	Token(..), lexer, ParseResult(..), PState(..),
	ExtFlags(..), mkPState, 
	StringBuffer,

	P, thenP, thenP_, returnP, mapP, failP, failMsgP,
	getSrcLocP, setSrcLocP, getSrcFile,
	layoutOn, layoutOff, pushContext, popContext
    ) where

#include "HsVersions.h"

import Char 		( toUpper, isDigit, chr, ord )
import Ratio		( (%) )

import PrelNames	( mkTupNameStr )
import ForeignCall	( Safety(..) )
import UniqFM           ( listToUFM, lookupUFM )
import BasicTypes	( Boxity(..) )
import SrcLoc		( SrcLoc, incSrcLine, srcLocFile, srcLocLine,
			  replaceSrcLine, mkSrcLoc )

import ErrUtils		( Message )
import Outputable

import FastString
import StringBuffer
import GlaExts
import Ctype

import Bits		( Bits(..) )	   -- non-std
import Int		( Int32 )
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

  | ITspecialise_prag		-- Pragmas
  | ITsource_prag
  | ITinline_prag
  | ITnoinline_prag
  | ITrules_prag
  | ITdeprecated_prag
  | ITline_prag
  | ITscc_prag
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
  | ITstar
  | ITdot

  | ITbiglam			-- GHC-extension symbols

  | ITocurly  			-- special symbols
  | ITccurly
  | ITocurlybar                 -- {|, for type applications
  | ITccurlybar                 -- |}, for type applications
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

  | ITvarid   FAST_STRING	-- identifiers
  | ITconid   FAST_STRING
  | ITvarsym  FAST_STRING
  | ITconsym  FAST_STRING
  | ITqvarid  (FAST_STRING,FAST_STRING)
  | ITqconid  (FAST_STRING,FAST_STRING)
  | ITqvarsym (FAST_STRING,FAST_STRING)
  | ITqconsym (FAST_STRING,FAST_STRING)

  | ITdupipvarid   FAST_STRING	-- GHC extension: implicit param: ?x
  | ITsplitipvarid FAST_STRING	-- GHC extension: implicit param: %x

  | ITpragma StringBuffer

  | ITchar       Int
  | ITstring     FAST_STRING
  | ITinteger    Integer
  | ITrational   Rational

  | ITprimchar   Int
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
	( "SCC",	ITscc_prag ),
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
	( "_scc_",	ITscc )		-- ToDo: remove
     ]

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
isSpecial ITwith      	= True
isSpecial ITccallconv   = True
isSpecial ITstdcallconv = True
isSpecial _             = False

-- IMPORTANT: Keep this in synch with ParseIface.y's var_fs production! (SUP)
ghcExtensionKeywordsFM = listToUFM $
	map (\ (x,y) -> (_PK_ x,y))
     [	( "forall",	ITforall ),
	( "foreign",	ITforeign ),
	( "export",	ITexport ),
	( "label",	ITlabel ),
	( "dynamic",	ITdynamic ),
	( "safe",	ITsafe ),
	( "threadsafe",	ITthreadsafe ),
	( "unsafe",	ITunsafe ),
	( "with",	ITwith ),
	( "stdcall",    ITstdcallconv),
	( "ccall",      ITccallconv),
	( "dotnet",     ITdotnet),
        ("_ccall_",	ITccall (False, False, PlayRisky)),
        ("_ccall_GC_",	ITccall (False, False, PlaySafe False)),
        ("_casm_",	ITccall (False, True,  PlayRisky)),
        ("_casm_GC_",	ITccall (False, True,  PlaySafe False))
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
       ,("*",		ITstar)
       ,(".",		ITdot)		-- sadly, for 'forall a . t'
       ]
\end{code}

-----------------------------------------------------------------------------
The lexical analyser

Lexer state:

	- (exts)  lexing a source with extensions, eg, an interface file or 
		  with -fglasgow-exts
	- (bol)   pointer to beginning of line (for column calculations)
	- (buf)   pointer to beginning of token
	- (buf)   pointer to current char
	- (atbol) flag indicating whether we're at the beginning of a line

\begin{code}
lexer :: (Token -> P a) -> P a
lexer cont buf s@(PState{
		    loc = loc,
		    extsBitmap = exts,
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
			        else tab y bol atbol 
					 (stepOnUntilChar# (stepOnBy# buf n) '\n'#)
		    in trundle 1#

		-- comments and pragmas.  We deal with LINE pragmas here,
		-- and throw out any unrecognised pragmas as comments.  Any
		-- pragmas we know about are dealt with later (after any layout
		-- processing if necessary).
            '{'# | lookAhead# buf 1# `eqChar#` '-'# ->
		if lookAhead# buf 2# `eqChar#` '#'# then
		  case expandWhile# is_space (setCurrentPos# buf 3#) of { buf1->
		  case expandWhile# is_ident (stepOverLexeme buf1)   of { buf2->
		  let lexeme = mkFastString -- ToDo: too slow
			          (map toUpper (lexemeToString buf2)) in
		  case lookupUFM pragmaKeywordsFM lexeme of
			-- ignore RULES pragmas when -fglasgow-exts is off
			Just ITrules_prag | not (glaExtsEnabled exts) ->
			   skip_to_end (stepOnBy# buf 2#) s'
			Just ITline_prag -> 
			   line_prag skip_to_end buf2 s'
			Just other -> is_a_token
			Nothing -> skip_to_end (stepOnBy# buf 2#) s'
		  }}

		else skip_to_end (stepOnBy# buf 2#) s'
		where
		    skip_to_end = skipNestedComment (lexer cont)

		-- special GHC extension: we grok cpp-style #line pragmas
	    '#'# | lexemeIndex buf ==# bol -> 	-- the '#' must be in column 0
		let buf1 | lookAhead# buf 1# `eqChar#` 'l'# &&
		   	   lookAhead# buf 2# `eqChar#` 'i'# &&
		    	   lookAhead# buf 3# `eqChar#` 'n'# &&
		   	   lookAhead# buf 4# `eqChar#` 'e'#  = stepOnBy# buf 5#
			 | otherwise = stepOn buf
		in
		case expandWhile# is_space buf1 of { buf2 ->
		if is_digit (currentChar# buf2) 
			then line_prag next_line buf2 s'
			else is_a_token
		}
		where
		next_line buf = lexer cont (stepOnUntilChar# buf '\n'#)

		-- tabs have been expanded beforehand
	    c | is_space c -> tab y bol atbol (stepOn buf)
	      | otherwise  -> is_a_token

	   where s' = s{loc = replaceSrcLine loc y, 
		        bol = bol,
		       atbol = atbol}

	      	 is_a_token | atbol /=# 0# = lexBOL cont buf s'
	      		    | otherwise    = lexToken cont exts buf s'

-- {-# LINE .. #-} pragmas.  yeuch.
line_prag cont buf s@PState{loc=loc} =
  case expandWhile# is_space buf 		of { buf1 ->
  case scanNumLit 0 (stepOverLexeme buf1) 	of { (line,buf2) ->
  -- subtract one: the line number refers to the *following* line.
  let real_line = line - 1 in
  case fromInteger real_line 			of { i@(I# l) -> 
	-- ToDo, if no filename then we skip the newline.... d'oh
  case expandWhile# is_space buf2 		of { buf3 ->
  case currentChar# buf3 			of
     '\"'#{-"-} -> 
	case untilEndOfString# (stepOn (stepOverLexeme buf3)) of { buf4 ->
	let 
	    file = lexemeToFastString buf4 
	    new_buf = stepOn (stepOverLexeme buf4)
	in
	if nullFastString file
		then cont new_buf s{loc = replaceSrcLine loc l}
		else cont new_buf s{loc = mkSrcLoc file i}
	}
     _other -> cont (stepOverLexeme buf3) s{loc = replaceSrcLine loc l}
  }}}}

skipNestedComment :: P a -> P a
skipNestedComment cont buf state = skipNestedComment' (loc state) cont buf state

skipNestedComment' :: SrcLoc -> P a -> P a
skipNestedComment' orig_loc cont buf = loop buf
 where
   loop buf = 
     case currentChar# buf of
	'-'# | lookAhead# buf 1# `eqChar#` '}'# -> cont (stepOnBy# buf 2#)

	'{'# | lookAhead# buf 1# `eqChar#` '-'# ->
	      skipNestedComment 
		(skipNestedComment' orig_loc cont) 
		(stepOnBy# buf 2#)

	'\n'# -> \ s@PState{loc=loc} ->
		 let buf' = stepOn buf in
		 loop buf' s{loc = incSrcLine loc, 
			     bol = currentIndex# buf',
			     atbol = 1#}

	-- pass the original SrcLoc to lexError so that the error is
	-- reported at the line it was originally on, not the line at
	-- the end of the file.
	'\NUL'# | bufferExhausted (stepOn buf) -> 
		\s -> lexError "unterminated `{-'" buf s{loc=orig_loc} -- -}

	_   -> loop (stepOn buf)

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: (Token -> P a) -> P a
lexBOL cont buf s@(PState{
		    loc = loc,
		    extsBitmap = exts,
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
		lexToken cont exts buf s{atbol = 0#}
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
lexToken cont exts buf =
-- trace "lexToken" $
  case currentChar# buf of

    -- special symbols ----------------------------------------------------
    '('# | glaExtsEnabled exts && lookAhead# buf 1# `eqChar#` '#'# 
		-> cont IToubxparen (setCurrentPos# buf 2#)
	 | otherwise
		-> cont IToparen (incLexeme buf)

    ')'# -> cont ITcparen    (incLexeme buf)
    '['# | parrEnabled exts && lookAhead# buf 1# `eqChar#` ':'# ->
	    cont ITopabrack  (setCurrentPos# buf 2#)
	 | otherwise -> 
	    cont ITobrack    (incLexeme buf)
    ']'# -> cont ITcbrack    (incLexeme buf)
    ','# -> cont ITcomma     (incLexeme buf)
    ';'# -> cont ITsemi      (incLexeme buf)
    '}'# -> \ s@PState{context = ctx} ->
	    case ctx of	
		(_:ctx') -> cont ITccurly (incLexeme buf) s{context=ctx'}
		_  	 -> lexError "too many '}'s" buf s
    '|'# -> case lookAhead# buf 1# of
	         '}'#  | glaExtsEnabled exts -> cont ITccurlybar 
                                                     (setCurrentPos# buf 2#)
                 _                           -> lex_sym cont (incLexeme buf)
    ':'# -> case lookAhead# buf 1# of
	         ']'#  | parrEnabled exts    -> cont ITcpabrack
                                                     (setCurrentPos# buf 2#)
                 _                           -> lex_sym cont (incLexeme buf)

                
    '#'# -> case lookAhead# buf 1# of
		')'#  | glaExtsEnabled exts 
		     -> cont ITcubxparen (setCurrentPos# buf 2#)
		'-'# -> case lookAhead# buf 2# of
			   '}'# -> cont ITclose_prag (setCurrentPos# buf 3#)
			   _    -> lex_sym cont (incLexeme buf)
		_    -> lex_sym cont (incLexeme buf)

    '`'# | glaExtsEnabled exts && lookAhead# buf 1# `eqChar#` '`'#
		-> lex_cstring cont (setCurrentPos# buf 2#)
	 | otherwise
	   	-> cont ITbackquote (incLexeme buf)

    '{'# ->   -- for Emacs: -}
            case lookAhead# buf 1# of
           '|'# | glaExtsEnabled exts 
                -> cont ITocurlybar (setCurrentPos# buf 2#)
	   '-'# -> case lookAhead# buf 2# of
		    '#'# -> lex_prag cont (setCurrentPos# buf 3#)
	   	    _    -> cont ITocurly (incLexeme buf) 
	   _ -> (layoutOff `thenP_` cont ITocurly)  (incLexeme buf) 

    -- strings/characters -------------------------------------------------
    '\"'#{-"-} -> lex_string cont exts [] (incLexeme buf)
    '\''#      -> lex_char (char_end cont) exts (incLexeme buf)

	-- Hexadecimal and octal constants
    '0'# | (ch `eqChar#` 'x'# || ch `eqChar#` 'X'#) && is_hexdigit ch2
		-> readNum (after_lexnum cont exts) buf' is_hexdigit 16 hex
	 | (ch `eqChar#` 'o'# || ch `eqChar#` 'O'#) && is_octdigit ch2
		-> readNum (after_lexnum cont exts) buf' is_octdigit  8 oct_or_dec
	where ch   = lookAhead# buf 1#
	      ch2  = lookAhead# buf 2#
	      buf' = setCurrentPos# buf 2#

    '\NUL'# ->
	    if bufferExhausted (stepOn buf) then
	       cont ITeof buf
	    else
	       trace "lexIface: misplaced NUL?" $ 
	       cont (ITunknown "\NUL") (stepOn buf)

    '?'# | glaExtsEnabled exts && is_lower (lookAhead# buf 1#) ->
	    lex_ip ITdupipvarid cont (incLexeme buf)
    '%'# | glaExtsEnabled exts && is_lower (lookAhead# buf 1#) ->
	    lex_ip ITsplitipvarid cont (incLexeme buf)
    c | is_digit  c -> lex_num cont exts 0 buf
      | is_symbol c -> lex_sym cont buf
      | is_upper  c -> lex_con cont exts buf
      | is_ident  c -> lex_id  cont exts buf
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

lex_string cont exts s buf
  = case currentChar# buf of
	'"'#{-"-} -> 
	   let buf' = incLexeme buf
               s' = mkFastStringNarrow (map chr (reverse s)) 
           in case currentChar# buf' of
		'#'# | glaExtsEnabled exts -> if all (<= 0xFF) s
                    then cont (ITprimstring s') (incLexeme buf')
                    else lexError "primitive string literal must contain only characters <= \'\\xFF\'" buf'
		_                   -> cont (ITstring s') buf'

	-- ignore \& in a string, deal with string gaps
	'\\'# | next_ch `eqChar#` '&'# 
		-> lex_string cont exts s buf'
	      | is_space next_ch
		-> lex_stringgap cont exts s (incLexeme buf)

	    where next_ch = lookAhead# buf 1#
		  buf' = setCurrentPos# buf 2#

	_ -> lex_char (lex_next_string cont s) exts buf

lex_stringgap cont exts s buf
  = let buf' = incLexeme buf in
    case currentChar# buf of
	'\n'# -> \st@PState{loc = loc} -> lex_stringgap cont exts s buf' 
		  st{loc = incSrcLine loc}
	'\\'# -> lex_string cont exts s buf'
	c | is_space c -> lex_stringgap cont exts s buf'
	other -> charError buf'

lex_next_string cont s exts c buf = lex_string cont exts (c:s) buf

lex_char :: (Int# -> Int -> P a) -> Int# -> P a
lex_char cont exts buf
  = case currentChar# buf of
	'\\'# -> lex_escape (cont exts) (incLexeme buf)
	c | is_any c -> cont exts (I# (ord# c)) (incLexeme buf)
	other -> charError buf

char_end cont exts c buf
  = case currentChar# buf of
	'\''# -> let buf' = incLexeme buf in
		 case currentChar# buf' of
			'#'# | glaExtsEnabled exts 
				-> cont (ITprimchar c) (incLexeme buf')
			_   	-> cont (ITchar c) buf'
	_     -> charError buf

lex_escape cont buf
  = let buf' = incLexeme buf in
    case currentChar# buf of
	'a'#	   -> cont (ord '\a') buf'
	'b'#	   -> cont (ord '\b') buf'
	'f'#	   -> cont (ord '\f') buf'
	'n'#	   -> cont (ord '\n') buf'
	'r'#	   -> cont (ord '\r') buf'
	't'#	   -> cont (ord '\t') buf'
	'v'#	   -> cont (ord '\v') buf'
	'\\'#      -> cont (ord '\\') buf'
	'"'#       -> cont (ord '\"') buf'
	'\''#      -> cont (ord '\'') buf'
	'^'#	   -> let c = currentChar# buf' in
		      if c `geChar#` '@'# && c `leChar#` '_'#
			then cont (I# (ord# c -# ord# '@'#)) (incLexeme buf')
			else charError buf'

	'x'#      -> readNum (after_charnum cont) buf' is_hexdigit 16 hex
	'o'#      -> readNum (after_charnum cont) buf' is_octdigit  8 oct_or_dec
	x | is_digit x 
		  -> readNum (after_charnum cont) buf is_digit    10 oct_or_dec

	_          -> case [ (c,buf2) | (p,c) <- silly_escape_chars,
				       Just buf2 <- [prefixMatch buf p] ] of
			    (c,buf2):_ -> cont (ord c) buf2
			    [] -> charError buf'

after_charnum cont i buf
  = if i >= 0 && i <= 0x10FFFF
	then cont (fromInteger i) buf
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

-----------------------------------------------------------------------------
-- Numbers

lex_num :: (Token -> P a) -> Int# -> Integer -> P a
lex_num cont exts acc buf =
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
				'-'# | is_digit (lookAhead# buf3 1#)
				   -> expandWhile# is_digit (incLexeme buf3)
				'+'# | is_digit (lookAhead# buf3 1#)
				   -> expandWhile# is_digit (incLexeme buf3)
				x | is_digit x -> expandWhile# is_digit buf3
				_ -> buf2

		    v = readRational__ (lexemeToString l)

		in case currentChar# l of -- glasgow exts only
		      '#'# | glaExtsEnabled exts -> let l' = incLexeme l in
			      case currentChar# l' of
				'#'# -> cont (ITprimdouble v) (incLexeme l')
				_    -> cont (ITprimfloat  v) l'
		      _ -> cont (ITrational v) l

         _ -> after_lexnum cont exts acc' buf'
		
after_lexnum cont exts i buf
  = case currentChar# buf of
	'#'# | glaExtsEnabled exts -> cont (ITprimint i) (incLexeme buf)
	_                          -> cont (ITinteger i) buf

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

lex_ip ip_constr cont buf =
 case expandWhile# is_ident buf of
   buf' -> cont (ip_constr (tailFS lexeme)) buf'
	where lexeme = lexemeToFastString buf'

lex_id cont exts buf =
 let buf1 = expandWhile# is_ident buf in
 seq buf1 $

 case (if glaExtsEnabled exts 
	then expandWhile# (eqChar# '#'#) buf1 -- slurp trailing hashes
	else buf1) 				of { buf' ->
 seq buf' $

 let lexeme  = lexemeToFastString buf' in

 case _scc_ "haskellKeyword" lookupUFM haskellKeywordsFM lexeme of {
 	Just kwd_token -> --trace ("hkeywd: "++_UNPK_(lexeme)) $
			  cont kwd_token buf';
 	Nothing        -> 

 let var_token = cont (ITvarid lexeme) buf' in

 if not (glaExtsEnabled exts)
   then var_token
   else

 case lookupUFM ghcExtensionKeywordsFM lexeme of {
	Just kwd_token -> cont kwd_token buf';
	Nothing        -> var_token

 }}}

lex_sym cont buf =
 -- trace "lex_sym" $
 case expandWhile# is_symbol buf of
   buf' -> case lookupUFM haskellKeySymsFM lexeme of {
	 	Just kwd_token -> --trace ("keysym: "++unpackFS lexeme) $
				  cont kwd_token buf' ;
	 	Nothing        -> --trace ("sym: "++unpackFS lexeme) $
				  cont (mk_var_token lexeme) buf'
           }
   	where lexeme = lexemeToFastString buf'


-- lex_con recursively collects components of a qualified identifer.
-- The argument buf is the StringBuffer representing the lexeme
-- identified so far, where the next character is upper-case.

lex_con cont exts buf =
 -- trace ("con: "{-++unpackFS lexeme-}) $
 let empty_buf = stepOverLexeme buf in
 case expandWhile# is_ident empty_buf of { buf1 ->
 case slurp_trailing_hashes buf1 exts of { con_buf ->

 let all_buf = mergeLexemes buf con_buf
     
     con_lexeme = lexemeToFastString con_buf
     mod_lexeme = lexemeToFastString (decLexeme buf)
     all_lexeme = lexemeToFastString all_buf

     just_a_conid
	| emptyLexeme buf = cont (ITconid con_lexeme)               all_buf
	| otherwise 	  = cont (ITqconid (mod_lexeme,con_lexeme)) all_buf
 in

 case currentChar# all_buf of
     '.'# -> maybe_qualified cont exts all_lexeme 
		(incLexeme all_buf) just_a_conid
     _    -> just_a_conid
  }}


maybe_qualified cont exts mod buf just_a_conid =
 -- trace ("qid: "{-++unpackFS lexeme-}) $
 case currentChar# buf of
  '['# -> 	-- Special case for []
    case lookAhead# buf 1# of
     ']'# -> cont (ITqconid  (mod,SLIT("[]"))) (setCurrentPos# buf 2#)
     _    -> just_a_conid

  '('# ->  -- Special case for (,,,)
	   -- This *is* necessary to deal with e.g. "instance C PrelBase.(,,)"
    case lookAhead# buf 1# of
     '#'# | glaExtsEnabled exts -> case lookAhead# buf 2# of
		','# -> lex_ubx_tuple cont mod (setCurrentPos# buf 3#) 
				just_a_conid
		_    -> just_a_conid
     ')'# -> cont (ITqconid (mod,SLIT("()"))) (setCurrentPos# buf 2#)
     ','# -> lex_tuple cont mod (setCurrentPos# buf 2#) just_a_conid
     _    -> just_a_conid

  '-'# -> case lookAhead# buf 1# of
            '>'# -> cont (ITqconid (mod,SLIT("(->)"))) (setCurrentPos# buf 2#)
            _    -> lex_id3 cont exts mod buf just_a_conid

  _    -> lex_id3 cont exts mod buf just_a_conid


lex_id3 cont exts mod buf just_a_conid
  | is_upper (currentChar# buf) =
     lex_con cont exts buf

  | is_symbol (currentChar# buf) =
     let 
	start_new_lexeme = stepOverLexeme buf
     in
     -- trace ("lex_id31 "{-++unpackFS lexeme-}) $
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
     -- trace ("lex_id32 "{-++unpackFS lexeme-}) $
     case expandWhile# is_ident start_new_lexeme of { buf1 ->
     if emptyLexeme buf1 
    	    then just_a_conid
    	    else

     case slurp_trailing_hashes buf1 exts of { buf' ->

     let
      lexeme	  = lexemeToFastString buf'
      new_buf     = mergeLexemes buf buf'
      is_a_qvarid = cont (mk_qvar_token mod lexeme) new_buf
     in
     case _scc_ "haskellKeyword" lookupUFM haskellKeywordsFM lexeme of {
    	    Nothing          -> is_a_qvarid ;

    	    Just kwd_token | isSpecial kwd_token   -- special ids (as, qualified, hiding) shouldn't be
			   -> is_a_qvarid	   --  recognised as keywords here.
			   | otherwise
			   -> just_a_conid	   -- avoid M.where etc.
     }}}

slurp_trailing_hashes buf exts
  | glaExtsEnabled exts = expandWhile# (`eqChar#` '#'#) buf
  | otherwise		= buf


mk_var_token pk_str
  | is_upper f		= ITconid pk_str
  | is_ident f		= ITvarid pk_str
  | f `eqChar#` ':'#	= ITconsym pk_str
  | otherwise		= ITvarsym pk_str
  where
      (C# f) = _HEAD_ pk_str
      -- tl     = _TAIL_ pk_str

mk_qvar_token m token =
-- trace ("mk_qvar ") $ 
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
      ')'# -> cont (ITqconid (mod, snd (mkTupNameStr Boxed n))) (stepOn buf)
      _    -> back_off

lex_ubx_tuple cont mod buf back_off =
  go 2 buf
  where
   go n buf =
    case currentChar# buf of
      ','# -> go (n+1) (stepOn buf)
      '#'# -> case lookAhead# buf 1# of
		')'# -> cont (ITqconid (mod, snd (mkTupNameStr Unboxed n)))
				 (stepOnBy# buf 2#)
	        _    -> back_off
      _    -> back_off
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
	loc        :: SrcLoc,
	extsBitmap :: Int#,	-- bitmap that determines permitted extensions
	bol        :: Int#,
	atbol      :: Int#,
	context	   :: [LayoutContext]
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

-- use a temporary SrcLoc for the duration of the argument
setSrcLocP :: SrcLoc -> P a -> P a
setSrcLocP new_loc p buf s = 
  case p buf s{ loc=new_loc } of
      POk _ a   -> POk s a
      PFailed e -> PFailed e
  
getSrcFile :: P FAST_STRING
getSrcFile buf s@(PState{ loc = loc }) = POk s (srcLocFile loc)

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

-- for reasons of efficiency, flags indicating language extensions (eg,
-- -fglasgow-exts or -fparr) are represented by a bitmap stored in an unboxed
-- integer

glaExtsBit, ffiBit, parrBit :: Int
glaExtsBit = 0
ffiBit	   = 1	-- FIXME: not used yet; still part of `glaExtsBit'
parrBit	   = 2

glaExtsEnabled, ffiEnabled, parrEnabled :: Int# -> Bool
glaExtsEnabled flags = testBit (toInt32 flags) glaExtsBit
ffiEnabled     flags = testBit (toInt32 flags) ffiBit
parrEnabled    flags = testBit (toInt32 flags) parrBit

toInt32 :: Int# -> Int32
toInt32 x# = fromIntegral (I# x#)

-- convenient record-based bitmap for the interface to the rest of the world
--
data ExtFlags = ExtFlags {
		  glasgowExtsEF :: Bool,
--		  ffiEF		:: Bool,  -- commented out to avoid warnings
		  parrEF	:: Bool	  -- while not used yet
		}

-- create a parse state
--
mkPState          :: SrcLoc -> ExtFlags -> PState
mkPState loc exts  = PState {
		       loc        = loc,
		       extsBitmap = case (fromIntegral bitmap) of {I# bits -> bits},
		       bol	  = 0#,
		       atbol      = 1#,
		       context    = []
		     }
		     where
		       bitmap =     glaExtsBit `setBitIf` glasgowExtsEF exts
--			        .|. ffiBit     `setBitIf` ffiEF		exts
				.|. parrBit    `setBitIf` parrEF	exts
                       --
		       setBitIf :: Int -> Bool -> Int32
		       b `setBitIf` cond | cond      = bit b
					 | otherwise = 0

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
