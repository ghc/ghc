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
{-# OPTIONS -#include "ctypes.h" #-}

module Lex (

	ifaceParseErr,

	-- Monad for parser
	IfaceToken(..), lexIface, IfM, thenIf, returnIf, getSrcLocIf,
	checkVersion, 
	happyError,
	StringBuffer

    ) where

#include "HsVersions.h"

import Char 		( ord, isSpace )
import List             ( isSuffixOf )

import IdInfo		( InlinePragInfo(..), CprInfo(..) )
import Name		( isLowerISO, isUpperISO )
import Module		( IfaceFlavour, hiFile, hiBootFile )
import PrelMods		( mkTupNameStr, mkUbxTupNameStr )
import CmdLineOpts	( opt_IgnoreIfacePragmas, opt_HiVersion, opt_NoHiCheck )
import Demand		( Demand(..) {- instance Read -} )
import UniqFM           ( UniqFM, listToUFM, lookupUFM)
import BasicTypes	( NewOrData(..) )
import SrcLoc		( SrcLoc, incSrcLine, srcLocFile )

import Maybes		( MaybeErr(..) )
import ErrUtils		( Message )
import Outputable

import FastString
import StringBuffer
import GlaExts
import ST		( runST )

#if __GLASGOW_HASKELL__ >= 303
import Bits
import Word
#endif

import Addr
import PrelRead 		( readRational__ ) -- Glasgow non-std
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
data IfaceToken
  = ITcase  			-- Haskell keywords
  | ITclass
  | ITdata
  | ITdefault
  | ITderiving
  | ITdo
  | ITelse
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
  | ITthen
  | ITtype
  | ITwhere
  | ITas
  | ITqualified
  | IThiding

  | ITinterface			-- GHC-extension keywords
  | ITexport
  | ITinstimport
  | ITforall
  | ITletrec 
  | ITcoerce
  | ITinline
  | ITccall (Bool,Bool,Bool)	-- (is_dyn, is_casm, may_gc)
  | ITdefaultbranch
  | ITbottom
  | ITinteger_lit 
  | ITfloat_lit
  | ITrational_lit
  | ITaddr_lit
  | ITlit_lit
  | ITstring_lit
  | ITtypeapp
  | ITarity 
  | ITspecialise
  | ITnocaf
  | ITunfold InlinePragInfo
  | ITstrict ([Demand], Bool)
  | ITcprinfo (CprInfo)
  | ITscc
  | ITsccAllCafs

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

  | ITbiglam			-- GHC-extension symbols

  | ITocurly  			-- special symbols
  | ITccurly
  | ITobrack
  | ITcbrack
  | IToparen
  | ITcparen
  | IToubxparen
  | ITcubxparen
  | ITsemi
  | ITcomma

  | ITvarid   FAST_STRING	-- identifiers
  | ITconid   FAST_STRING
  | ITvarsym  FAST_STRING
  | ITconsym  FAST_STRING
  | ITqvarid  (FAST_STRING,FAST_STRING,IfaceFlavour)
  | ITqconid  (FAST_STRING,FAST_STRING,IfaceFlavour)
  | ITqvarsym (FAST_STRING,FAST_STRING,IfaceFlavour)
  | ITqconsym (FAST_STRING,FAST_STRING,IfaceFlavour)

  | ITpragma StringBuffer

  | ITchar Char 
  | ITstring FAST_STRING
  | ITinteger Integer 
  | ITrational Rational

  | ITunknown String		-- Used when the lexer can't make sense of it
  | ITeof			-- end of file token
  deriving Text -- debugging
\end{code}

%************************************************************************
%*									*
\subsection{The lexical analyser}
%*									*
%************************************************************************

\begin{code}
lexIface :: (IfaceToken -> IfM a) -> IfM a
lexIface cont buf =
 _scc_ "Lexer" 
-- if bufferExhausted buf then
--  []
-- else
--  trace ("Lexer: '"++[C# (currentChar# buf)]++"'") $
  case currentChar# buf of
      -- whitespace and comments, ignore.
    ' '#  -> lexIface cont (stepOn buf)
    '\t'# -> lexIface cont (stepOn buf)
    '\n'# -> \ loc -> lexIface cont (stepOn buf) (incSrcLine loc)

-- Numbers and comments
    '-'#  ->
      case lookAhead# buf 1# of
--        '-'# -> lex_comment cont (stepOnBy# buf 2#)
        c    -> 
	  if is_digit c
          then lex_num cont (negate) (ord# c -# ord# '0'#) (incLexeme (incLexeme buf))
	  else lex_sym cont buf

    '{'# ->				-- look for "{-##" special iface pragma
	case lookAhead# buf 1# of
	   '-'# -> case lookAhead# buf 2# of
		    '#'# -> case lookAhead# buf 3# of
				'#'# ->  
				   let (lexeme, buf') 
					  = doDiscard False (stepOnBy# buf 4#) in
				   cont (ITpragma lexeme) buf'
				_ ->  lex_nested_comment (lexIface cont) buf
	   	    _    -> cont ITocurly (stepOn buf)
			    -- lex_nested_comment (lexIface cont) buf
	   _ -> cont ITocurly (stepOn buf)

    -- special symbols ----------------------------------------------------
    '('# -> 
	 case prefixMatch (stepOn buf) "..)" of
	   Just buf' ->  cont ITdotdot (stepOverLexeme buf')
           Nothing ->
            case lookAhead# buf 1# of
	      '#'# -> cont IToubxparen (stepOnBy# buf 2#)
	      _    -> cont IToparen (stepOn buf)
    ')'# -> cont ITcparen (stepOn buf)
    '}'# -> cont ITccurly (stepOn buf)
    '#'# -> case lookAhead# buf 1# of
		')'# -> cont ITcubxparen (stepOnBy# buf 2#)
		_    -> lex_sym cont (incLexeme buf)
    '['# -> cont ITobrack (stepOn buf)
    ']'# -> cont ITcbrack (stepOn buf)
    ','# -> cont ITcomma  (stepOn buf)
    ';'# -> cont ITsemi   (stepOn buf)

    -- strings/characters -------------------------------------------------
    '\"'#{-"-} -> case untilEndOfString# (stepOn buf) of
	      buf' ->
		  -- the string literal does *not* include the dquotes
		case lexemeToFastString buf' of
		 v -> cont (ITstring v) (stepOn (stepOverLexeme buf'))

    '\''# -> --
	     -- untilEndOfChar# extends the current lexeme until
	     -- it hits a non-escaped single quote. The lexeme of the
             -- StringBuffer returned does *not* include the closing quote,
	     -- hence we augment the lexeme and make sure to add the
	     -- starting quote, before `read'ing the string.
	     --
	     case untilEndOfChar# (stepOn buf) of
	       buf' -> case reads ('\'':lexemeToString (incLexeme buf')) of
			[  (ch, rest)] -> cont (ITchar ch) (stepOverLexeme (incLexeme buf'))

    -- strictness and cpr pragmas and __scc treated specially.
    '_'# ->
	 case lookAhead# buf 1# of
	   '_'# -> case lookAhead# buf 2# of
	   	    'S'# -> 
			lex_demand cont (stepOnUntil (not . isSpace) 
		                        (stepOnBy# buf 3#)) -- past __S
	   	    'M'# -> 
			lex_cpr cont (stepOnUntil (not . isSpace) 
		                     (stepOnBy# buf 3#)) -- past __M
	   	    's'# -> 
			case prefixMatch (stepOnBy# buf 3#) "cc" of
		               Just buf' -> lex_scc cont (stepOverLexeme buf')
		     	       Nothing   -> lex_id cont buf
		    _ -> lex_id cont buf
	   _    -> lex_id cont buf

-- ``thingy'' form for casm
    '`'# ->
	    case lookAhead# buf 1# of
	      '`'# -> lex_cstring cont (stepOnBy# buf 2#) -- remove the `s and go.
	      _    -> lex_sym cont (incLexeme buf)         -- add ` to lexeme and assume
						     -- scanning an id of some sort.

    '\NUL'# ->
	    if bufferExhausted (stepOn buf) then
	       cont ITeof buf
	    else
	       trace "lexIface: misplaced NUL?" $ 
	       cont (ITunknown "\NUL") (stepOn buf)

    c | is_digit  c -> lex_num cont (id) (ord# c -# ord# '0'#) (incLexeme buf)
      | is_symbol c -> lex_sym cont buf
      | is_upper  c -> lex_con cont buf
      | is_ident  c -> lex_id  cont buf

--  where
lex_comment cont buf = 
--   _trace ("comment: "++[C# (currentChar# buf)]) $
   case untilChar# buf '\n'# of {buf' -> lexIface cont (stepOverLexeme buf')}

-------------------------------------------------------------------------------

lex_nested_comment cont buf =
  case currentChar# buf of
	'-'# -> case lookAhead# buf 1# of
		 '}'# -> cont (stepOnBy# buf 2#)
		 _    -> lex_nested_comment cont (stepOn buf)

	'{'# -> case lookAhead# buf 1# of
		 '-'# -> lex_nested_comment
				(lex_nested_comment cont) 
				(stepOnBy# buf 2#)
		 _    -> lex_nested_comment cont (stepOn buf)

	_   -> lex_nested_comment cont (stepOn buf)

-------------------------------------------------------------------------------

lex_demand cont buf = 
 case read_em [] buf of { (ls,buf') -> 
 case currentChar# buf' of
   'B'# -> cont (ITstrict (ls, True )) (stepOverLexeme (stepOn buf'))
   _    -> cont (ITstrict (ls, False)) (stepOverLexeme buf')
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

lex_cpr cont buf = 
 case read_em [] buf of { (cpr_inf,buf') -> 
   ASSERT ( null (tail cpr_inf) )
   cont (ITcprinfo $ head cpr_inf) (stepOverLexeme buf')
 }
 where
   -- code snatched from lex_demand above
  read_em acc buf = 
   case currentChar# buf of
    '-'# -> read_em (NoCPRInfo : acc) (stepOn buf)
    '('# -> do_unpack acc (stepOn buf)
    ')'# -> (reverse acc, stepOn buf)
    _    -> (reverse acc, buf)

  do_unpack acc buf
   = case read_em [] buf of
      (stuff, rest) -> read_em ((CPRInfo stuff)  : acc) rest

------------------
lex_scc cont buf =
 case currentChar# buf of
  'C'# -> cont ITsccAllCafs  (stepOverLexeme (stepOn buf))
  other -> cont ITscc buf

-----------
lex_num :: (IfaceToken -> IfM a) -> (Int -> Int) -> Int# -> IfM a
lex_num cont minus acc# buf =
 --trace ("lex_num: "++[C# (currentChar# buf)]) $
 case scanNumLit (I# acc#) buf of
     (acc',buf') ->
       case currentChar# buf' of
         '.'# ->
             -- this case is not optimised at all, as the
             -- presence of floating point numbers in interface
             -- files is not that common. (ToDo)
	    case expandWhile# is_digit (incLexeme buf') of
              buf2 -> -- points to first non digit char
		let l = case currentChar# buf2 of
		          'e'# -> let buf3 = incLexeme buf2 in
			      case currentChar# buf3 of
				'-'# -> expandWhile# is_digit (incLexeme buf3)
				_    -> expandWhile# is_digit buf3
		          _ -> buf2
		in let v = readRational__ (lexemeToString l) in
	           cont (ITrational v) (stepOverLexeme l)

         _ -> cont (ITinteger (fromInt (minus acc'))) (stepOverLexeme buf')

-----------
lex_cstring cont buf =
 case expandUntilMatch buf "\'\'" of
   buf' -> cont (ITstring (lexemeToFastString (setCurrentPos# buf' (negateInt# 2#))))
           (stepOverLexeme buf')	

------------------------------------------------------------------------------
-- Character Classes

is_ident, is_symbol, is_any, is_upper, is_digit :: Char# -> Bool

{-# INLINE is_ctype #-}
#if __GLASGOW_HASKELL__ >= 303
is_ctype :: Word8 -> Char# -> Bool
is_ctype mask = \c ->
   (indexWord8OffAddr (``char_types'' :: Addr) (ord (C# c)) .&. mask) /= 0
#else
is_ctype :: Int -> Char# -> Bool
is_ctype (I# mask) = \c ->
    let (A# ctype) = ``char_types'' :: Addr
	flag_word  = int2Word# (ord# (indexCharOffAddr# ctype (ord# c)))
    in
	(flag_word `and#` (int2Word# mask)) `neWord#` (int2Word# 0#)
#endif

is_ident  = is_ctype 1
is_symbol = is_ctype 2
is_any    = is_ctype 4
is_space  = is_ctype 8
is_upper  = is_ctype 16
is_digit  = is_ctype 32

-----------------------------------------------------------------------------
-- identifiers, symbols etc.

lex_id cont buf =
 case expandWhile# is_ident buf of { buf1 -> 
 case expandWhile# (eqChar# '#'#) buf1 of { buf' -> -- only if GHC extns on
 let new_buf = stepOverLexeme buf' 
     lexeme  = lexemeToFastString buf'
 in
 case _scc_ "Lex.haskellKeyword" lookupUFM haskellKeywordsFM lexeme of {
 	Just kwd_token -> --trace ("hkeywd: "++_UNPK_(lexeme)) $
			  cont kwd_token new_buf;
 	Nothing        -> 
 case lookupUFM ifaceKeywordsFM lexeme of {
 	Just kwd_token -> --trace ("ifacekeywd: "++_UNPK_(lexeme)) $
			  cont kwd_token new_buf;
 	Nothing        -> --trace ("id: "++_UNPK_(lexeme)) $
			  cont (mk_var_token lexeme) new_buf
 }}}}

lex_sym cont buf =
 case expandWhile# is_symbol buf of
   buf'
     | is_comment lexeme -> lex_comment cont new_buf
     | otherwise         ->
	   case lookupUFM haskellKeySymsFM lexeme of {
	 	Just kwd_token -> --trace ("keysym: "++unpackFS lexeme) $
				  cont kwd_token new_buf ;
	 	Nothing        -> --trace ("sym: "++unpackFS lexeme) $
				  cont (mk_var_token lexeme) new_buf
           }
   	where lexeme = lexemeToFastString buf'
	      new_buf = stepOverLexeme buf'

	      is_comment fs 
	        | len < 2   = False
		| otherwise = trundle 0
		  where
		   len = lengthFS fs
		   
		   trundle n | n == len  = True
			     | otherwise = indexFS fs n == '-' && trundle (n+1)

lex_con cont buf = 
 case expandWhile# is_ident buf of 	  { buf1 ->
 case expandWhile# (eqChar# '#'#) buf1 of { buf' ->
 case currentChar# buf' of
     '.'# -> munch hiFile
     '!'# -> munch hiBootFile
     _    -> just_a_conid
 
   where
    just_a_conid = --trace ("con: "++unpackFS lexeme) $
		   cont (ITconid lexeme) new_buf
    lexeme = lexemeToFastString buf'
    new_buf = stepOverLexeme buf'
    munch hif = lex_qid cont lexeme hif (stepOn new_buf) just_a_conid
 }}

lex_qid cont mod hif buf just_a_conid =
 case currentChar# buf of
  '['# -> 	-- Special case for []
    case lookAhead# buf 1# of
     ']'# -> cont (ITqconid  (mod,SLIT("[]"),hif)) (stepOnBy# buf 2#)
     _    -> just_a_conid

  '('# ->  -- Special case for (,,,)
	   -- This *is* necessary to deal with e.g. "instance C PrelBase.(,,)"
    case lookAhead# buf 1# of
     '#'# -> case lookAhead# buf 2# of
		','# -> lex_ubx_tuple cont mod hif (stepOnBy# buf 3#) 
				just_a_conid
		_    -> just_a_conid
     ')'# -> cont (ITqconid (mod,SLIT("()"),hif)) (stepOnBy# buf 2#)
     ','# -> lex_tuple cont mod hif (stepOnBy# buf 2#) just_a_conid
     _    -> just_a_conid

  '-'# -> case lookAhead# buf 1# of
            '>'# -> cont (ITqconid (mod,SLIT("->"),hif)) (stepOnBy# buf 2#)
            _    -> lex_id3 cont mod hif buf just_a_conid
  _    -> lex_id3 cont mod hif buf just_a_conid

lex_id3 cont mod hif buf just_a_conid
  | is_symbol c =
     case expandWhile# is_symbol buf of { buf' ->
     let
      lexeme  = lexemeToFastString buf'
      new_buf = stepOverLexeme buf'
     in
     case lookupUFM haskellKeySymsFM lexeme of {
	Just kwd_token -> just_a_conid;	-- avoid M.:: etc.
	Nothing        -> cont (mk_qvar_token mod hif lexeme) new_buf
     }}

  | otherwise   =
     case expandWhile# is_ident buf of { buf1 ->
     if emptyLexeme buf1 
    	    then just_a_conid
    	    else
     case expandWhile# (eqChar# '#'#) buf1 of { buf' -> -- only if GHC extns on
     let
      lexeme  = lexemeToFastString buf'
      new_buf = stepOverLexeme buf'
     in
     case _scc_ "Lex.haskellKeyword" lookupUFM haskellKeywordsFM lexeme of {
    	    Just kwd_token -> just_a_conid; -- avoid M.where etc.
    	    Nothing        -> 
     case lookupUFM ifaceKeywordsFM lexeme of {	-- only for iface files
    	    Just kwd_token -> just_a_conid;
    	    Nothing        -> cont (mk_qvar_token mod hif lexeme) new_buf
     }}}}
  where c = currentChar# buf

mk_var_token pk_str
  | is_upper f		= ITconid pk_str
 	-- _[A-Z] is treated as a constructor in interface files.
  | f `eqChar#` '_'# && not (_NULL_ tl) 
	&& (case _HEAD_ tl of { C# g -> is_upper g }) = ITconid pk_str
  | is_ident f		= ITvarid pk_str
  | f `eqChar#` ':'#	= ITconsym pk_str
  | otherwise		= ITvarsym pk_str
  where
      (C# f) = _HEAD_ pk_str
      tl     = _TAIL_ pk_str

mk_qvar_token m hif token =
 case mk_var_token token of
   ITconid n  -> ITqconid  (m,n,hif)
   ITvarid n  -> ITqvarid  (m,n,hif)
   ITconsym n -> ITqconsym (m,n,hif)
   ITvarsym n -> ITqvarsym (m,n,hif)
   _	      -> ITunknown (show token)
\end{code}

----------------------------------------------------------------------------
Horrible stuff for dealing with M.(,,,)

\begin{code}
lex_tuple cont mod hif buf back_off =
  go 2 buf
  where
   go n buf =
    case currentChar# buf of
      ','# -> go (n+1) (stepOn buf)
      ')'# -> cont (ITqconid (mod, snd (mkTupNameStr n),hif)) (stepOn buf)
      _    -> back_off

lex_ubx_tuple cont mod hif buf back_off =
  go 2 buf
  where
   go n buf =
    case currentChar# buf of
      ','# -> go (n+1) (stepOn buf)
      '#'# -> case lookAhead# buf 1# of
		')'# -> cont (ITqconid (mod, snd (mkUbxTupNameStr n), hif))
				 (stepOnBy# buf 2#)
	        _    -> back_off
      _    -> back_off
\end{code}

-----------------------------------------------------------------------------
Keyword Lists

\begin{code}
ifaceKeywordsFM :: UniqFM IfaceToken
ifaceKeywordsFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
     [  ("__interface",		ITinterface),
	("__export",		ITexport),
	("__instimport",	ITinstimport),
	("__forall",		ITforall),
	("__letrec",		ITletrec),
	("__coerce",		ITcoerce),
	("__inline",		ITinline),
	("__DEFAULT",		ITdefaultbranch),
	("__bot",		ITbottom),
	("__integer",		ITinteger_lit),
	("__float",		ITfloat_lit),
	("__rational",		ITrational_lit),
	("__addr",		ITaddr_lit),
	("__litlit",		ITlit_lit),
	("__string",		ITstring_lit),
	("__a",			ITtypeapp),
	("__A",			ITarity),
	("__P",			ITspecialise),
	("__C",			ITnocaf),
        ("__u",			ITunfold NoInlinePragInfo),
        ("__U",			ITunfold IWantToBeINLINEd),
        ("__UU",		ITunfold IMustBeINLINEd),
        ("__Unot",		ITunfold IMustNotBeINLINEd),
        ("__Ux",		ITunfold IAmALoopBreaker),
	
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

haskellKeywordsFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
       [( "case",	ITcase ),     
	( "class",	ITclass ),    
	( "data",	ITdata ),     
	( "default",	ITdefault ),  
	( "deriving",	ITderiving ), 
	( "do",		ITdo ),       
	( "else",	ITelse ),     
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
	( "then",	ITthen ),     
	( "type",	ITtype ),     
	( "where",	ITwhere )

--	These three aren't Haskell keywords at all
--	and 'as' is often used as a variable name
--	( "as",		ITas ),       
--	( "qualified",	ITqualified ),
--	( "hiding",	IThiding )

     ]

haskellKeySymsFM = listToUFM $
	map (\ (x,y) -> (_PK_ x,y))
      [ ("..",			ITdotdot)
       ,("::",			ITdcolon)
       ,("=",			ITequal)
       ,("\\",			ITlam)
       ,("|",			ITvbar)
       ,("<-",			ITlarrow)
       ,("->",			ITrarrow)
       ,("@",			ITat)
       ,("~",			ITtilde)
       ,("=>",			ITdarrow)
       ,("-",			ITminus)
       ,("!",			ITbang)
       ]
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
type IfM a = StringBuffer	-- Input string
	  -> SrcLoc
	  -> MaybeErr a {-error-}Message

returnIf   :: a -> IfM a
returnIf a s l = Succeeded a

thenIf	   :: IfM a -> (a -> IfM b) -> IfM b
m `thenIf` k = \s l ->
	case m s l of
		Succeeded a -> k a s l
		Failed err  -> Failed err

getSrcLocIf :: IfM SrcLoc
getSrcLocIf s l = Succeeded l

happyError :: IfM a
happyError s l = Failed (ifaceParseErr s l)


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
checkVersion :: Maybe Integer -> IfM ()
checkVersion mb@(Just v) s l
 | (v==0) || (v == fromInt opt_HiVersion) || opt_NoHiCheck = Succeeded ()
 | otherwise = Failed (ifaceVersionErr mb l ([]::[IfaceToken]){-Todo-})
checkVersion mb@Nothing  s l 
 | "hi-boot" `isSuffixOf` (_UNPK_ (srcLocFile l)) = Succeeded ()
 | otherwise = Failed (ifaceVersionErr mb l ([]::[IfaceToken]){-Todo-})

-----------------------------------------------------------------

ifaceParseErr :: StringBuffer -> SrcLoc -> Message
ifaceParseErr s l
  = hsep [ppr l, ptext SLIT("Interface-file parse error;"),
          ptext SLIT("current input ="), text first_bit]
  where
    first_bit = lexemeToString (stepOnBy# s 100#) 

ifaceVersionErr hi_vers l toks
  = hsep [ppr l, ptext SLIT("Interface file version error;"),
          ptext SLIT("Expected"), int opt_HiVersion, 
	  ptext SLIT("found "), pp_version]
    where
     pp_version =
      case hi_vers of
        Nothing -> ptext SLIT("pre ghc-3.02 version")
	Just v  -> ptext SLIT("version") <+> integer v

\end{code}
