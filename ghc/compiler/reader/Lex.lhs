%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Lexical analysis]{Lexical analysis}

\begin{code}
#include "HsVersions.h"

module Lex (

	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	mkTupNameStr, ifaceParseErr,

	-- Monad for parser
	IfaceToken(..), lexIface, SYN_IE(IfM), thenIf, returnIf, happyError,
	StringBuffer

    ) where


IMPORT_1_3(Char(isDigit, isAlpha, isAlphanum, isUpper,isLower, isSpace, ord))

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(Ubiq)
IMPORT_DELOOPER(IdLoop)    -- get the CostCentre type&constructors from here
#else
import {-# SOURCE #-} CostCentre
# if __GLASGOW_HASKELL__ == 202
import PrelBase ( Char(..) )
# endif
#endif

import CmdLineOpts	( opt_IgnoreIfacePragmas )
import Demand		( Demand(..) {- instance Read -} )
import UniqFM           ( UniqFM, listToUFM, lookupUFM)
import BasicTypes	( NewOrData(..), IfaceFlavour(..) )

#if __GLASGOW_HASKELL__ >= 202
import Maybes		( MaybeErr(..) )
#else
import Maybes		( Maybe(..), MaybeErr(..) )
#endif
import Pretty



import ErrUtils		( Error(..) )
import Outputable	( Outputable(..), PprStyle(..) )
import Util		( nOfThem, panic )

import FastString
import StringBuffer

#if __GLASGOW_HASKELL__ <= 201
import PreludeGlaST 
#else
import GlaExts
#endif
\end{code}

%************************************************************************
%*									*
\subsection{Lexical categories}
%*									*
%************************************************************************

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.  Normally applied as in e.g. @isCon
(getLocalName foo)@.

\begin{code}
isLexCon, isLexVar, isLexId, isLexSym, isLexConId, isLexConSym,
 isLexVarId, isLexVarSym  :: FAST_STRING -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------

isLexConId cs
  | _NULL_ cs	     = False
  | cs == SLIT("[]") = True
  | c  == '('	     = True	-- (), (,), (,,), ...
  | otherwise	     = isUpper c || isUpperISO c
  where					
    c = _HEAD_ cs

isLexVarId cs
  | _NULL_ cs	 = False
  | otherwise    = isLower c || isLowerISO c
  where
    c = _HEAD_ cs

isLexConSym cs
  | _NULL_ cs	= False
  | otherwise	= c  == ':'
	       || cs == SLIT("->")
  where
    c = _HEAD_ cs

isLexVarSym cs
  | _NULL_ cs = False
  | otherwise = isSymbolASCII c
	     || isSymbolISO c
  where
    c = _HEAD_ cs

-------------
isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
isSymbolISO   c = ord c `elem` (0xd7 : 0xf7 : [0xa1 .. 0xbf])
isUpperISO    (C# c#) = c# `geChar#` '\xc0'# && c# `leChar#` '\xde'# && c# `neChar#` '\xd7'#
--0xc0 <= oc && oc <= 0xde && oc /= 0xd7 where oc = ord c
isLowerISO    (C# c#) = c# `geChar#` '\xdf'# && c# `leChar#` '\xff'# && c# `neChar#` '\xf7'#
--0xdf <= oc && oc <= 0xff && oc /= 0xf7 where oc = ord c
\end{code}


%************************************************************************
%*									*
\subsection{Tuple strings -- ugh!}
%*									*
%************************************************************************

\begin{code}
mkTupNameStr 0 = SLIT("()")
mkTupNameStr 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr 2 = _PK_ "(,)"   -- not strictly necessary
mkTupNameStr 3 = _PK_ "(,,)"  -- ditto
mkTupNameStr 4 = _PK_ "(,,,)" -- ditto
mkTupNameStr n = _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")")
\end{code}



%************************************************************************
%*									*
\subsection{Data types}
%*									*
%************************************************************************

The token data type, fairly un-interesting except from two constructors,
@ITidinfo@ and @ITtype@, which are used to lazily lex id info (arity,
strictness, unfolding etc) and types for id decls. 

The Idea/Observation here is that the renamer needs to scan through
all of an interface file before it can continue. But only a fraction
of the information contained in the file turns out to be useful, so
delaying as much as possible of the scanning and parsing of an
interface file Makes Sense (Heap profiles of the compiler 
show at a reduction in heap usage by at least a factor of two,
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
  = ITinterface		-- keywords
  | ITusages
  | ITversions
  | ITexports
  | ITinstance_modules
  | ITinstances
  | ITfixities
  | ITdeclarations
  | ITpragmas
  | ITdata
  | ITtype
  | ITnewtype
  | ITderiving
  | ITclass
  | ITwhere
  | ITinstance
  | ITinfixl
  | ITinfixr
  | ITinfix
  | ITforall
  | ITbang		-- magic symbols
  | ITvbar
  | ITdcolon
  | ITcomma
  | ITdarrow
  | ITdotdot
  | ITequal
  | ITocurly
  | ITobrack
  | IToparen
  | ITrarrow
  | ITccurly
  | ITcbrack
  | ITcparen
  | ITsemi
  | ITvarid   FAST_STRING
  | ITconid   FAST_STRING
  | ITvarsym  FAST_STRING
  | ITconsym  FAST_STRING
  | ITqvarid  (FAST_STRING,FAST_STRING,IfaceFlavour)
  | ITqconid  (FAST_STRING,FAST_STRING,IfaceFlavour)
  | ITqvarsym (FAST_STRING,FAST_STRING,IfaceFlavour)
  | ITqconsym (FAST_STRING,FAST_STRING,IfaceFlavour)

  | ITtysig StringBuffer (Maybe StringBuffer)
			   -- lazily return the stream of tokens for
		           -- the info attached to an id.
	-- Stuff for reading unfoldings
  | ITarity 
  | ITunfold Bool		-- True <=> there's an INLINE pragma on this Id
  | ITstrict [Demand] | ITbottom
  | ITlam | ITbiglam | ITcase | ITprim_case | ITlet | ITletrec | ITin | ITof
  | ITcoerce_in | ITcoerce_out | ITatsign
  | ITccall (Bool,Bool)		-- (is_casm, may_gc)
  | ITscc CostCentre 
  | ITchar Char | ITstring FAST_STRING
  | ITinteger Integer | ITdouble Double
  | ITinteger_lit | ITfloat_lit | ITrational_lit | ITaddr_lit | ITlit_lit | ITstring_lit
  | ITunknown String		-- Used when the lexer can't make sense of it
  | ITeof				-- end of file token
  deriving Text -- debugging

instance Text CostCentre -- cheat!

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
--  _trace ("Lexer: "++[C# (currentChar# buf)]) $
  case currentChar# buf of
      -- whitespace and comments, ignore.
    ' '#  -> lexIface cont (stepOn buf)
    '\t'# -> lexIface cont (stepOn buf)
    '\n'# -> \line -> lexIface cont (stepOn buf) (line+1)

-- Numbers and comments
    '-'#  ->
      case lookAhead# buf 1# of
        '-'# -> lex_comment cont (stepOnBy# buf 2#)
        c    -> 
	  if isDigit (C# c)
          then lex_num cont (negate) (ord# c -# ord# '0'#) (incLexeme (incLexeme buf))
	  else lex_id cont buf

-- Leave out nested comments for now; danger of finding { and - juxtaposed by mistake?
--    '{' : '-' : cs -> lex_nested_comment 1{-one seen-} cs

    '('# -> 
	 case prefixMatch (stepOn buf) "..)" of
	   Just buf' ->  cont ITdotdot (stepOverLexeme buf')
           Nothing ->
            case lookAhead# buf 1# of
              ','# -> lex_tuple cont Nothing  (stepOnBy# buf 2#)
              ')'# -> cont (ITconid SLIT("()")) (stepOnBy# buf 2#)
	      _    -> cont IToparen (stepOn buf)

    '{'# -> cont ITocurly (stepOn buf)
    '}'# -> cont ITccurly (stepOn buf)
    ')'# -> cont ITcparen (stepOn buf)
    '['# -> 
      case lookAhead# buf 1# of
	']'# -> cont (ITconid SLIT("[]")) (stepOnBy# buf 2#)
        _    -> cont ITobrack (stepOn buf)
    ']'# -> cont ITcbrack (stepOn buf)
    ','# -> cont ITcomma  (stepOn buf)
    ':'# -> case lookAhead# buf 1# of
              ':'# -> cont ITdcolon (stepOnBy# buf 2#)
              _    -> lex_id cont (incLexeme buf)
    ';'#  -> cont ITsemi (stepOn buf)
    '\"'# -> case untilEndOfString# (stepOn buf) of
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

-- ``thingy'' form for casm
    '`'# ->
	    case lookAhead# buf 1# of
	      '`'# -> lex_cstring cont (stepOnBy# buf 2#) -- remove the `` and go.
	      _    -> lex_id cont (incLexeme buf)         -- add ` to lexeme and assume
						     -- scanning an id of some sort.
-- Keywords
    '_'# ->
	 case lookAhead# buf 1# of
	   'S'# -> case lookAhead# buf 2# of
		    '_'# ->
		            lex_demand cont (stepOnUntil (not . isSpace) 
		                            (stepOnBy# buf 3#)) -- past _S_
	   's'# -> case prefixMatch (stepOnBy# buf 2#) "cc_" of
		     Just buf' -> lex_scc cont (stepOnUntil (not . isSpace) (stepOverLexeme buf'))
		     Nothing   -> lex_keyword cont (stepOnBy# buf 1#) -- drop the '_' and assume
								 -- it is a keyword.
	   _    -> lex_keyword cont (stepOn buf)

    '\NUL'# ->
	    if bufferExhausted (stepOn buf) then
	       cont ITeof buf
	    else
	       lex_id cont buf
    c ->
	if isDigit (C# c) then
	   lex_num cont (id) (ord# c -# ord# '0'#) (incLexeme buf)
        else
	   lex_id cont buf
--  where
lex_comment cont buf = 
--   _trace ("comment: "++[C# (currentChar# buf)]) $
   case untilChar# buf '\n'# of {buf' -> lexIface cont (stepOverLexeme buf')}

------------------
lex_demand cont buf = 
-- _trace ("demand: "++[C# (currentChar# buf)]) $
 case read_em [] buf of { (ls,buf') -> cont (ITstrict ls) (stepOverLexeme buf')}
 where
   -- code snatched from Demand.lhs
  read_em acc buf = 
--   _trace ("read_em: "++[C# (currentChar# buf)]) $
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
-- _trace ("scc: "++[C# (currentChar# buf)]) $
 case currentChar# buf of
  '"'# ->
      -- YUCK^2
     case prefixMatch (stepOn buf) "NO_CC\"" of
      Just buf' -> cont (ITscc noCostCentre) (stepOverLexeme buf')
      Nothing -> 
       case prefixMatch (stepOn buf) "CURRENT_CC\"" of
        Just buf' -> cont (ITscc useCurrentCostCentre) (stepOverLexeme buf')
        Nothing   ->
         case prefixMatch (stepOn buf) "OVERHEAD\"" of
         Just buf' -> cont (ITscc overheadCostCentre) (stepOverLexeme buf')
         Nothing   ->
          case prefixMatch (stepOn buf) "DONT_CARE\"" of
           Just buf' -> cont (ITscc dontCareCostCentre) (stepOverLexeme buf')
           Nothing   ->
            case prefixMatch (stepOn buf) "SUBSUMED\"" of
             Just buf' -> cont (ITscc subsumedCosts) (stepOverLexeme buf')
             Nothing ->
              case prefixMatch (stepOn buf) "CAFs_in_...\"" of
               Just buf' -> cont (ITscc preludeCafsCostCentre) (stepOverLexeme buf')
               Nothing ->
                case prefixMatch (stepOn buf) "CC_CAFs_in_..." of
                 Just buf' ->
	          case untilChar# (stepOverLexeme buf') '\"'# of
	           buf'' -> cont (ITscc (mkAllCafsCC ({-module-}lexemeToFastString buf'') _NIL_)) (stepOn (stepOverLexeme buf''))
                 Nothing ->
                  case prefixMatch (stepOn buf) "DICTs_in_...\"" of
                   Just buf' -> cont (ITscc (preludeDictsCostCentre True)) (stepOverLexeme buf')
                   Nothing ->
                    case prefixMatch (stepOn buf) "CC_DICTs_in_..." of
                     Just buf' ->
	              case untilChar# (stepOverLexeme buf') '\"'# of
	               buf'' -> cont (ITscc (mkAllDictsCC (lexemeToFastString buf'') _NIL_ True)) 
	                        (stepOn (stepOverLexeme buf''))
                     Nothing ->
		      let
		       match_user_cc buf =
	                case untilChar# buf '/'# of
	                 buf' -> 
                          let mod_name = lexemeToFastString buf' in
--	                  case untilChar# (stepOn (stepOverLexeme buf')) '/'# of
--	                   buf'' -> 
--                            let grp_name = lexemeToFastString buf'' in
			    case untilChar# (stepOn (stepOverLexeme buf')) '\"'# of
			     buf'' ->
			       -- The label may contain arbitrary characters, so it
			       -- may have been escaped etc., hence we `read' it in to get
			       -- rid of these meta-chars in the string and then pack it (again.)
			       -- ToDo: do the same for module name (single quotes allowed in m-names).
			       -- BTW, the code in this module is totally gruesome..
			       let upk_label = _UNPK_ (lexemeToFastString buf'') in
			       case reads ('"':upk_label++"\"") of
				((cc_label,_):_) -> 
				    let cc_name = _PK_ cc_label in
				    (mkUserCC cc_name mod_name _NIL_{-grp_name-}, 
			             stepOn (stepOverLexeme buf''))
				_ -> 
				  trace ("trouble lexing scc label: " ++ upk_label ++ " , ignoring") 
				  (mkUserCC _NIL_ mod_name _NIL_{-grp_name-}, 
			           stepOn (stepOverLexeme buf''))
                      in
                      case prefixMatch (stepOn buf) "CAF:" of
                       Just buf' ->
			 case match_user_cc (stepOverLexeme buf') of
			  (cc, buf'') -> cont (ITscc (cafifyCC cc)) buf''
                       Nothing ->
	                 case match_user_cc (stepOn buf) of
			  (cc, buf'') -> cont (ITscc cc) buf''
  c -> cont (ITunknown [C# c]) (stepOn buf)


-----------
lex_num :: (IfaceToken -> IfM a) -> 
	(Int -> Int) -> Int# -> IfM a
lex_num cont minus acc# buf =
-- _trace ("lex_num: "++[C# (currentChar# buf)]) $
 case scanNumLit (I# acc#) buf of
     (acc',buf') ->
       case currentChar# buf' of
         '.'# ->
             -- this case is not optimised at all, as the
             -- presence of floating point numbers in interface
             -- files is not that common. (ToDo)
	    case expandWhile (isDigit) (incLexeme buf') of
              buf'' -> -- points to first non digit char
		case reads (lexemeToString buf'') of
	          [(v,_)] -> cont (ITdouble v) (stepOverLexeme buf'')
         _ -> cont (ITinteger (fromInt (minus acc'))) (stepOverLexeme buf')

--	   case reads (lexemeToString buf') of
--	     [(i,_)] -> cont (ITinteger i) (stepOverLexeme buf')

------------
lex_keyword cont buf =
-- _trace ("lex_keyword: "++[C# (currentChar# buf)]) $
 case currentChar# buf of
  ':'# -> case lookAhead# buf 1# of
	    '_'# -> -- a binding, type (and other id-info) follows,
		    -- to make the parser ever so slightly, we push
		    -- 
		lex_decl cont (stepOnBy# buf 2#)
	    v# -> cont (ITunknown (['_',':',C# v#])) (stepOnBy# buf 2#)
  _ ->
    case expandWhile (is_kwd_char) buf of
     buf' ->
      let kw = lexemeToFastString buf' in
--    _trace ("kw: "++lexemeToString buf') $
      case lookupUFM ifaceKeywordsFM kw of
       Nothing -> cont (ITunknown (_UNPK_ kw)) -- (minor) sigh 
		  (stepOverLexeme buf')
       Just xx -> cont xx (stepOverLexeme buf')

lex_decl cont buf =
 case doDiscard False buf of -- spin until ;; is found
   buf' ->
      {- _trace (show (lexemeToString buf')) $ -}
      case currentChar# buf' of
       '\n'# -> -- newline, no id info.
	   cont (ITtysig (lexemeToBuffer (decLexeme buf')) Nothing)
		(stepOverLexeme buf')
       '\r'# -> -- just to be sure for those Win* boxes..
	   cont (ITtysig (lexemeToBuffer (decLexeme buf')) Nothing)
	   	(stepOverLexeme buf')
       '\NUL'# ->
	   cont (ITtysig (lexemeToBuffer (decLexeme buf')) Nothing)
	   	(stepOverLexeme buf')
       c     -> -- run all over the id info
	 case doDiscard False (stepOverLexeme buf') of -- spin until ;; is found (outside a string!)
	   buf'' -> 
		    --_trace ((C# c):show (lexemeToString (decLexeme buf')))  $
		    --_trace (show (lexemeToString (decLexeme buf''))) $
		    let idinfo = 
			    if opt_IgnoreIfacePragmas then
				Nothing
			    else
				Just (lexemeToBuffer (decLexeme buf''))
		        --_trace (show is) $
		    in
                    cont (ITtysig (lexemeToBuffer (decLexeme buf')) idinfo)
			(stepOverLexeme buf'')
		    
-- ToDo: hammer!
is_kwd_char c@(C# c#) = 
 isAlphanum c || -- OLD: c `elem` "_@/\\"
 (case c# of
   '_'#  -> True
   '@'#  -> True
   '/'#  -> True
   '\\'# -> True
   _     -> False)



-----------
lex_cstring cont buf =
-- _trace ("lex_cstring: "++[C# (currentChar# buf)]) $
 case expandUntilMatch buf "\'\'" of
   buf' -> cont (ITstring (lexemeToFastString (setCurrentPos# buf' (negateInt# 2#))))
           (stepOverLexeme buf')
	
-----------
lex_tuple cont module_dot buf =
-- _trace ("lex_tuple: "++[C# (currentChar# buf)]) $
  go 2 buf
  where
   go n buf =
    case currentChar# buf of
      ','# -> go (n+1) (stepOn buf)
      ')'# -> end_lex_id cont module_dot (ITconid (mkTupNameStr n)) (stepOn buf)
      _    -> cont (ITunknown ("tuple " ++ show n)) buf

-- Similarly ' itself is ok inside an identifier, but not at the start

id_arr :: _ByteArray Int
id_arr =
 unsafePerformPrimIO (
  newCharArray (0,255) `thenPrimIO` \ barr ->
  let
   loop 256# = returnPrimIO ()
   loop i# =
    if isAlphanum (C# (chr# i#)) || is_sym (chr# i#) then
       writeCharArray barr (I# i#) '\1' `seqPrimIO`
       loop (i# +# 1#)
    else
       writeCharArray barr (I# i#) '\0' `seqPrimIO`
       loop (i# +# 1#)
  in
  loop 0#                    `seqPrimIO`
  unsafeFreezeByteArray barr)

is_id_char (C# c#) = 
 let
  _ByteArray _ arr# = id_arr
 in
 case ord# (indexCharArray# arr# (ord# c#)) of
  0# -> False
  1# -> True

--is_id_char c@(C# c#)  = isAlphanum c || is_sym c#

is_sym c#=
 case c# of {
   ':'# -> True; '_'#  -> True; '\''# -> True; '!'# -> True; 
   '#'# -> True; '$'#  -> True; ':'#  -> True; '%'# -> True; 
   '&'# -> True; '*'#  -> True; '+'#  -> True; '.'# -> True; 
   '/'# -> True; '<'#  -> True; '='#  -> True; '>'# -> True; 
   '?'# -> True; '\\'# -> True; '^'#  -> True; '|'# -> True; 
   '-'# -> True; '~'#  -> True; '@'#  -> True; _    -> False }

--isAlphanum c || c `elem` ":_'!#$%&*+./<=>?@\\^|-~" -- ToDo: add ISOgraphic


mod_arr :: _ByteArray Int
mod_arr =
 unsafePerformPrimIO (
  newCharArray (0,255) `thenPrimIO` \ barr ->
  let
   loop 256# = returnPrimIO ()
   loop i# =
    if isAlphanum (C# (chr# i#)) || i# ==# (ord# '_'#) || i# ==# (ord# '\''#) then
       writeCharArray barr (I# i#) '\1' `seqPrimIO`
       loop (i# +# 1#)
    else
       writeCharArray barr (I# i#) '\0' `seqPrimIO`
       loop (i# +# 1#)
  in
  loop 0#                    `seqPrimIO`
  unsafeFreezeByteArray barr)

             
is_mod_char (C# c#) = 
 let
  _ByteArray _ arr# = mod_arr
 in
 case ord# (indexCharArray# arr# (ord# c#)) of
  0# -> False
  1# -> True

--isAlphanum c || c == '_' || c== '\'' --`elem` "_'"

{-
lex_id cs = 
 case _scc_ "lex_id.span" my_span' (is_mod_char) cs of
   (xs, len, cs') ->
    case cs' of
     [] -> case xs of
	    [] -> lex_id2 Nothing cs
	    _  -> lex_id3 Nothing len xs cs

     '.':cs'' ->
        case xs of
	  [] -> lex_id2 Nothing cs
	  _  ->
           let
            pk_str = _PK_ (xs::String)
            len = lengthPS pk_str
           in
           if len==len+1 then
              error "Well, I never!"
           else
              lex_id2 (Just pk_str) cs''
     _ -> case xs of
	    [] -> lex_id2 Nothing cs
	    _  -> lex_id3 Nothing len xs cs'

-}

lex_id cont buf = 
-- _trace ("lex_id: "++[C# (currentChar# buf)]) $
 case expandWhile (is_mod_char) buf of
   buf' ->
    case currentChar# buf' of
     '.'# -> munch buf' HiFile
     '!'# -> munch buf' HiBootFile
     _    -> lex_id2 cont Nothing buf'
   where
    munch buf' hif = 
	if not (emptyLexeme buf') then
--	   _trace ("lex_id: "++(C# (currentChar# (stepOverLexeme buf'))):show (lexemeToFastString buf')) $ 
	   case lexemeToFastString buf' of
	     l@(FastString u# l# ba#) -> lex_id2 cont (Just (FastString u# l# ba#, hif)) 
	     					 (stepOn (stepOverLexeme buf'))
	else
	   lex_id2 cont Nothing buf'		
	

-- Dealt with the Module.part
lex_id2 cont module_dot buf =
-- _trace ("lex_id2: "++[C# (currentChar# buf)]) $
 case currentChar# buf of
  '['# -> 
    case lookAhead# buf 1# of
     ']'# -> end_lex_id cont module_dot (ITconid SLIT("[]")) (stepOnBy# buf 2#)
     _    -> lex_id3 cont module_dot buf
  '('# ->
    case lookAhead# buf 1# of
     ')'# -> end_lex_id cont module_dot (ITconid SLIT("()")) (stepOnBy# buf 2#)
     ','# -> lex_tuple cont module_dot (stepOnBy# buf 2#)
     _    -> lex_id3 cont module_dot buf
  ':'# -> lex_id3 cont module_dot (incLexeme buf)
  '-'# ->
     case module_dot of
       Nothing  -> lex_id3 cont module_dot buf
       Just ghc -> -- this should be "GHC" (current home of (->))
         case lookAhead# buf 1# of
          '>'# -> end_lex_id cont module_dot (ITconid SLIT("->")) 
			(stepOnBy# buf 2#)
          _    -> lex_id3 cont module_dot buf
  _    -> lex_id3 cont module_dot buf



-- Dealt with [], (), : special cases

lex_id3 cont module_dot buf =
-- _trace ("lex_id3: "++[C# (currentChar# buf)]) $
 case expandWhile (is_id_char) buf of
  buf' ->
    case module_dot of
     Just _ ->
       end_lex_id cont module_dot (mk_var_token lexeme) (stepOverLexeme buf')
     Nothing ->
       case _scc_ "Lex.haskellKeyword" lookupUFM haskellKeywordsFM lexeme of
         Just kwd_token -> cont kwd_token new_buf
	 Nothing        -> cont (mk_var_token lexeme) new_buf
    where
     lexeme  = lexemeToFastString buf'
     new_buf = stepOverLexeme buf'


{- OLD:
lex_id2 module_dot [] ('[': ']': cs) = end_lex_id module_dot (ITconid SLIT("[]")) cs
lex_id2 module_dot [] ('(': ')': cs) = end_lex_id module_dot (ITconid SLIT("()")) cs
lex_id2 module_dot [] ('(': ',': cs) = lex_tuple module_dot cs
lex_id2 module_dot [] (':' : cs)     = lex_id3 module_dot [':'] cs
lex_id2 module_dot xs cs 	     = lex_id3 module_dot xs cs
-}

-- Dealt with [], (), : special cases

{-
lex_id3 module_dot len_xs xs cs =
 case my_span' (is_id_char) cs of
   (xs1,len_xs1,rest) ->
    case module_dot of
     Just m  -> end_lex_id (Just m) (mk_var_token rxs) rest --OLD:_PK_ (reverse xs))) rest
     Nothing -> 
      case _scc_ "Lex.haskellKeyword" lookupUFM haskellKeywordsFM rxs of
       Just kwd_token -> kwd_token	    : lexIface rest
       other 	      -> token : lexIface cs end_lex_id Nothing (mk_var_token rxs) rest
    where
     rxs = packNChars (len_xs+len_xs1) (xs++xs1) -- OLD: _PK_ (reverse xs)
-}
mk_var_token pk_str =
     let
      f = _HEAD_ pk_str
     in
     --
     -- These tests assume a non- ISO-8859-1 impl of isUpper&isLower,
     -- remove the second half of disjunction when using a 1.3 prelude.
     --
     if      isUpper f    then ITconid pk_str
     else if isLower f	  then ITvarid pk_str
     else if f == ':'	  then ITconsym pk_str
     else if isLowerISO f then ITvarid pk_str
     else if isUpperISO f then ITconid pk_str
     else ITvarsym pk_str

{-
    mk_var_token xs@(f:_) | isUpper f || isUpperISO f = ITconid n
		    	  | f == ':'		  = ITconsym n
		      	  | isAlpha f		  = ITvarid n
		    	  | otherwise		  = ITvarsym n 
		where
		      n = _PK_ xs
-}
			    
end_lex_id cont Nothing token buf  = cont token buf
end_lex_id cont (Just (m,hif)) token buf =
 case token of
   ITconid n  -> cont (ITqconid  (m,n,hif))         buf
   ITvarid n  -> cont (ITqvarid  (m,n,hif))         buf
   ITconsym n -> cont (ITqconsym (m,n,hif))         buf
   ITvarsym n -> cont (ITqvarsym (m,n,hif))         buf
   ITbang     -> cont (ITqvarsym (m,SLIT("!"),hif)) buf
   _	      -> cont (ITunknown (show token))      buf

------------
ifaceKeywordsFM :: UniqFM IfaceToken
ifaceKeywordsFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
       [("/\\_",		ITbiglam)
       ,("@_",			ITatsign)
       ,("letrec_",		ITletrec)
       ,("interface_",	 	ITinterface)
       ,("usages_",		ITusages)
       ,("versions_",		ITversions)
       ,("exports_",		ITexports)
       ,("instance_modules_",	ITinstance_modules)
       ,("instances_",		ITinstances)
       ,("fixities_",		ITfixities)
       ,("declarations_",	ITdeclarations)
       ,("pragmas_",		ITpragmas)
       ,("forall_",		ITforall)
       ,("U_",			ITunfold False)
       ,("U!_",			ITunfold True)
       ,("A_",			ITarity)
       ,("coerce_in_",		ITcoerce_in)
       ,("coerce_out_",		ITcoerce_out)
       ,("bot_",		ITbottom)
       ,("integer_",		ITinteger_lit)
       ,("rational_",		ITrational_lit)
       ,("addr_",		ITaddr_lit)
       ,("float_",		ITfloat_lit)
       ,("string_",		ITstring_lit)
       ,("litlit_",		ITlit_lit)
       ,("ccall_",		ITccall (False, False))
       ,("ccall_GC_",		ITccall (False, True))
       ,("casm_",		ITccall (True,  False))
       ,("casm_GC_",		ITccall (True,  True))
       ]

haskellKeywordsFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
      [ ("data",		ITdata)
       ,("type",		ITtype)
       ,("newtype",		ITnewtype)
       ,("class",		ITclass)
       ,("where",		ITwhere)
       ,("instance",		ITinstance)
       ,("infixl",		ITinfixl)
       ,("infixr",		ITinfixr)
       ,("infix",		ITinfix)
       ,("case",		ITcase)
       ,("case#",		ITprim_case)
       ,("of",			ITof)
       ,("in",			ITin)
       ,("let",			ITlet)
       ,("deriving",		ITderiving)

       ,("->",			ITrarrow)
       ,("\\",			ITlam)
       ,("|",			ITvbar)
       ,("!",			ITbang)
       ,("=>",			ITdarrow)
       ,("=",			ITequal)
       ]


-- doDiscard rips along really fast looking for a double semicolon, 
-- indicating the end of the pragma we're skipping
doDiscard inStr buf =
-- _trace (show (C# (currentChar# buf))) $
 case currentChar# buf of
   ';'# ->
     if not inStr then
       case lookAhead# buf 1# of
        ';'# -> incLexeme (incLexeme buf)
        _    -> doDiscard inStr (incLexeme buf)
     else
       doDiscard inStr (incLexeme buf)
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

begin{code}
my_span :: (a -> Bool) -> [a] -> ([a],[a])
my_span p xs = go [] xs
  where
    go so_far (x:xs') | p x = go (x:so_far) xs'
    go so_far xs            = (reverse so_far, xs)

my_span' :: (a -> Bool) -> [a] -> ([a],Int,[a])
my_span' p xs = go [] 0 xs
  where
    go so_far n (x:xs') | p x = go (x:so_far) (n+1) xs'
    go so_far n xs            = (reverse so_far,n, xs)
end{code}


%************************************************************************
%*									*
\subsection{Other utility functions
%*									*
%************************************************************************

\begin{code}
type IfM a = StringBuffer -> Int -> MaybeErr a Error

returnIf   :: a -> IfM a
returnIf a s l = Succeeded a

thenIf	   :: IfM a -> (a -> IfM b) -> IfM b
m `thenIf` k = \s l ->
	case m s l of
		Succeeded a -> k a s l
		Failed err  -> Failed err

happyError :: IfM a
happyError s l = Failed (ifaceParseErr l ([]::[IfaceToken]){-Todo-})

-----------------------------------------------------------------

ifaceParseErr l toks sty
  = hsep [ptext SLIT("Interface-file parse error: line"), int l, ptext SLIT("toks="), text (show (take 10 toks))]
\end{code}
