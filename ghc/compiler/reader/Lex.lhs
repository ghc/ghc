%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Lexical analysis]{Lexical analysis}

\begin{code}
#include "HsVersions.h"

module Lex (

	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	mkTupNameStr,

	-- Monad for parser
	IfaceToken(..), lexIface, SYN_IE(IfM), thenIf, returnIf, happyError

    ) where


IMPORT_1_3(Char(isDigit, isAlpha, isAlphanum, isUpper))

import Demand		( Demand {- instance Read -} )
import FiniteMap	( FiniteMap, listToFM, lookupFM )
import Maybes		( Maybe(..), MaybeErr(..) )
import Pretty
import CharSeq		( CSeq )
import ErrUtils		( Error(..) )
import Outputable	( Outputable(..) )
import PprStyle		( PprStyle(..) )
import Util		( nOfThem, panic )

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
isUpperISO    c = 0xc0 <= oc && oc <= 0xde && oc /= 0xd7 where oc = ord c
isLowerISO    c = 0xdf <= oc && oc <= 0xff && oc /= 0xf7 where oc = ord c
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
  | ITdccurly
  | ITdocurly
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
  | ITqvarid  (FAST_STRING,FAST_STRING)
  | ITqconid  (FAST_STRING,FAST_STRING)
  | ITqvarsym (FAST_STRING,FAST_STRING)
  | ITqconsym (FAST_STRING,FAST_STRING)

	-- Stuff for reading unfoldings
  | ITarity | ITstrict | ITunfold
  | ITdemand [Demand] | ITbottom
  | ITlam | ITbiglam | ITcase | ITprim_case | ITlet | ITletrec | ITin | ITof
  | ITcoerce_in | ITcoerce_out | ITatsign
  | ITccall (Bool,Bool)		-- (is_casm, may_gc)

  | ITchar Char | ITstring FAST_STRING
  | ITinteger Integer | ITdouble Double
  | ITinteger_lit | ITfloat_lit | ITrational_lit | ITaddr_lit | ITlit_lit | ITstring_lit
  deriving Text -- debugging
\end{code}

%************************************************************************
%*									*
\subsection{The lexical analyser}
%*									*
%************************************************************************

\begin{code}
lexIface :: String -> [IfaceToken]

lexIface input
  = _scc_ "Lexer"
    case input of
      []    -> []

      -- whitespace and comments
      ' '	: cs -> lexIface cs
      '\t'	: cs -> lexIface cs
      '\n'	: cs -> lexIface cs
      '-' : '-' : cs -> lex_comment cs

-- Leave out nested comments for now; danger of finding { and - juxtaposed by mistake?
--    '{' : '-' : cs -> lex_nested_comment 1{-one seen-} cs

      '(' : '.' : '.' : ')' : cs -> ITdotdot	: lexIface cs
      '{'		    : cs -> ITocurly	: lexIface cs
      '}'		    : cs -> ITccurly	: lexIface cs
      '(' : ',' 	    : cs -> lex_tuple Nothing cs 
      '(' : ')' 	    : cs -> ITconid SLIT("()")	: lexIface cs
      '(' 		    : cs -> IToparen	: lexIface cs
      ')'		    : cs -> ITcparen	: lexIface cs
      '[' : ']'		    : cs -> ITconid SLIT("[]")	: lexIface cs
      '['		    : cs -> ITobrack	: lexIface cs
      ']'		    : cs -> ITcbrack	: lexIface cs
      ','		    : cs -> ITcomma	: lexIface cs
      ':' : ':'		    : cs -> ITdcolon    : lexIface cs
      ';'		    : cs -> ITsemi	: lexIface cs
      '@'		    : cs -> ITatsign	: lexIface cs
      '\"'		    : cs -> case reads input of
					[(str, rest)] -> ITstring (_PK_ (str::String)) : lexIface rest
      '\''		    : cs -> case reads input of
					[(ch, rest)] -> ITchar ch : lexIface rest

-- ``thingy'' form for casm
      '`' : '`'		    : cs -> lex_cstring "" cs

-- Keywords
      '_' : 'S' : '_'	    : cs -> ITstrict	: lex_demand cs
      '_' 		    : cs -> lex_keyword cs

-- Numbers
      '-' : c : cs | isDigit c 	 -> lex_num "-" (c:cs)
      c       : cs | isDigit c 	 -> lex_num ""  (c:cs)
      
      other			 -> lex_id input
  where
    lex_comment str
      = case (span ((/=) '\n') str) of { (junk, rest) ->
	lexIface rest }

    ------------------
    lex_demand (c:cs) | isSpace c = lex_demand cs
		      | otherwise = case readList (c:cs) of
					((demand,rest) : _) -> ITdemand demand : lexIface rest

    -----------
    lex_num minus str
      = case (span isDigit str) of { (num, rest) ->
	case rest of 
	   '.' : str2 -> case (span isDigit str2) of { (num2,rest2) ->
			 ITdouble (read (minus ++ num ++ ('.':num2))) : lexIface rest2
			 }

	   other   -> ITinteger (read (minus ++ num)) : lexIface rest
	}

    ------------
    lex_keyword str
      = case (span is_kwd_mod_char str)    of { (kw, rest) ->
	case (lookupFM ifaceKeywordsFM kw) of
	  Nothing -> panic ("lex_keyword:"++str)
	  Just xx -> xx : lexIface rest
	}

    is_kwd_mod_char '_' = True
    is_kwd_mod_char c   = isAlphanum c

    -----------
    lex_cstring so_far ('\'' : '\'' : cs) = ITstring (_PK_ (reverse (so_far::String))) : lexIface cs
    lex_cstring so_far (c	    : cs) = lex_cstring (c:so_far) cs
	

    -----------
    lex_tuple module_dot orig_cs = go 2 orig_cs
		 where
		   go n (',':cs) = go (n+1) cs
		   go n (')':cs) = end_lex_id module_dot (ITconid (mkTupNameStr n)) cs
		   go n other    = panic ("lex_tuple" ++ orig_cs)

	-- NB: ':' isn't valid inside an identifier, only at the start.
	-- otherwise we get confused by a::t!
	-- Similarly ' itself is ok inside an identifier, but not at the start
    is_id_char c = isAlphanum c || c `elem` "_'!#$%&*+./<=>?@\\^|-~" -- ToDo: add ISOgraphic

    lex_id cs = go [] cs
	where
	  go xs (f  :cs) | is_kwd_mod_char f = go (f : xs) cs
	  go xs ('.':cs) | not (null xs)     = lex_id2 (Just (_PK_ (reverse xs))) [] cs
	  go xs cs			     = lex_id2 Nothing		   	  xs cs

	-- Dealt with the Module.part
    lex_id2 module_dot [] ('[': ']': cs) = end_lex_id module_dot (ITconid SLIT("[]")) cs
    lex_id2 module_dot [] ('(': ')': cs) = end_lex_id module_dot (ITconid SLIT("()")) cs
    lex_id2 module_dot [] ('(': ',': cs) = lex_tuple module_dot cs
    lex_id2 module_dot [] (':' : cs)     = lex_id3 module_dot [':'] cs
    lex_id2 module_dot xs cs 		 = lex_id3 module_dot xs cs

	-- Dealt with [], (), : special cases
    lex_id3 module_dot xs (f:cs) | is_id_char f = lex_id3 module_dot (f : xs) cs

    lex_id3 Nothing xs rest = case lookupFM haskellKeywordsFM rxs of
				       Just kwd_token -> kwd_token	    : lexIface rest
				       other 	      -> (mk_var_token rxs) : lexIface rest
			    where
			       rxs = reverse xs

    lex_id3 (Just m) xs rest = end_lex_id (Just m) (mk_var_token (reverse xs)) rest

    mk_var_token xs@(f:_) | isUpper f || isUpperISO f = ITconid n
		    	  | f == ':'		  = ITconsym n
		      	  | isAlpha f		  = ITvarid n
		    	  | otherwise		  = ITvarsym n 
		where
		      n = _PK_ xs
			    
    end_lex_id (Just m) (ITconid n)  cs = ITqconid (m,n) : lexIface cs
    end_lex_id (Just m) (ITvarid n)  cs = ITqvarid (m,n) : lexIface cs
    end_lex_id (Just m) (ITconsym n) cs = ITqconsym (m,n): lexIface cs
    end_lex_id (Just m) (ITvarsym n) cs = ITqvarsym (m,n): lexIface cs
    end_lex_id (Just m) ITbang	     cs = ITqvarsym (m,SLIT("!")) : lexIface cs
    end_lex_id (Just m) token	     cs = panic ("end_lex_id:" ++ show token)
    end_lex_id Nothing  token	     cs = token : lexIface cs

    ------------
    ifaceKeywordsFM :: FiniteMap String IfaceToken
    ifaceKeywordsFM = listToFM [
	("interface_",	 	ITinterface)
       ,("usages_",		ITusages)
       ,("versions_",		ITversions)
       ,("exports_",		ITexports)
       ,("instance_modules_",	ITinstance_modules)
       ,("instances_",		ITinstances)
       ,("fixities_",		ITfixities)
       ,("declarations_",	ITdeclarations)
       ,("pragmas_",		ITpragmas)
       ,("forall_",		ITforall)
       ,("U_",			ITunfold)
       ,("A_",			ITarity)
       ,("coerce_in_",		ITcoerce_in)
       ,("coerce_out_",		ITcoerce_out)
       ,("A_",			ITarity)
       ,("A_",			ITarity)
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

    haskellKeywordsFM = listToFM [
        ("data",		ITdata)
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
       ,("letrec",		ITletrec)
       ,("deriving",		ITderiving)

       ,("->",			ITrarrow)
       ,("\\",			ITlam)
       ,("/\\",			ITbiglam)
       ,("|",			ITvbar)
       ,("!",			ITbang)
       ,("=>",			ITdarrow)
       ,("=",			ITequal)
       ]
\end{code}


%************************************************************************
%*									*
\subsection{Other utility functions
%*									*
%************************************************************************

\begin{code}
type IfM a = MaybeErr a Error

returnIf   :: a -> IfM a
thenIf	   :: IfM a -> (a -> IfM b) -> IfM b
happyError :: Int -> [IfaceToken] -> IfM a

returnIf a = Succeeded a

thenIf (Succeeded a) k = k a
thenIf (Failed  err) _ = Failed err

happyError ln toks = Failed (ifaceParseErr ln toks)

-----------------------------------------------------------------

ifaceParseErr ln toks sty
  = ppCat [ppPStr SLIT("Interface-file parse error: line"), ppInt ln, ppStr "toks=", ppStr (show (take 10 toks))]
\end{code}
