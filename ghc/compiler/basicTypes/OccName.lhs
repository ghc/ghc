
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[OccName]{@OccName@}

\begin{code}
module OccName (
	-- Modules
	Module,		-- Abstract, instance of Outputable
	mkModule, mkModuleFS, moduleString, moduleCString, pprModule,

	-- The OccName type
	OccName, 	-- Abstract, instance of Outputable
	varOcc,    tcOcc,    tvOcc,	-- Occ constructors
	srcVarOcc, srcTCOcc, srcTvOcc,	-- For Occs arising from source code

	mkSuperDictSelOcc, mkDFunOcc, 
	mkDictOcc, mkWorkerOcc, mkDefaultMethodOcc,
 	mkClassTyConOcc, mkClassDataConOcc,
	
	isTvOcc, isTCOcc, isVarOcc, isConSymOcc, isConOcc, isSymOcc,
	isWildCardOcc, isAnonOcc, 
	pprOccName, occNameString, occNameFlavour, 

	-- The basic form of names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	isLowerISO, isUpperISO,
	
	-- Tidying up
	TidyOccEnv, emptyTidyOccEnv, tidyOccName, initTidyOccEnv,

	-- Junk	
 	identToC

    ) where

#include "HsVersions.h"

import Char	( isAlpha, isUpper, isLower, ISALPHANUM, ord )
import Util	( thenCmp )
import FiniteMap ( FiniteMap, emptyFM, lookupFM, addToFM, elemFM )
import Outputable
import GlaExts
\end{code}


%************************************************************************
%*									*
\subsection[Module]{The name of a module}
%*									*
%************************************************************************

\begin{code}
data Module = Module FAST_STRING	-- User and interface files
		     FAST_STRING	-- Print this in C files

	-- The C version has quote chars Z-encoded

instance Outputable Module where
  ppr = pprModule

instance Eq Module where
  (Module m1 _) == (Module m2 _) = m1 == m2

instance Ord Module where
  (Module m1 _) `compare` (Module m2 _) = m1 `compare` m2

pprModule :: Module -> SDoc
pprModule (Module real code) 
  = getPprStyle 	$ \ sty ->
    if codeStyle sty then
	ptext code
    else
	ptext real

mkModule :: String -> Module
mkModule s = Module (_PK_ s) (identToC s)

mkModuleFS :: FAST_STRING -> Module
mkModuleFS s = Module s (identFsToC s)

moduleString :: Module -> String
moduleString (Module mod _) = _UNPK_ mod

moduleCString :: Module -> String
moduleCString (Module _ code) = _UNPK_ code
\end{code}


%************************************************************************
%*									*
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
%*									*
%************************************************************************

\begin{code}
data OccName = OccName
		  OccSpace
		  FAST_STRING	-- The 'real name'
		  FAST_STRING	-- Print this in interface files
		  FAST_STRING	-- Print this in C/asm code

-- The OccSpace/real-name pair define the OccName
-- The iface and c/asm versions are simply derived from the
-- other two.  They are cached here simply to avoid recomputing
-- them repeatedly when printing

-- The latter two are irrelevant in RdrNames; on the other hand,
-- the OccSpace field is irrelevant after RdrNames.
-- So the OccName type might be refined a bit.  
-- It is now abstract so that's easier than before


-- Why three print-names?  
--	Real	Iface	C
--	---------------------	
--	foo	foo	foo
--
--	+	+	Zp	Operators OK in interface files;
--				'Z' is the escape char for C names
--
--	x#	x#	xZh	Trailing # lexed ok by GHC -fglasgow-exts
--
--	_foo	_ufoo	_ufoo	Leading '_' is the escape char in interface files
--
--	_vfoo	_vfoo	_vfoo	Worker for foo
--
--	_wp	_wp	_wp	Worker for +


data OccSpace = VarOcc  -- Variables and data constructors
	      | TvOcc	-- Type variables
	      | TCOcc	-- Type constructors and classes
	      deriving( Eq, Ord )
\end{code}


%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************
 
\begin{code}
instance Outputable OccName where
  ppr = pprOccName

pprOccName :: OccName -> SDoc
pprOccName (OccName space real iface code)
  = getPprStyle $ \ sty ->
    if codeStyle sty then
	ptext code
    else if ifaceStyle sty then
	ptext iface
    else
	ptext real
\end{code}


%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************
 
*Source-code* things beginning with '_' are zapped to begin with '_u'

\begin{code}
mkSrcOcc :: OccSpace -> FAST_STRING -> OccName
mkSrcOcc occ_sp real
  = case _UNPK_ real of

	'_' : rest -> OccName occ_sp real (_PK_ zapped_str) (identToC zapped_str)
		   where
		      zapped_str = '_' : 'u' : rest

	other	   -> OccName occ_sp real real (identFsToC real)

srcVarOcc, srcTCOcc, srcTvOcc :: FAST_STRING -> OccName
srcVarOcc = mkSrcOcc VarOcc
srcTCOcc  = mkSrcOcc TCOcc
srcTvOcc  = mkSrcOcc TvOcc
\end{code}

However, things that don't come from Haskell source code aren't
treated specially.  

\begin{code}
mkOcc :: OccSpace -> String -> OccName
mkOcc occ_sp str = OccName occ_sp fs fs (identToC str)
		 where
		   fs = _PK_ str

mkFsOcc :: OccSpace -> FAST_STRING -> OccName
mkFsOcc occ_sp real = OccName occ_sp real real (identFsToC real)

varOcc, tcOcc, tvOcc :: FAST_STRING -> OccName
varOcc = mkFsOcc VarOcc
tcOcc  = mkFsOcc TCOcc
tvOcc  = mkFsOcc TvOcc
\end{code}


%************************************************************************
%*									*
\subsection{Making system names}
%*									*
%************************************************************************

Here's our convention for splitting up the interface file name space:

	_d...		dictionary identifiers

	_f...		dict-fun identifiers (from inst decls)
	_g...		ditto, when the tycon has symbols

	_t...		externally visible (non-user visible) names

	_m...		default methods
	_n...		default methods (encoded symbols, eg. <= becomes _nle)

	_p...		superclass selectors

	_v...		workers
	_w...		workers (encoded symbols)

	_x...		local variables

	_u...		user-defined names that previously began with '_'

	_T...		compiler-generated tycons for dictionaries
	_D..		...ditto data cons

	__....		keywords (__export, __letrec etc.)

This knowledge is encoded in the following functions.




@mkDerivedOcc@ generates an @OccName@ from an existing @OccName@;
	eg: workers, derived methods

We pass a character to use as the prefix.  So, for example, 
	"f" gets derived to "_vf", if the prefix char is 'v'

\begin{code}
mk_deriv :: OccSpace -> Char -> String -> OccName
mk_deriv occ_sp sys_ch str = mkOcc occ_sp ('_' : sys_ch : str)
\end{code}

Things are a bit more complicated if the thing is an operator; then
we must encode it into a normal identifier first.  We do this in 
a simple way, and use a different character prefix (one after the one 
suggested).  For example
	"<" gets derived to "_wl", if the prefix char is 'v'

\begin{code}
mk_enc_deriv :: OccSpace
	     -> Char	-- The system-name-space character (see list above)
	     -> OccName	-- The OccName from which we are deriving
	     -> OccName

mk_enc_deriv occ_sp sys_ch occ
  | needs_encoding real_str = mk_deriv occ_sp sys_op_ch (encode_operator real_str)
  | otherwise 		    = mk_deriv occ_sp sys_ch    real_str
  where
    real_str  = occNameString occ
    sys_op_ch = succ sys_ch


mkDictOcc, mkWorkerOcc, mkDefaultMethodOcc,
 	   mkClassTyConOcc, mkClassDataConOcc
   :: OccName -> OccName

mkWorkerOcc        = mk_enc_deriv VarOcc 'v'	-- v,w
mkDefaultMethodOcc = mk_enc_deriv VarOcc 'm'	-- m,n
mkClassTyConOcc    = mk_enc_deriv TCOcc  'T'	-- not U
mkClassDataConOcc  = mk_enc_deriv VarOcc 'D'	-- not E
mkDictOcc	   = mk_enc_deriv VarOcc 'd'	-- not e
\end{code}

\begin{code}
mkSuperDictSelOcc :: Int 	-- Index of superclass, eg 3
		  -> OccName 	-- Class, eg "Ord"
		  -> OccName	-- eg "p3Ord"
mkSuperDictSelOcc index cls_occ
  = mk_deriv VarOcc 'p' (show index ++ occNameString cls_occ)
\end{code}


\begin{code}
mkDFunOcc :: OccName 	-- class, eg "Ord"
	  -> OccName 	-- tycon (or something convenient from the instance type)
			--	eg "Maybe"
	  -> Int	-- Unique to distinguish dfuns which share the previous two
			--	eg 3
	  -> OccName	-- "dOrdMaybe3"

mkDFunOcc cls_occ tycon_occ index
  | needs_encoding tycon_str  	-- Drat!  Have to encode the tycon
  = mk_deriv VarOcc 'g' (show_index ++ cls_str ++ encode_operator tycon_str)
  | otherwise			-- Normal case
  = mk_deriv VarOcc 'f' (show_index ++ cls_str ++ tycon_str)
  where
    cls_str   = occNameString cls_occ
    tycon_str = occNameString tycon_occ
	-- NB: if a non-operator the tycon has a trailing # we don't encode.
    show_index | index == 0 = ""
   	       | otherwise  = show index
\end{code}


%************************************************************************
%*									*
\subsection{Lexical categories}
%*									*
%************************************************************************

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.

\begin{code}
isLexCon,   isLexVar,    isLexId,    isLexSym    :: FAST_STRING -> Bool
isLexConId, isLexConSym, isLexVarId, isLexVarSym :: FAST_STRING -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------

isLexConId cs				-- Prefix type or data constructors
  | _NULL_ cs	     = False		-- 	e.g. "Foo", "[]", "(,)" 
  | cs == SLIT("[]") = True
  | c  == '('	     = True	-- (), (,), (,,), ...
  | otherwise	     = isUpper c || isUpperISO c
  where					
    c = _HEAD_ cs

isLexVarId cs				-- Ordinary prefix identifiers
  | _NULL_ cs	 = False		-- 	e.g. "x", "_x"
  | otherwise    = isLower c || isLowerISO c || c == '_'
  where
    c = _HEAD_ cs

isLexConSym cs				-- Infix type or data constructors
  | _NULL_ cs	= False			--	e.g. ":-:", ":", "->"
  | otherwise	= c  == ':'
	       || cs == SLIT("->")
  where
    c = _HEAD_ cs

isLexVarSym cs				-- Infix identifiers
  | _NULL_ cs = False			-- 	e.g. "+"
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
\subsection{Predicates and taking them apart}
%*									*
%************************************************************************

\begin{code} 
occNameString :: OccName -> String
occNameString (OccName _ s _ _) = _UNPK_ s

-- occNameFlavour is used only to generate good error messages, so it doesn't matter
-- that the VarOcc case isn't mega-efficient.  We could have different Occ constructors for
-- data constructors and values, but that makes everything else a bit more complicated.
occNameFlavour :: OccName -> String
occNameFlavour (OccName VarOcc s _ _) | isLexConId s = "Data constructor"
				      | otherwise    = "Value"
occNameFlavour (OccName TvOcc _ _ _)		     = "Type variable"
occNameFlavour (OccName TCOcc s _ _)  		     = "Type constructor or class"

isVarOcc, isTCOcc, isTvOcc,
 isConSymOcc, isSymOcc, isWildCardOcc :: OccName -> Bool

isVarOcc (OccName VarOcc _ _ _) = True
isVarOcc other                  = False

isTvOcc (OccName TvOcc _ _ _) = True
isTvOcc other                 = False

isTCOcc (OccName TCOcc _ _ _) = True
isTCOcc other                 = False

isConSymOcc (OccName _ s _ _) = isLexConSym s

isSymOcc (OccName _ s _ _) = isLexSym s

isConOcc (OccName _ s _ _) = isLexCon s

isWildCardOcc (OccName _ s _ _) = (_HEAD_ s) == '_' && _LENGTH_ s == 1 

isAnonOcc (OccName _ s _ _) = (_HEAD_ s) == '_'
\end{code}


%************************************************************************
%*									*
\subsection{Comparison}
%*									*
%************************************************************************
 
Comparison is done by space and 'real' name

\begin{code}
instance Eq OccName where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord OccName where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }

    compare (OccName sp1 r1 _ _) (OccName sp2 r2 _ _)
	= (sp1 `compare` sp2) `thenCmp` (r1 `compare` r2)
\end{code}


%************************************************************************
%*									*
\subsection{Tidying them up}
%*									*
%************************************************************************

Before we print chunks of code we like to rename it so that
we don't have to print lots of silly uniques in it.  But we mustn't
accidentally introduce name clashes!  So the idea is that we leave the
OccName alone unless it accidentally clashes with one that is already
in scope; if so, we tack on '1' at the end and try again, then '2', and
so on till we find a unique one.

There's a wrinkle for operators.  Consider '>>='.  We can't use '>>=1' 
because that isn't a single lexeme.  So we encode it to 'lle' and *then*
tack on the '1', if necessary.

\begin{code}
type TidyOccEnv = FiniteMap FAST_STRING Int	-- The in-scope OccNames
emptyTidyOccEnv = emptyFM

initTidyOccEnv :: [OccName] -> TidyOccEnv	-- Initialise with names to avoid!
initTidyOccEnv = foldl (\env (OccName _ fs _ _) -> addToFM env fs 1) emptyTidyOccEnv

tidyOccName :: TidyOccEnv -> OccName -> (TidyOccEnv, OccName)

tidyOccName in_scope occ@(OccName occ_sp real _ _)
  | not (real `elemFM` in_scope) &&
    not (isLexCon real)			-- Hack alert!   Specialised versions of overloaded
					-- constructors end up as ordinary Ids, but we don't
					-- want them as ConIds in interface files.

  = (addToFM in_scope real 1, occ)	-- First occurrence

  | otherwise				-- Already occurs
  = 	-- First encode, to deal with
	-- 	a) operators, and 
	--	b) trailing # signs
	-- so that we can then append '1', '2', etc
    go in_scope (encode_operator (_UNPK_ real))
  where

    go in_scope str = case lookupFM in_scope pk_str of
			Just n  -> go (addToFM in_scope pk_str (n+1)) (str ++ show n)
				-- Need to go round again, just in case "t3" (say) 
				-- clashes with a "t3" that's already in scope

			Nothing -> (addToFM in_scope pk_str 1, mkFsOcc occ_sp pk_str)
				-- str is now unique
		    where
		      pk_str = _PK_ str
\end{code}


%************************************************************************
%*									*
\subsection{Encoding for operators in derived names}
%*									*
%************************************************************************

See comments with mk_enc_deriv

\begin{code}
needs_encoding :: String -> Bool	-- Needs encoding when embedded in a derived name
					-- Just look at the first character
needs_encoding (c:cs) = not (isAlpha c || c == '_')

encode_operator :: String -> String
encode_operator nm = foldr tran "" nm
 where 
    tran c cs = case trChar c of
		   '\0'  -> '_' : show (ord c) ++ cs  -- No translation
		   tr_c  -> tr_c : cs

    trChar '&'  = 'a'
    trChar '|'  = 'b'
    trChar ':'  = 'c'
    trChar '/'  = 'd'
    trChar '='  = 'e'
    trChar '>'  = 'g'
    trChar '#'  = 'h'
    trChar '@'  = 'i'
    trChar '<'  = 'l'
    trChar '-'  = 'm'
    trChar '!'  = 'n'
    trChar '+'  = 'p'
    trChar '\'' = 'q'
    trChar '$'  = 'r'
    trChar '?'  = 's'
    trChar '*'  = 't'
    trChar '_'  = 'u'
    trChar '.'  = 'v'
    trChar '\\' = 'w'
    trChar '%'  = 'x'
    trChar '~'  = 'y'
    trChar '^'  = 'z'
    trChar _    = '\0'	-- No translation
\end{code}


%************************************************************************
%*									*
\subsection{The 'Z' encoding}
%*									*
%************************************************************************

We provide two interfaces for efficiency.

\begin{code}
identToC :: String -> FAST_STRING
identToC str
  | all ISALPHANUM str && not std = _PK_ str
  | std 			  = _PK_ ("Zs" ++ encode str)
  | otherwise			  = _PK_ (encode str)
  where
    std = has_std_prefix str

identFsToC :: FAST_STRING -> FAST_STRING
identFsToC fast_str
  | all ISALPHANUM str && not std = fast_str
  | std				  = _PK_ ("Zs" ++ encode str)
  | otherwise			  = _PK_ (encode str)
  where
    std = has_std_prefix str
    str = _UNPK_ fast_str

-- avoid "stdin", "stdout", and "stderr"...
has_std_prefix ('s':'t':'d':_) = True
has_std_prefix _	       = False

encode :: String -> String
encode [] = []
encode (c:cs) = encode_ch c ++ encode cs

encode_ch :: Char -> String
encode_ch c | ISALPHANUM c = [c]
	-- Common case first
encode_ch 'Z'  = "ZZ"
encode_ch '&'  = "Za"
encode_ch '|'  = "Zb"
encode_ch ':'  = "Zc"
encode_ch '/'  = "Zd"
encode_ch '='  = "Ze"
encode_ch '>'  = "Zg"
encode_ch '#'  = "Zh"
encode_ch '<'  = "Zl"
encode_ch '-'  = "Zm"
encode_ch '!'  = "Zn"
encode_ch '.'  = "Zs"
encode_ch '\'' = "Zq"
encode_ch '*'  = "Zt"
encode_ch '+'  = "Zp"
encode_ch '_'  = "_"
encode_ch c    = 'Z':show (ord c)
\end{code}

For \tr{modnameToC}, we really only have to worry about \tr{'}s
(quote chars) in the name.  Rare.

\begin{code}
modnameToC  :: FAST_STRING -> FAST_STRING
modnameToC fast_str = identFsToC fast_str
\end{code}
