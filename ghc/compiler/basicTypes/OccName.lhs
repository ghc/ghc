{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[OccName]{@OccName@}

\begin{code}
module OccName (
	-- The NameSpace type; abstact
	NameSpace, tcName, clsName, tcClsName, dataName, varName, 
	tvName, srcDataName, nameSpaceString, 

	-- The OccName type
	OccName, 	-- Abstract, instance of Outputable
	pprOccName, 

	mkOccFS, mkSysOcc, mkSysOccFS, mkFCallOcc, mkKindOccFS,
	mkVarOcc, mkVarOccEncoded,
	mkSuperDictSelOcc, mkDFunOcc, mkForeignExportOcc,
	mkDictOcc, mkIPOcc, mkWorkerOcc, mkMethodOcc, mkDefaultMethodOcc,
 	mkDerivedTyConOcc, mkClassTyConOcc, mkClassDataConOcc, mkSpecOcc,
	mkGenOcc1, mkGenOcc2, mkLocalOcc, mkDataTOcc, mkDataCOcc,
	mkDataConWrapperOcc, mkDataConWorkerOcc,
	
	isTvOcc, isTcOcc, isDataOcc, isDataSymOcc, isSymOcc, isValOcc,
	reportIfUnused,

	occNameFS, occNameString, occNameUserString, occNameSpace, 
	occNameFlavour, briefOccNameFlavour,
	setOccNameSpace,

	-- Tidying up
	TidyOccEnv, emptyTidyOccEnv, tidyOccName, initTidyOccEnv,

	-- Encoding
	EncodedString, EncodedFS, UserString, UserFS, encode, encodeFS, decode, pprEncodedFS,

	-- The basic form of names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	isLowerISO, isUpperISO

    ) where

#include "HsVersions.h"

import Char	( isDigit, isUpper, isLower, isAlphaNum, ord, chr, digitToInt )
import Util	( thenCmp )
import Unique	( Unique )
import FiniteMap ( FiniteMap, emptyFM, lookupFM, addToFM, elemFM )
import FastString
import Outputable
import Binary

import GLAEXTS
\end{code}

We hold both module names and identifier names in a 'Z-encoded' form
that makes them acceptable both as a C identifier and as a Haskell
(prefix) identifier. 

They can always be decoded again when printing error messages
or anything else for the user, but it does make sense for it
to be represented here in encoded form, so that when generating
code the encoding operation is not performed on each occurrence.

These type synonyms help documentation.

\begin{code}
type UserFS    = FastString	-- As the user typed it
type EncodedFS = FastString	-- Encoded form

type UserString = String	-- As the user typed it
type EncodedString = String	-- Encoded form


pprEncodedFS :: EncodedFS -> SDoc
pprEncodedFS fs
  = getPprStyle 	$ \ sty ->
    if userStyle sty
	-- ftext (decodeFS fs) would needlessly pack the string again
	then text (decode (unpackFS fs))
        else ftext fs
\end{code}

%************************************************************************
%*									*
\subsection{Name space}
%*									*
%************************************************************************

\begin{code}
data NameSpace = VarName	-- Variables, including "source" data constructors
	       | DataName	-- "Real" data constructors 
	       | TvName		-- Type variables
	       | TcClsName	-- Type constructors and classes; Haskell has them
				-- in the same name space for now.
	       deriving( Eq, Ord )
   {-! derive: Binary !-}

-- Note [Data Constructors]  
-- see also: Note [Data Constructor Naming] in DataCon.lhs
-- 
--	"Source" data constructors are the data constructors mentioned
--	in Haskell source code
--
--	"Real" data constructors are the data constructors of the
--	representation type, which may not be the same as the source
--	type

-- Example:
--	data T = T !(Int,Int)
--
-- The source datacon has type (Int,Int) -> T
-- The real   datacon has type Int -> Int -> T
-- GHC chooses a representation based on the strictness etc.


-- Though type constructors and classes are in the same name space now,
-- the NameSpace type is abstract, so we can easily separate them later
tcName    = TcClsName		-- Type constructors
clsName   = TcClsName		-- Classes
tcClsName = TcClsName		-- Not sure which!

dataName    = DataName
srcDataName = DataName	-- Haskell-source data constructors should be
			-- in the Data name space

tvName      = TvName
varName     = VarName

nameSpaceString :: NameSpace -> String
nameSpaceString DataName  = "Data constructor"
nameSpaceString VarName   = "Variable"
nameSpaceString TvName    = "Type variable"
nameSpaceString TcClsName = "Type constructor or class"
\end{code}


%************************************************************************
%*									*
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
%*									*
%************************************************************************

\begin{code}
data OccName = OccName 
			NameSpace
			EncodedFS
   {-! derive : Binary !-}
\end{code}


\begin{code}
instance Eq OccName where
    (OccName sp1 s1) == (OccName sp2 s2) = s1 == s2 && sp1 == sp2

instance Ord OccName where
    compare (OccName sp1 s1) (OccName sp2 s2) = (s1  `compare` s2) `thenCmp`
						(sp1 `compare` sp2)
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
pprOccName (OccName sp occ) = pprEncodedFS occ
\end{code}


%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************

*Sys* things do no encoding; the caller should ensure that the thing is
already encoded

\begin{code}
mkSysOcc :: NameSpace -> EncodedString -> OccName
mkSysOcc occ_sp str = ASSERT2( alreadyEncoded str, text str )
		      OccName occ_sp (mkFastString str)

mkSysOccFS :: NameSpace -> EncodedFS -> OccName
mkSysOccFS occ_sp fs = ASSERT2( alreadyEncodedFS fs, ppr fs )
		       OccName occ_sp fs

mkFCallOcc :: EncodedString -> OccName
-- This version of mkSysOcc doesn't check that the string is already encoded,
-- because it will be something like "{__ccall f dyn Int# -> Int#}" 
-- This encodes a lot into something that then parses like an Id.
-- But then alreadyEncoded complains about the braces!
mkFCallOcc str = OccName varName (mkFastString str)

-- Kind constructors get a special function.  Uniquely, they are not encoded,
-- so that they have names like '*'.  This means that *even in interface files*
-- we'll get kinds like (* -> (* -> *)).  We can't use mkSysOcc because it
-- has an ASSERT that doesn't hold.
mkKindOccFS :: NameSpace -> EncodedFS -> OccName
mkKindOccFS occ_sp fs = OccName occ_sp fs
\end{code}

*Source-code* things are encoded.

\begin{code}
mkOccFS :: NameSpace -> UserFS -> OccName
mkOccFS occ_sp fs = mkSysOccFS occ_sp (encodeFS fs)

mkVarOcc :: UserFS -> OccName
mkVarOcc fs = mkSysOccFS varName (encodeFS fs)

mkVarOccEncoded :: EncodedFS -> OccName
mkVarOccEncoded fs = mkSysOccFS varName fs
\end{code}



%************************************************************************
%*									*
\subsection{Predicates and taking them apart}
%*									*
%************************************************************************

\begin{code} 
occNameFS :: OccName -> EncodedFS
occNameFS (OccName _ s) = s

occNameString :: OccName -> EncodedString
occNameString (OccName _ s) = unpackFS s

occNameUserString :: OccName -> UserString
occNameUserString occ = decode (occNameString occ)

occNameSpace :: OccName -> NameSpace
occNameSpace (OccName sp _) = sp

setOccNameSpace :: NameSpace -> OccName -> OccName
setOccNameSpace sp (OccName _ occ) = OccName sp occ

-- occNameFlavour is used only to generate good error messages
occNameFlavour :: OccName -> String
occNameFlavour (OccName DataName _)  = "Data constructor"
occNameFlavour (OccName TvName _)    = "Type variable"
occNameFlavour (OccName TcClsName _) = "Type constructor or class"
occNameFlavour (OccName VarName s)   = "Variable"

-- briefOccNameFlavour is used in debug-printing of names
briefOccNameFlavour :: OccName -> String
briefOccNameFlavour (OccName DataName _)    = "d"
briefOccNameFlavour (OccName VarName _)     = "v"
briefOccNameFlavour (OccName TvName _)      = "tv"
briefOccNameFlavour (OccName TcClsName _)   = "tc"
\end{code}

\begin{code}
isTvOcc, isDataSymOcc, isSymOcc, isTcOcc :: OccName -> Bool

isTvOcc (OccName TvName _) = True
isTvOcc other              = False

isTcOcc (OccName TcClsName _) = True
isTcOcc other                 = False

isValOcc (OccName VarName  _) = True
isValOcc (OccName DataName _) = True
isValOcc other		      = False

-- Data constructor operator (starts with ':', or '[]')
-- Pretty inefficient!
isDataSymOcc (OccName DataName s) = isLexConSym (decodeFS s)
isDataSymOcc (OccName VarName s)  = isLexConSym (decodeFS s)
isDataSymOcc other		  = False

isDataOcc (OccName DataName _) = True
isDataOcc (OccName VarName s)  = isLexCon (decodeFS s)
isDataOcc other		       = False

-- Any operator (data constructor or variable)
-- Pretty inefficient!
isSymOcc (OccName DataName s) = isLexConSym (decodeFS s)
isSymOcc (OccName VarName s)  = isLexSym (decodeFS s)
\end{code}


\begin{code}
reportIfUnused :: OccName -> Bool
  -- Haskell 98 encourages compilers to suppress warnings about
  -- unused names in a pattern if they start with "_".
reportIfUnused occ = case occNameUserString occ of
			('_' : _) -> False
			zz_other  -> True
\end{code}



%************************************************************************
%*									*
\subsection{Making system names}
%*									*
%************************************************************************

Here's our convention for splitting up the interface file name space:

	d...		dictionary identifiers
			(local variables, so no name-clash worries)

	$f...		dict-fun identifiers (from inst decls)
	$dm...		default methods
	$p...		superclass selectors
	$w...		workers
	:T...		compiler-generated tycons for dictionaries
	:D...		...ditto data cons
	$sf..		specialised version of f

	in encoded form these appear as Zdfxxx etc

	:...		keywords (export:, letrec: etc.)
--- I THINK THIS IS WRONG!

This knowledge is encoded in the following functions.


@mk_deriv@ generates an @OccName@ from the prefix and a string.
NB: The string must already be encoded!

\begin{code}
mk_deriv :: NameSpace 
	 -> String		-- Distinguishes one sort of derived name from another
	 -> EncodedString	-- Must be already encoded!!  We don't want to encode it a 
				-- second time because encoding isn't idempotent
	 -> OccName

mk_deriv occ_sp sys_prefix str = mkSysOcc occ_sp (encode sys_prefix ++ str)
\end{code}

\begin{code}
mkDictOcc, mkIPOcc, mkWorkerOcc, mkDefaultMethodOcc,
 	   mkClassTyConOcc, mkClassDataConOcc, mkSpecOcc
   :: OccName -> OccName

-- These derived variables have a prefix that no Haskell value could have
mkDataConWrapperOcc = mk_simple_deriv varName  "$W"
mkWorkerOcc         = mk_simple_deriv varName  "$w"
mkDefaultMethodOcc  = mk_simple_deriv varName  "$dm"
mkDerivedTyConOcc   = mk_simple_deriv tcName   ":"	-- The : prefix makes sure it classifies
mkClassTyConOcc     = mk_simple_deriv tcName   ":T"	-- as a tycon/datacon
mkClassDataConOcc   = mk_simple_deriv dataName ":D"	-- We go straight to the "real" data con
							-- for datacons from classes
mkDictOcc	    = mk_simple_deriv varName  "$d"
mkIPOcc		    = mk_simple_deriv varName  "$i"
mkSpecOcc	    = mk_simple_deriv varName  "$s"
mkForeignExportOcc  = mk_simple_deriv varName  "$f"

-- Generic derivable classes
mkGenOcc1           = mk_simple_deriv varName  "$gfrom"
mkGenOcc2           = mk_simple_deriv varName  "$gto" 

-- data T = MkT ... deriving( Data ) needs defintions for 
--	$tT   :: Data.Generics.Basics.DataType
--	$cMkT :: Data.Generics.Basics.Constr
mkDataTOcc = mk_simple_deriv varName  "$t"
mkDataCOcc = mk_simple_deriv varName  "$c"

mk_simple_deriv sp px occ = mk_deriv sp px (occNameString occ)


-- Data constructor workers are made by setting the name space
-- of the data constructor OccName (which should be a DataName)
-- to DataName
mkDataConWorkerOcc datacon_occ = setOccNameSpace varName datacon_occ 
\end{code}

\begin{code}
mkSuperDictSelOcc :: Int 	-- Index of superclass, eg 3
		  -> OccName 	-- Class, eg "Ord"
		  -> OccName	-- eg "$p3Ord"
mkSuperDictSelOcc index cls_occ
  = mk_deriv varName "$p" (show index ++ occNameString cls_occ)

mkLocalOcc :: Unique 		-- Unique
	   -> OccName		-- Local name (e.g. "sat")
	   -> OccName		-- Nice unique version ("$L23sat")
mkLocalOcc uniq occ
   = mk_deriv varName ("$L" ++ show uniq) (occNameString occ)
	-- The Unique might print with characters 
	-- that need encoding (e.g. 'z'!)
\end{code}


\begin{code}
mkDFunOcc :: EncodedString	-- Typically the class and type glommed together e.g. "OrdMaybe"
	  -> OccName		-- "$fOrdMaybe"

mkDFunOcc string = mk_deriv VarName "$f" string
\end{code}

We used to add a '$m' to indicate a method, but that gives rise to bad
error messages from the type checker when we print the function name or pattern
of an instance-decl binding.  Why? Because the binding is zapped
to use the method name in place of the selector name.
(See TcClassDcl.tcMethodBind)

The way it is now, -ddump-xx output may look confusing, but
you can always say -dppr-debug to get the uniques.

However, we *do* have to zap the first character to be lower case,
because overloaded constructors (blarg) generate methods too.
And convert to VarName space

e.g. a call to constructor MkFoo where
	data (Ord a) => Foo a = MkFoo a

If this is necessary, we do it by prefixing '$m'.  These 
guys never show up in error messages.  What a hack.

\begin{code}
mkMethodOcc :: OccName -> OccName
mkMethodOcc occ@(OccName VarName fs) = occ
mkMethodOcc occ			     = mk_simple_deriv varName "$m" occ
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
type TidyOccEnv = FiniteMap FastString Int	-- The in-scope OccNames
emptyTidyOccEnv = emptyFM

initTidyOccEnv :: [OccName] -> TidyOccEnv	-- Initialise with names to avoid!
initTidyOccEnv = foldl (\env (OccName _ fs) -> addToFM env fs 1) emptyTidyOccEnv

tidyOccName :: TidyOccEnv -> OccName -> (TidyOccEnv, OccName)

tidyOccName in_scope occ@(OccName occ_sp fs)
  | not (fs `elemFM` in_scope)
  = (addToFM in_scope fs 1, occ)	-- First occurrence

  | otherwise				-- Already occurs
  = go in_scope (unpackFS fs)
  where

    go in_scope str = case lookupFM in_scope pk_str of
			Just n  -> go (addToFM in_scope pk_str (n+1)) (str ++ show n)
				-- Need to go round again, just in case "t3" (say) 
				-- clashes with a "t3" that's already in scope

			Nothing -> (addToFM in_scope pk_str 1, mkSysOccFS occ_sp pk_str)
				-- str is now unique
		    where
		      pk_str = mkFastString str
\end{code}


%************************************************************************
%*									*
\subsection{The 'Z' encoding}
%*									*
%************************************************************************

This is the main name-encoding and decoding function.  It encodes any
string into a string that is acceptable as a C name.  This is the name
by which things are known right through the compiler.

The basic encoding scheme is this.  

* Tuples (,,,) are coded as Z3T

* Alphabetic characters (upper and lower) and digits
	all translate to themselves; 
	except 'Z', which translates to 'ZZ'
	and    'z', which translates to 'zz'
  We need both so that we can preserve the variable/tycon distinction

* Most other printable characters translate to 'zx' or 'Zx' for some
	alphabetic character x

* The others translate as 'znnnU' where 'nnn' is the decimal number
        of the character

	Before		After
	--------------------------
	Trak		Trak
	foo_wib		foozuwib
	>		zg
	>1		zg1
	foo#		foozh
	foo##		foozhzh
	foo##1		foozhzh1
	fooZ		fooZZ	
	:+		ZCzp
	()		Z0T	0-tuple
	(,,,,)		Z5T	5-tuple  
	(# #)           Z1H     unboxed 1-tuple	(note the space)
	(#,,,,#)	Z5H	unboxed 5-tuple
		(NB: There is no Z1T nor Z0H.)

\begin{code}
-- alreadyEncoded is used in ASSERTs to check for encoded
-- strings.  It isn't fail-safe, of course, because, say 'zh' might
-- be encoded or not.
alreadyEncoded :: String -> Bool
alreadyEncoded s = all ok s
		 where
		   ok ' ' = True
			-- This is a bit of a lie; if we really wanted spaces
			-- in names we'd have to encode them.  But we do put
			-- spaces in ccall "occurrences", and we don't want to
			-- reject them here
		   ok ch  = isAlphaNum ch

alreadyEncodedFS :: FastString -> Bool
alreadyEncodedFS fs = alreadyEncoded (unpackFS fs)

encode :: UserString -> EncodedString
encode cs = case maybe_tuple cs of
		Just n  -> n		-- Tuples go to Z2T etc
		Nothing -> go cs
	  where
		go []     = []
		go (c:cs) = encode_ch c ++ go cs

maybe_tuple "(# #)" = Just("Z1H")
maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
				 (n, '#' : ')' : cs) -> Just ('Z' : shows (n+1) "H")
				 other		     -> Nothing
maybe_tuple "()" = Just("Z0T")
maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
				 (n, ')' : cs) -> Just ('Z' : shows (n+1) "T")
				 other	       -> Nothing
maybe_tuple other    	     = Nothing

count_commas :: Int -> String -> (Int, String)
count_commas n (',' : cs) = count_commas (n+1) cs
count_commas n cs	  = (n,cs)

encodeFS :: UserFS -> EncodedFS
encodeFS fast_str  | all unencodedChar str = fast_str
		   | otherwise	           = mkFastString (encode str)
		   where
		     str = unpackFS fast_str

unencodedChar :: Char -> Bool	-- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
	          || c >= 'A' && c <= 'Z'
		  || c >= '0' && c <= '9'

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]	-- Common case first

-- Constructors
encode_ch '('  = "ZL"	-- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"	-- For symmetry with (
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"

-- Variables
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = 'z' : shows (ord c) "U"
\end{code}

Decode is used for user printing.

\begin{code}
decodeFS :: FastString -> FastString
decodeFS fs = mkFastString (decode (unpackFS fs))

decode :: EncodedString -> UserString
decode [] = []
decode ('Z' : rest) = decode_escape rest
decode ('z' : rest) = decode_escape rest
decode (c   : rest) = c : decode rest

decode_escape :: EncodedString -> UserString

decode_escape ('L' : rest) = '(' : decode rest
decode_escape ('R' : rest) = ')' : decode rest
decode_escape ('M' : rest) = '[' : decode rest
decode_escape ('N' : rest) = ']' : decode rest
decode_escape ('C' : rest) = ':' : decode rest
decode_escape ('Z' : rest) = 'Z' : decode rest

decode_escape ('z' : rest) = 'z' : decode rest
decode_escape ('a' : rest) = '&' : decode rest
decode_escape ('b' : rest) = '|' : decode rest
decode_escape ('c' : rest) = '^' : decode rest
decode_escape ('d' : rest) = '$' : decode rest
decode_escape ('e' : rest) = '=' : decode rest
decode_escape ('g' : rest) = '>' : decode rest
decode_escape ('h' : rest) = '#' : decode rest
decode_escape ('i' : rest) = '.' : decode rest
decode_escape ('l' : rest) = '<' : decode rest
decode_escape ('m' : rest) = '-' : decode rest
decode_escape ('n' : rest) = '!' : decode rest
decode_escape ('p' : rest) = '+' : decode rest
decode_escape ('q' : rest) = '\'' : decode rest
decode_escape ('r' : rest) = '\\' : decode rest
decode_escape ('s' : rest) = '/' : decode rest
decode_escape ('t' : rest) = '*' : decode rest
decode_escape ('u' : rest) = '_' : decode rest
decode_escape ('v' : rest) = '%' : decode rest

-- Tuples are coded as Z23T
-- Characters not having a specific code are coded as z224U
decode_escape (c : rest)
  | isDigit c = go (digitToInt c) rest
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ('T' : rest) 		= "()" ++ (decode rest)
    go n ('T' : rest)		= '(' : replicate (n-1) ',' ++ ')' : decode rest
    go 1 ('H' : rest)		= "(# #)" ++ (decode rest)
    go n ('H' : rest)		= '(' : '#' : replicate (n-1) ',' ++ '#' : ')' : decode rest
    go n ('U' : rest)           = chr n : decode rest
    go n other = pprPanic "decode_escape" (ppr n <+> text (c:rest))

decode_escape (c : rest) = pprTrace "decode_escape" (char c) (decode rest)
decode_escape []	 = pprTrace "decode_escape" (text "empty") ""
\end{code}


%************************************************************************
%*									*
\subsection{Lexical categories}
%*									*
%************************************************************************

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.

\begin{code}
isLexCon,   isLexVar,    isLexId,    isLexSym    :: FastString -> Bool
isLexConId, isLexConSym, isLexVarId, isLexVarSym :: FastString -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------

isLexConId cs				-- Prefix type or data constructors
  | nullFastString cs = False		-- 	e.g. "Foo", "[]", "(,)" 
  | cs == FSLIT("[]") = True
  | otherwise	      = startsConId (headFS cs)

isLexVarId cs				-- Ordinary prefix identifiers
  | nullFastString cs = False		-- 	e.g. "x", "_x"
  | otherwise         = startsVarId (headFS cs)

isLexConSym cs				-- Infix type or data constructors
  | nullFastString cs = False		--	e.g. ":-:", ":", "->"
  | cs == FSLIT("->") = True
  | otherwise	      = startsConSym (headFS cs)

isLexVarSym cs				-- Infix identifiers
  | nullFastString cs = False		-- 	e.g. "+"
  | otherwise         = startsVarSym (headFS cs)

-------------
startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = isSymbolASCII c || isSymbolISO c	-- Infix Ids
startsConSym c = c == ':'				-- Infix data constructors
startsVarId c  = isLower c || isLowerISO c || c == '_'	-- Ordinary Ids
startsConId c  = isUpper c || isUpperISO c || c == '('	-- Ordinary type constructors and data constructors


isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
isSymbolISO   c = ord c `elem` (0xd7 : 0xf7 : [0xa1 .. 0xbf])
isUpperISO    (C# c#) = c# `geChar#` '\xc0'# && c# `leChar#` '\xde'# && c# `neChar#` '\xd7'#
	--0xc0 <= oc && oc <= 0xde && oc /= 0xd7 where oc = ord c
isLowerISO    (C# c#) = c# `geChar#` '\xdf'# && c# `leChar#` '\xff'# && c# `neChar#` '\xf7'#
	--0xdf <= oc && oc <= 0xff && oc /= 0xf7 where oc = ord c
\end{code}
\begin{code}
{-* Generated by DrIFT-v1.0 : Look, but Don't Touch. *-}
instance Binary NameSpace where
    put_ bh VarName = do
	    putByte bh 0
    put_ bh DataName = do
	    putByte bh 1
    put_ bh TvName = do
	    putByte bh 2
    put_ bh TcClsName = do
	    putByte bh 3
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return VarName
	      1 -> do return DataName
	      2 -> do return TvName
	      _ -> do return TcClsName

instance Binary OccName where
    put_ bh (OccName aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (OccName aa ab)

--  Imported from other files :-

\end{code}
