
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[OccName]{@OccName@}

\begin{code}
module OccName (
	-- Modules
	Module,		-- Abstract, instance of Outputable
	mkSrcModule, mkSrcModuleFS, mkSysModuleFS, mkImportModuleFS, mkBootModule, mkIfaceModuleFS,
	moduleString, moduleUserString, moduleIfaceFlavour,
	pprModule, pprModuleSep, pprModuleBoot,

	-- IfaceFlavour
	IfaceFlavour,
	hiFile, hiBootFile, bootFlavour,

	-- The NameSpace type; abstact
	NameSpace, tcName, clsName, tcClsName, dataName, varName, tvName,
	nameSpaceString, 

	-- The OccName type
	OccName, 	-- Abstract, instance of Outputable
	pprOccName, 

	mkSrcOccFS, mkSysOcc, mkSysOccFS, mkSrcVarOcc, mkKindOccFS,
	mkSuperDictSelOcc, mkDFunOcc, mkForeignExportOcc,
	mkDictOcc, mkWorkerOcc, mkMethodOcc, mkDefaultMethodOcc,
 	mkDerivedTyConOcc, mkClassTyConOcc, mkClassDataConOcc, mkSpecOcc,
	
	isTvOcc, isDataOcc, isDataSymOcc, isSymOcc,

	occNameFS, occNameString, occNameUserString, occNameSpace, occNameFlavour, 
	setOccNameSpace,

	-- Tidying up
	TidyOccEnv, emptyTidyOccEnv, tidyOccName, initTidyOccEnv,

	-- Encoding
	EncodedString, EncodedFS, UserString, UserFS, encode, encodeFS, decode,

	-- The basic form of names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	isLowerISO, isUpperISO

    ) where

#include "HsVersions.h"

import Char	( isDigit, isAlpha, isUpper, isLower, ISALPHANUM, ord, chr, digitToInt, intToDigit )
import Util	( thenCmp )
import FiniteMap ( FiniteMap, emptyFM, lookupFM, addToFM, elemFM )
import Outputable
import GlaExts
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
type UserFS     = FAST_STRING	-- As the user typed it
type EncodedFS = FAST_STRING	-- Encoded form

type UserString = String	-- As the user typed it
type EncodedString = String	-- Encoded form


pprEncodedFS :: EncodedFS -> SDoc
pprEncodedFS fs
  = getPprStyle 	$ \ sty ->
    if userStyle sty then
	text (decode (_UNPK_ fs))
    else
	ptext fs
\end{code}


%************************************************************************
%*									*
\subsection{Interface file flavour}
%*									*
%************************************************************************

The IfaceFlavour type is used mainly in an imported Name's Provenance
to say whether the name comes from a regular .hi file, or whether it comes
from a hand-written .hi-boot file.  This is important, because it has to be 
propagated.  Suppose

	C.hs imports B
	B.hs imports A
	A.hs imports C {-# SOURCE -#} ( f )

Then in A.hi we may mention C.f, in an inlining.  When compiling B we *must not* 
read C.f's details from C.hi, even if the latter happens to exist from an earlier
compilation run.  So we use the name "C!f" in A.hi, and when looking for an interface
file with details of C!f we look in C.hi-boot.  The "!" stuff is recorded in the
IfaceFlavour in the Module of C.f in A. 

Not particularly beautiful, but it works.

\begin{code}
data IfaceFlavour = HiFile		-- The thing comes from a standard interface file
					-- or from the source file itself
		  | HiBootFile		-- ... or from a handwritten "hi-boot" interface file
		  deriving( Eq )

hiFile     = HiFile
hiBootFile = HiBootFile

instance Text IfaceFlavour where	-- Just used in debug prints of lex tokens
  showsPrec n HiFile     s = s
  showsPrec n HiBootFile s = "!" ++ s

bootFlavour :: IfaceFlavour -> Bool
bootFlavour HiBootFile = True
bootFlavour HiFile     = False
\end{code}


%************************************************************************
%*									*
\subsection[Module]{The name of a module}
%*									*
%************************************************************************

\begin{code}
data Module = Module
		EncodedFS
		IfaceFlavour
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them
\end{code}

\begin{code}
instance Outputable Module where
  ppr = pprModule

-- Ignore the IfaceFlavour when comparing modules
instance Eq Module where
  (Module m1 _) == (Module m2 _) = m1 == m2

instance Ord Module where
  (Module m1 _) `compare` (Module m2 _) = m1 `compare` m2
\end{code}


\begin{code}
pprModule :: Module -> SDoc
pprModule (Module mod _) = pprEncodedFS mod

pprModuleSep, pprModuleBoot :: Module -> SDoc
pprModuleSep (Module mod HiFile)     = dot
pprModuleSep (Module mod HiBootFile) = char '!'

pprModuleBoot (Module mod HiFile)     = empty
pprModuleBoot (Module mod HiBootFile) = char '!'
\end{code}


\begin{code}
mkSrcModule :: UserString -> Module
mkSrcModule s = Module (_PK_ (encode s)) HiFile

mkSrcModuleFS :: UserFS -> Module
mkSrcModuleFS s = Module (encodeFS s) HiFile

mkImportModuleFS :: UserFS -> IfaceFlavour -> Module
mkImportModuleFS s hif = Module (encodeFS s) hif

mkSysModuleFS :: EncodedFS -> IfaceFlavour -> Module
mkSysModuleFS s hif = Module s hif

mkIfaceModuleFS :: EncodedFS -> Module
mkIfaceModuleFS s = Module s HiFile

mkBootModule :: Module -> Module
mkBootModule (Module s _) = Module s HiBootFile

moduleString :: Module -> EncodedString
moduleString (Module mod _) = _UNPK_ mod

moduleUserString :: Module -> UserString
moduleUserString (Module mod _) = decode (_UNPK_ mod)

moduleIfaceFlavour :: Module -> IfaceFlavour
moduleIfaceFlavour (Module _ hif) = hif
\end{code}


%************************************************************************
%*									*
\subsection{Name space}
%*									*
%************************************************************************

\begin{code}
data NameSpace = VarName	-- Variables
	       | DataName	-- Data constructors
	       | TvName		-- Type variables
	       | TcClsName	-- Type constructors and classes; Haskell has them
				-- in the same name space for now.  
	       deriving( Eq, Ord )

-- Though type constructors and classes are in the same name space now,
-- the NameSpace type is abstract, so we can easily separate them later
tcName    = TcClsName		-- Type constructors
clsName   = TcClsName		-- Classes
tcClsName = TcClsName		-- Not sure which!

dataName = DataName
tvName   = TvName
varName  = VarName


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
mkSysOcc occ_sp str = ASSERT( alreadyEncoded str )
		      OccName occ_sp (_PK_ str)

mkSysOccFS :: NameSpace -> EncodedFS -> OccName
mkSysOccFS occ_sp fs = ASSERT2( alreadyEncodedFS fs, ppr fs )
		       OccName occ_sp fs

-- Kind constructors get a speical function.  Uniquely, they are not encoded,
-- so that they have names like '*'.  This means that *even in interface files*
-- we'll get kinds like (* -> (* -> *)).  We can't use mkSysOcc because it
-- has an ASSERT that doesn't hold.
mkKindOccFS :: NameSpace -> EncodedFS -> OccName
mkKindOccFS occ_sp fs = OccName occ_sp fs
\end{code}

*Source-code* things are encoded.

\begin{code}
mkSrcOccFS :: NameSpace -> UserFS -> OccName
mkSrcOccFS occ_sp fs = mkSysOccFS occ_sp (encodeFS fs)

mkSrcVarOcc :: UserFS -> OccName
mkSrcVarOcc fs = mkSysOccFS varName (encodeFS fs)
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
occNameString (OccName _ s) = _UNPK_ s

occNameUserString :: OccName -> UserString
occNameUserString occ = decode (occNameString occ)

occNameSpace :: OccName -> NameSpace
occNameSpace (OccName sp _) = sp

setOccNameSpace :: OccName -> NameSpace -> OccName
setOccNameSpace (OccName _ occ) sp = OccName sp occ

-- occNameFlavour is used only to generate good error messages
occNameFlavour :: OccName -> String
occNameFlavour (OccName sp _) = nameSpaceString sp
\end{code}

\begin{code}
isTvOcc, isDataSymOcc, isSymOcc :: OccName -> Bool

isTvOcc (OccName TvName _) = True
isTvOcc other              = False

-- Data constructor operator (starts with ':', or '[]')
-- Pretty inefficient!
isDataSymOcc (OccName DataName s) = isLexConSym (decodeFS s)
isDataSymOcc other		  = False

isDataOcc (OccName DataName _) = True
isDataOcc oter		       = False

-- Any operator (data constructor or variable)
-- Pretty inefficient!
isSymOcc (OccName DataName s) = isLexConSym (decodeFS s)
isSymOcc (OccName VarName s)  = isLexSym (decodeFS s)
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
	$T...		compiler-generated tycons for dictionaries
	$D...		...ditto data cons
	$sf..		specialised version of f

	in encoded form these appear as Zdfxxx etc

	:...		keywords (export:, letrec: etc.)

This knowledge is encoded in the following functions.


@mk_deriv@ generates an @OccName@ from the one-char prefix and a string.
NB: The string must already be encoded!

\begin{code}
mk_deriv :: NameSpace 
	 -> String		-- Distinguishes one sort of derived name from another
	 -> EncodedString	-- Must be already encoded!!  We don't want to encode it a 
				-- second time because encoding isn't itempotent
	 -> OccName

mk_deriv occ_sp sys_prefix str = mkSysOcc occ_sp (encode sys_prefix ++ str)
\end{code}

\begin{code}
mkDictOcc, mkWorkerOcc, mkDefaultMethodOcc,
 	   mkClassTyConOcc, mkClassDataConOcc, mkSpecOcc
   :: OccName -> OccName

-- These derived variables have a prefix that no Haskell value could have
mkWorkerOcc        = mk_simple_deriv varName  "$w"
mkDefaultMethodOcc = mk_simple_deriv varName  "$dm"
mkDerivedTyConOcc  = mk_simple_deriv tcName   ":"	-- The : prefix makes sure it classifies
mkClassTyConOcc    = mk_simple_deriv tcName   ":T"	-- as a tycon/datacon
mkClassDataConOcc  = mk_simple_deriv dataName ":D"	--
mkDictOcc	   = mk_simple_deriv varName  "$d"
mkSpecOcc	   = mk_simple_deriv varName  "$s"
mkForeignExportOcc = mk_simple_deriv varName  "$f"

mk_simple_deriv sp px occ = mk_deriv sp px (occNameString occ)
\end{code}

\begin{code}
mkSuperDictSelOcc :: Int 	-- Index of superclass, eg 3
		  -> OccName 	-- Class, eg "Ord"
		  -> OccName	-- eg "p3Ord"
mkSuperDictSelOcc index cls_occ
  = mk_deriv varName "$p" (show index ++ occNameString cls_occ)
\end{code}


\begin{code}
mkDFunOcc :: OccName 	-- class, eg "Ord"
	  -> OccName 	-- tycon (or something convenient from the instance type)
			--	eg "Maybe"
	  -> Int	-- Unique to distinguish dfuns which share the previous two
			--	eg 3
	  -> OccName	-- "dOrdMaybe3"

mkDFunOcc cls_occ tycon_occ index
  = mk_deriv VarName "$f" (show_index ++ cls_str ++ tycon_str)
  where
    cls_str   = occNameString cls_occ
    tycon_str = occNameString tycon_occ
    show_index | index == 0 = ""
   	       | otherwise  = show index
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
type TidyOccEnv = FiniteMap FAST_STRING Int	-- The in-scope OccNames
emptyTidyOccEnv = emptyFM

initTidyOccEnv :: [OccName] -> TidyOccEnv	-- Initialise with names to avoid!
initTidyOccEnv = foldl (\env (OccName _ fs) -> addToFM env fs 1) emptyTidyOccEnv

tidyOccName :: TidyOccEnv -> OccName -> (TidyOccEnv, OccName)

tidyOccName in_scope occ@(OccName occ_sp fs)
  | not (fs `elemFM` in_scope)
  = (addToFM in_scope fs 1, occ)	-- First occurrence

  | otherwise				-- Already occurs
  = go in_scope (_UNPK_ fs)
  where

    go in_scope str = case lookupFM in_scope pk_str of
			Just n  -> go (addToFM in_scope pk_str (n+1)) (str ++ show n)
				-- Need to go round again, just in case "t3" (say) 
				-- clashes with a "t3" that's already in scope

			Nothing -> (addToFM in_scope pk_str 1, mkSysOccFS occ_sp pk_str)
				-- str is now unique
		    where
		      pk_str = _PK_ str
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

* Alphabetic characters (upper and lower), digits, and '_'
	all translate to themselves; 
	except 'Z', which translates to 'ZZ'
	and    'z', which translates to 'zz'
  We need both so that we can preserve the variable/tycon distinction

* Most other printable characters translate to 'Zx' for some
	alphabetic character x

* The others translate as 'Zxdd' where 'dd' is exactly two hexadecimal
	digits for the ord of the character

	Before		After
	--------------------------
	Trak		Trak
	foo_wib		foo_wib
	>		Zg
	>1		Zg1
	foo#		fooZh
	foo##		fooZhZh
	foo##1		fooZhXh1
	fooZ		fooZZ	
	:+		ZcZp
	()		Z0T
	(,,,,)		Z4T


\begin{code}
-- alreadyEncoded is used in ASSERTs to check for encoded
-- strings.  It isn't fail-safe, of course, because, say 'zh' might
-- be encoded or not.
alreadyEncoded :: String -> Bool
alreadyEncoded s = all ok s
		 where
		   ok '_' = True
		   ok ch  = ISALPHANUM ch

alreadyEncodedFS :: FAST_STRING -> Bool
alreadyEncodedFS fs = alreadyEncoded (_UNPK_ fs)

encode :: UserString -> EncodedString
encode cs = case maybe_tuple cs of
		Just n  -> 'Z' : show n ++ "T"		-- Tuples go to Z2T etc
		Nothing -> go cs
	  where
		go []     = []
		go (c:cs) = encode_ch c ++ go cs

-- ToDo: Unboxed tuples too, perhaps?
maybe_tuple ('(' : cs) = check_tuple 0 cs
maybe_tuple other      = Nothing

check_tuple :: Int -> String -> Maybe Int
check_tuple n (',' : cs) = check_tuple (n+1) cs
check_tuple n ")"	 = Just n
check_tuple n other      = Nothing

encodeFS :: UserFS -> EncodedFS
encodeFS fast_str  | all unencodedChar str = fast_str
		   | otherwise	           = _PK_ (encode str)
		   where
		     str = _UNPK_ fast_str

unencodedChar :: Char -> Bool	-- True for chars that don't need encoding
unencodedChar '_' = True
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   = ISALPHANUM c

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
encode_ch '^'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = ['z', 'x', intToDigit hi, intToDigit lo]
	       where
		 (hi,lo) = ord c `quotRem` 16
\end{code}

Decode is used for user printing.

\begin{code}
decodeFS :: FAST_STRING -> FAST_STRING
decodeFS fs = _PK_ (decode (_UNPK_ fs))

decode :: EncodedString -> UserString
decode [] = []
decode ('Z' : rest) = decode_escape rest
decode ('z' : rest) = decode_escape rest
decode (c   : rest) = c : decode rest

decode_escape :: EncodedString -> UserString

decode_escape ('Z' : rest) = 'Z' : decode rest
decode_escape ('C' : rest) = ':' : decode rest
decode_escape ('L' : rest) = '(' : decode rest
decode_escape ('R' : rest) = ')' : decode rest
decode_escape ('M' : rest) = '[' : decode rest
decode_escape ('N' : rest) = ']' : decode rest

decode_escape ('z' : rest) = 'z' : decode rest
decode_escape ('a' : rest) = '&' : decode rest
decode_escape ('b' : rest) = '|' : decode rest
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
decode_escape ('u' : rest) = '^' : decode rest
decode_escape ('v' : rest) = '%' : decode rest
decode_escape ('x' : d1 : d2 : rest) = chr (digitToInt d1 * 16 + digitToInt d2)  : decode rest

-- Tuples are coded as Z23T
decode_escape (c : rest)
  | isDigit c = go (digitToInt c) rest
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go n ('T' : rest)		= '(' : replicate n ',' ++ ')' : decode rest
    go n other = pprPanic "decode_escape" (ppr n <+> text (c:rest))

decode_escape (c : rest) = pprTrace "decode_escape" (char c) (decode rest)
\end{code}


%************************************************************************
%*									*
n\subsection{Lexical categories}
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
