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

	-- The OccEnv type
	OccEnv, emptyOccEnv, unitOccEnv, extendOccEnv,
	lookupOccEnv, mkOccEnv, extendOccEnvList, elemOccEnv,
	occEnvElts, foldOccEnv, plusOccEnv, plusOccEnv_C, extendOccEnv_C,


	-- The OccSet type
	OccSet, emptyOccSet, unitOccSet, mkOccSet, extendOccSet, extendOccSetList,
	unionOccSets, unionManyOccSets, minusOccSet, elemOccSet, occSetElts, 
	foldOccSet, isEmptyOccSet, intersectOccSet, intersectsOccSet,

	mkOccName, mkOccFS, mkSysOcc, mkSysOccFS, mkFCallOcc, mkKindOccFS,
	mkVarOcc, mkVarOccEncoded, mkTyVarOcc,
	mkSuperDictSelOcc, mkDFunOcc, mkForeignExportOcc,
	mkDictOcc, mkIPOcc, mkWorkerOcc, mkMethodOcc, mkDefaultMethodOcc,
 	mkDerivedTyConOcc, mkClassTyConOcc, mkClassDataConOcc, mkSpecOcc,
	mkGenOcc1, mkGenOcc2, mkLocalOcc, mkDataTOcc, mkDataCOcc,
	mkDataConWrapperOcc, mkDataConWorkerOcc,
	
	isVarOcc, isTvOcc, isTcOcc, isDataOcc, isDataSymOcc, isSymOcc, isValOcc,
	parenSymOcc, reportIfUnused, isTcClsName, isVarName,

	occNameFS, occNameString, occNameUserString, occNameSpace, 
	occNameFlavour, briefOccNameFlavour,
	setOccNameSpace,

	mkTupleOcc, isTupleOcc_maybe,

	-- Tidying up
	TidyOccEnv, emptyTidyOccEnv, tidyOccName, initTidyOccEnv,

	-- Encoding
	EncodedString, EncodedFS, UserString, UserFS, encode, encodeFS, decode, pprEncodedFS,

	-- The basic form of names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	isLowerISO, isUpperISO,
	startsVarSym, startsVarId, startsConSym, startsConId
    ) where

#include "HsVersions.h"

import Char	( isDigit, isUpper, isLower, isAlphaNum, ord, chr, digitToInt )
import Util	( thenCmp )
import Unique	( Unique, mkUnique, Uniquable(..) )
import BasicTypes ( Boxity(..), Arity )
import StaticFlags ( opt_PprStyle_Debug )
import UniqFM
import UniqSet
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
    if userStyle sty || dumpStyle sty
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

isTcClsName :: NameSpace -> Bool
isTcClsName TcClsName = True
isTcClsName _	      = False

isVarName :: NameSpace -> Bool	-- Variables or type variables, but not constructors
isVarName TvName  = True
isVarName VarName = True
isVarName other   = False


nameSpaceString :: NameSpace -> String
nameSpaceString DataName  = "data constructor"
nameSpaceString VarName   = "variable"
nameSpaceString TvName    = "type variable"
nameSpaceString TcClsName = "type constructor or class"
\end{code}


%************************************************************************
%*									*
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
%*									*
%************************************************************************

\begin{code}
data OccName = OccName 
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !EncodedFS
    }
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
pprOccName (OccName sp occ) 
  = getPprStyle $ \ sty ->
    pprEncodedFS occ <> if debugStyle sty then
			   braces (text (briefNameSpaceFlavour sp))
			else empty
\end{code}


%************************************************************************
%*									*
\subsection{Construction}
%*									*
%*****p*******************************************************************

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

mkOccName :: NameSpace -> String -> OccName
mkOccName ns s = mkSysOcc ns (encode s)

mkVarOcc :: UserFS -> OccName
mkVarOcc fs = mkSysOccFS varName (encodeFS fs)

mkTyVarOcc :: UserFS -> OccName
mkTyVarOcc fs = mkSysOccFS tvName (encodeFS fs)

mkVarOccEncoded :: EncodedFS -> OccName
mkVarOccEncoded fs = mkSysOccFS varName fs
\end{code}



%************************************************************************
%*									*
		Environments
%*									*
%************************************************************************

OccEnvs are used mainly for the envts in ModIfaces.

They are efficient, because FastStrings have unique Int# keys.  We assume
this key is less than 2^24, so we can make a Unique using
	mkUnique ns key  :: Unique
where 'ns' is a Char reprsenting the name space.  This in turn makes it
easy to build an OccEnv.

\begin{code}
instance Uniquable OccName where
  getUnique (OccName ns fs)
      = mkUnique char (I# (uniqueOfFS fs))
      where	-- See notes above about this getUnique function
        char = case ns of
		VarName   -> 'i'
		DataName  -> 'd'
		TvName    -> 'v'
		TcClsName -> 't'

type OccEnv a = UniqFM a

emptyOccEnv :: OccEnv a
unitOccEnv  :: OccName -> a -> OccEnv a
extendOccEnv :: OccEnv a -> OccName -> a -> OccEnv a
extendOccEnvList :: OccEnv a -> [(OccName, a)] -> OccEnv a
lookupOccEnv :: OccEnv a -> OccName -> Maybe a
mkOccEnv     :: [(OccName,a)] -> OccEnv a
elemOccEnv   :: OccName -> OccEnv a -> Bool
foldOccEnv   :: (a -> b -> b) -> b -> OccEnv a -> b
occEnvElts   :: OccEnv a -> [a]
extendOccEnv_C :: (a->a->a) -> OccEnv a -> OccName -> a -> OccEnv a
plusOccEnv     :: OccEnv a -> OccEnv a -> OccEnv a
plusOccEnv_C   :: (a->a->a) -> OccEnv a -> OccEnv a -> OccEnv a

emptyOccEnv  	 = emptyUFM
unitOccEnv   	 = unitUFM
extendOccEnv 	 = addToUFM
extendOccEnvList = addListToUFM
lookupOccEnv 	 = lookupUFM
mkOccEnv         = listToUFM
elemOccEnv	 = elemUFM
foldOccEnv	 = foldUFM
occEnvElts 	 = eltsUFM
plusOccEnv	 = plusUFM
plusOccEnv_C	 = plusUFM_C
extendOccEnv_C   = addToUFM_C


type OccSet = UniqFM OccName

emptyOccSet	  :: OccSet
unitOccSet	  :: OccName -> OccSet
mkOccSet          :: [OccName] -> OccSet
extendOccSet      :: OccSet -> OccName -> OccSet
extendOccSetList  :: OccSet -> [OccName] -> OccSet
unionOccSets	  :: OccSet -> OccSet -> OccSet
unionManyOccSets  :: [OccSet] -> OccSet
minusOccSet 	  :: OccSet -> OccSet -> OccSet
elemOccSet	  :: OccName -> OccSet -> Bool
occSetElts	  :: OccSet -> [OccName]
foldOccSet	  :: (OccName -> b -> b) -> b -> OccSet -> b
isEmptyOccSet	  :: OccSet -> Bool
intersectOccSet   :: OccSet -> OccSet -> OccSet
intersectsOccSet  :: OccSet -> OccSet -> Bool

emptyOccSet	  = emptyUniqSet
unitOccSet	  = unitUniqSet
mkOccSet          = mkUniqSet
extendOccSet	  = addOneToUniqSet
extendOccSetList  = addListToUniqSet
unionOccSets      = unionUniqSets
unionManyOccSets  = unionManyUniqSets
minusOccSet	  = minusUniqSet
elemOccSet        = elementOfUniqSet
occSetElts        = uniqSetToList
foldOccSet	  = foldUniqSet
isEmptyOccSet     = isEmptyUniqSet
intersectOccSet   = intersectUniqSets
intersectsOccSet s1 s2 = not (isEmptyOccSet (s1 `intersectOccSet` s2))
\end{code}


%************************************************************************
%*									*
\subsection{Predicates and taking them apart}
%*									*
%************************************************************************

\begin{code} 
occNameString :: OccName -> EncodedString
occNameString (OccName _ s) = unpackFS s

occNameUserString :: OccName -> UserString
occNameUserString occ = decode (occNameString occ)

setOccNameSpace :: NameSpace -> OccName -> OccName
setOccNameSpace sp (OccName _ occ) = OccName sp occ

-- occNameFlavour is used only to generate good error messages
occNameFlavour :: OccName -> SDoc
occNameFlavour (OccName DataName _)  = ptext SLIT("data constructor")
occNameFlavour (OccName TvName _)    = ptext SLIT("type variable")
occNameFlavour (OccName TcClsName _) = ptext SLIT("type constructor or class")
occNameFlavour (OccName VarName s)   = empty

-- briefOccNameFlavour is used in debug-printing of names
briefOccNameFlavour :: OccName -> String
briefOccNameFlavour (OccName sp _) = briefNameSpaceFlavour sp

briefNameSpaceFlavour DataName  = "d"
briefNameSpaceFlavour VarName   = "v"
briefNameSpaceFlavour TvName    = "tv"
briefNameSpaceFlavour TcClsName = "tc"
\end{code}

\begin{code}
isVarOcc, isTvOcc, isDataSymOcc, isSymOcc, isTcOcc :: OccName -> Bool

isVarOcc (OccName VarName _) = True
isVarOcc other               = False

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
isSymOcc (OccName DataName s)  = isLexConSym (decodeFS s)
isSymOcc (OccName TcClsName s) = isLexConSym (decodeFS s)
isSymOcc (OccName VarName s)   = isLexSym (decodeFS s)
isSymOcc other		       = False

parenSymOcc :: OccName -> SDoc -> SDoc
-- Wrap parens around an operator
parenSymOcc occ doc | isSymOcc occ = parens doc
		    | otherwise    = doc
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
-- to VarName
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
				-- Only used in debug mode, for extra clarity
	  -> Bool		-- True <=> hs-boot instance dfun
	  -> Int		-- Unique index
	  -> OccName		-- "$f3OrdMaybe"

-- In hs-boot files we make dict funs like $fx7ClsTy, which get bound to the real
-- thing when we compile the mother module. Reason: we don't know exactly
-- what the  mother module will call it.

mkDFunOcc info_str is_boot index 
  = mk_deriv VarName prefix string
  where
    prefix | is_boot   = "$fx"
	   | otherwise = "$f"
    string | opt_PprStyle_Debug = show index ++ info_str
	   | otherwise		= show index
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
type TidyOccEnv = OccEnv Int	-- The in-scope OccNames
	-- Range gives a plausible starting point for new guesses

emptyTidyOccEnv = emptyOccEnv

initTidyOccEnv :: [OccName] -> TidyOccEnv	-- Initialise with names to avoid!
initTidyOccEnv = foldl (\env occ -> extendOccEnv env occ 1) emptyTidyOccEnv

tidyOccName :: TidyOccEnv -> OccName -> (TidyOccEnv, OccName)

tidyOccName in_scope occ@(OccName occ_sp fs)
  = case lookupOccEnv in_scope occ of
	Nothing -> 	-- Not already used: make it used
		   (extendOccEnv in_scope occ 1, occ)

	Just n  -> 	-- Already used: make a new guess, 
			-- change the guess base, and try again
		   tidyOccName  (extendOccEnv in_scope occ (n+1))
				(mkSysOcc occ_sp (unpackFS fs ++ show n))
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
decode ('Z' : d : rest) | isDigit d = decode_tuple   d rest
			| otherwise = decode_upper   d : decode rest
decode ('z' : d : rest) | isDigit d = decode_num_esc d rest
			| otherwise = decode_lower   d : decode rest
decode (c   : rest) = c : decode rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = pprTrace "decode_upper" (char ch) ch
	     	
decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = pprTrace "decode_lower" (char ch) ch

-- Characters not having a specific code are coded as z224U
decode_num_esc d rest
  = go (digitToInt d) rest
  where
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : decode rest
    go n other = pprPanic "decode_num_esc" (ppr n <+> text other)

decode_tuple :: Char -> EncodedString -> UserString
decode_tuple d rest
  = go (digitToInt d) rest
  where
	-- NB. recurse back to decode after decoding the tuple, because
	-- the tuple might be embedded in a longer name.
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ('T':rest)	= "()" ++ decode rest
    go n ('T':rest)	= '(' : replicate (n-1) ',' ++ ")" ++ decode rest
    go 1 ('H':rest)	= "(# #)" ++ decode rest
    go n ('H':rest)	= '(' : '#' : replicate (n-1) ',' ++ "#)" ++ decode rest
    go n other = pprPanic "decode_tuple" (ppr n <+> text other)
\end{code}


%************************************************************************
%*									*
		Stuff for dealing with tuples
%*									*
%************************************************************************

Tuples are encoded as
	Z3T or Z3H
for 3-tuples or unboxed 3-tuples respectively.  No other encoding starts 
	Z<digit>

* "(# #)" is the tycon for an unboxed 1-tuple (not 0-tuple)
  There are no unboxed 0-tuples.  

* "()" is the tycon for a boxed 0-tuple.
  There are no boxed 1-tuples.


\begin{code}
maybe_tuple :: UserString -> Maybe EncodedString

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
\end{code}

\begin{code}
mkTupleOcc :: NameSpace -> Boxity -> Arity -> OccName
mkTupleOcc ns bx ar
  = OccName ns (mkFastString ('Z' : (show ar ++ bx_char)))
  where
    bx_char = case bx of
		Boxed   -> "T"
		Unboxed -> "H"

isTupleOcc_maybe :: OccName -> Maybe (NameSpace, Boxity, Arity)
-- Tuples are special, because there are so many of them!
isTupleOcc_maybe (OccName ns fs)
  = case unpackFS fs of
	('Z':d:rest) | isDigit d -> Just (decode_tup (digitToInt d) rest)
	other			 -> Nothing
  where
    decode_tup n "H" 	  = (ns, Unboxed, n)
    decode_tup n "T" 	  = (ns, Boxed, n)
    decode_tup n (d:rest) = decode_tup (n*10 + digitToInt d) rest
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

%************************************************************************
%*									*
		Binary instance
    Here rather than BinIface because OccName is abstract
%*									*
%************************************************************************

\begin{code}
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
\end{code}
