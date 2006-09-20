{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[OccName]{@OccName@}

\begin{code}
module OccName (
	-- * The NameSpace type; abstact
	NameSpace, tcName, clsName, tcClsName, dataName, varName, 
	tvName, srcDataName,

	-- ** Printing
	pprNameSpace, pprNonVarNameSpace, pprNameSpaceBrief,

	-- * The OccName type
	OccName, 	-- Abstract, instance of Outputable
	pprOccName, 

	-- ** Construction	
	mkOccName, mkOccNameFS, 
	mkVarOcc, mkVarOccFS,
	mkTyVarOcc,
	mkDFunOcc,
	mkTupleOcc, 
	setOccNameSpace,

	-- ** Derived OccNames
	mkDataConWrapperOcc, mkWorkerOcc, mkDefaultMethodOcc,
	mkDerivedTyConOcc, mkNewTyCoOcc,
  	mkClassTyConOcc, mkClassDataConOcc, mkDictOcc, mkIPOcc, 
 	mkSpecOcc, mkForeignExportOcc, mkGenOcc1, mkGenOcc2,
	mkDataTOcc, mkDataCOcc, mkDataConWorkerOcc,
	mkSuperDictSelOcc, mkLocalOcc, mkMethodOcc, mkInstTyTcOcc,
	mkInstTyCoOcc, 

	-- ** Deconstruction
	occNameFS, occNameString, occNameSpace, 

	isVarOcc, isTvOcc, isTcOcc, isDataOcc, isDataSymOcc, isSymOcc, isValOcc,
	parenSymOcc, reportIfUnused, isTcClsName, isVarName,

	isTupleOcc_maybe,

	-- The OccEnv type
	OccEnv, emptyOccEnv, unitOccEnv, extendOccEnv, mapOccEnv,
	lookupOccEnv, mkOccEnv, extendOccEnvList, elemOccEnv,
	occEnvElts, foldOccEnv, plusOccEnv, plusOccEnv_C, extendOccEnv_C,

	-- The OccSet type
	OccSet, emptyOccSet, unitOccSet, mkOccSet, extendOccSet, 
	extendOccSetList,
	unionOccSets, unionManyOccSets, minusOccSet, elemOccSet, occSetElts, 
	foldOccSet, isEmptyOccSet, intersectOccSet, intersectsOccSet,

	-- Tidying up
	TidyOccEnv, emptyTidyOccEnv, tidyOccName, initTidyOccEnv,

	-- The basic form of names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	startsVarSym, startsVarId, startsConSym, startsConId
    ) where

#include "HsVersions.h"

import Util		( thenCmp )
import Unique		( Unique, mkUnique, Uniquable(..) )
import BasicTypes 	( Boxity(..), Arity )
import StaticFlags 	( opt_PprStyle_Debug )
import UniqFM
import UniqSet
import FastString
import Outputable
import Binary

import GLAEXTS

import Data.Char	( isUpper, isLower, ord )

-- Unicode TODO: put isSymbol in libcompat
#if __GLASGOW_HASKELL__ > 604
import Data.Char	( isSymbol )
#else
isSymbol = const False
#endif

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

pprNameSpace :: NameSpace -> SDoc
pprNameSpace DataName  = ptext SLIT("data constructor")
pprNameSpace VarName   = ptext SLIT("variable")
pprNameSpace TvName    = ptext SLIT("type variable")
pprNameSpace TcClsName = ptext SLIT("type constructor or class")

pprNonVarNameSpace :: NameSpace -> SDoc
pprNonVarNameSpace VarName = empty
pprNonVarNameSpace ns = pprNameSpace ns

pprNameSpaceBrief DataName  = char 'd'
pprNameSpaceBrief VarName   = char 'v'
pprNameSpaceBrief TvName    = ptext SLIT("tv")
pprNameSpaceBrief TcClsName = ptext SLIT("tc")
\end{code}


%************************************************************************
%*									*
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
%*									*
%************************************************************************

\begin{code}
data OccName = OccName 
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !FastString
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
    if codeStyle sty 
	then ftext (zEncodeFS occ)
	else ftext occ <> if debugStyle sty 
			    then braces (pprNameSpaceBrief sp)
			    else empty
\end{code}


%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************

\begin{code}
mkOccName :: NameSpace -> String -> OccName
mkOccName occ_sp str = OccName occ_sp (mkFastString str)

mkOccNameFS :: NameSpace -> FastString -> OccName
mkOccNameFS occ_sp fs = OccName occ_sp fs

mkVarOcc :: String -> OccName
mkVarOcc s = mkOccName varName s

mkVarOccFS :: FastString -> OccName
mkVarOccFS fs = mkOccNameFS varName fs

mkTyVarOcc :: FastString -> OccName
mkTyVarOcc fs = mkOccNameFS tvName fs
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
mapOccEnv      :: (a->b) -> OccEnv a -> OccEnv b

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
mapOccEnv	 = mapUFM

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
occNameString :: OccName -> String
occNameString (OccName _ s) = unpackFS s

setOccNameSpace :: NameSpace -> OccName -> OccName
setOccNameSpace sp (OccName _ occ) = OccName sp occ

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
isDataSymOcc (OccName DataName s) = isLexConSym s
isDataSymOcc (OccName VarName s)  
  | isLexConSym s = pprPanic "isDataSymOcc: check me" (ppr s)
		-- Jan06: I don't think this should happen
isDataSymOcc other		  = False

isDataOcc (OccName DataName _) = True
isDataOcc (OccName VarName s)  
  | isLexCon s = pprPanic "isDataOcc: check me" (ppr s)
		-- Jan06: I don't think this should happen
isDataOcc other		       = False

-- Any operator (data constructor or variable)
-- Pretty inefficient!
isSymOcc (OccName DataName s)  = isLexConSym s
isSymOcc (OccName TcClsName s) = isLexConSym s
isSymOcc (OccName VarName s)   = isLexSym s
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
reportIfUnused occ = case occNameString occ of
			('_' : _) -> False
			_other    -> True
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
	 -> String
	 -> OccName

mk_deriv occ_sp sys_prefix str = mkOccName occ_sp (sys_prefix ++ str)
\end{code}

\begin{code}
mkDataConWrapperOcc, mkWorkerOcc, mkDefaultMethodOcc, mkDerivedTyConOcc,
  	mkClassTyConOcc, mkClassDataConOcc, mkDictOcc, mkIPOcc, 
 	mkSpecOcc, mkForeignExportOcc, mkGenOcc1, mkGenOcc2,
	mkDataTOcc, mkDataCOcc, mkDataConWorkerOcc, mkNewTyCoOcc
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
mkNewTyCoOcc        = mk_simple_deriv tcName  "Co"

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

-- Derive a name for the representation type constructor of a data/newtype
-- instance.
--
mkInstTyTcOcc :: Int			-- Index
	      -> OccName		-- Local name (e.g. "Map")
	      -> OccName		-- Nice unique version (":R23Map")
mkInstTyTcOcc index occ
   = mk_deriv varName (":R" ++ show index) (occNameString occ)

-- Derive a name for the coercion of a data/newtype instance.
--
mkInstTyCoOcc :: Int			-- Index
	      -> OccName		-- Local name (e.g. "Map")
	      -> OccName		-- Nice unique version (":Co23Map")
mkInstTyCoOcc index occ
   = mk_deriv varName (":Co" ++ show index) (occNameString occ)
\end{code}

\begin{code}
mkDFunOcc :: String		-- Typically the class and type glommed together e.g. "OrdMaybe"
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
				(mkOccName occ_sp (unpackFS fs ++ show n))
\end{code}

%************************************************************************
%*									*
		Stuff for dealing with tuples
%*									*
%************************************************************************

\begin{code}
mkTupleOcc :: NameSpace -> Boxity -> Arity -> OccName
mkTupleOcc ns bx ar = OccName ns (mkFastString str)
  where
 	-- no need to cache these, the caching is done in the caller
	-- (TysWiredIn.mk_tuple)
    str = case bx of
		Boxed   -> '(' : commas ++ ")"
		Unboxed -> '(' : '#' : commas ++ "#)"

    commas = take (ar-1) (repeat ',')

isTupleOcc_maybe :: OccName -> Maybe (NameSpace, Boxity, Arity)
-- Tuples are special, because there are so many of them!
isTupleOcc_maybe (OccName ns fs)
  = case unpackFS fs of
	'(':'#':',':rest -> Just (ns, Unboxed, 2 + count_commas rest)
	'(':',':rest     -> Just (ns, Boxed,   2 + count_commas rest)
	_other           -> Nothing
  where
    count_commas (',':rest) = 1 + count_commas rest
    count_commas _          = 0
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
  | nullFS cs	      = False		-- 	e.g. "Foo", "[]", "(,)" 
  | cs == FSLIT("[]") = True
  | otherwise	      = startsConId (headFS cs)

isLexVarId cs				-- Ordinary prefix identifiers
  | nullFS cs	      = False		-- 	e.g. "x", "_x"
  | otherwise         = startsVarId (headFS cs)

isLexConSym cs				-- Infix type or data constructors
  | nullFS cs	      = False		--	e.g. ":-:", ":", "->"
  | cs == FSLIT("->") = True
  | otherwise	      = startsConSym (headFS cs)

isLexVarSym cs				-- Infix identifiers
  | nullFS cs	      = False		-- 	e.g. "+"
  | otherwise         = startsVarSym (headFS cs)

-------------
startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = isSymbolASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids
startsConSym c = c == ':'				-- Infix data constructors
startsVarId c  = isLower c || c == '_'	-- Ordinary Ids
startsConId c  = isUpper c || c == '('	-- Ordinary type constructors and data constructors

isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
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
