%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelNames]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod 

[oh dear, looks like the recursive module monster caught up with
 and gobbled whoever was writing the above :-) -- SOF ]

\begin{code}
module PrelNames (
	Unique, Uniquable(..), hasKey, 	-- Re-exported for convenience

	-----------------------------------------------------------
	module PrelNames,	-- A huge bunch of (a) RdrNames, e.g. intTyCon_RDR
				--		   (b) Uniques   e.g. intTyConKey
				-- So many that we export them all

	-----------------------------------------------------------
	knownKeyNames, 
        mkTupNameStr, mkTupConRdrName,

	------------------------------------------------------------
	-- Goups of classes and types
	needsDataDeclCtxtClassKeys, cCallishClassKeys, noDictClassKeys,
	fractionalClassKeys, numericClassKeys, standardClassKeys,
	derivingOccurrences, 	-- For a given class C, this tells what other 
	derivableClassKeys,	-- things are needed as a result of a 
				-- deriving(C) clause
	numericTyKeys, cCallishTyKeys,

	mkUnboundName, isUnboundName
    ) where

#include "HsVersions.h"

import Module	  ( ModuleName, mkPrelModule, mkHomeModule, mkModuleName )
import OccName	  ( NameSpace, UserFS, varName, dataName, tcName, clsName, 
		    mkKindOccFS, mkOccFS
		  )
import RdrName	  ( RdrName, mkOrig, mkUnqual )
import UniqFM
import Unique	  ( Unique, Uniquable(..), hasKey,
		    mkPreludeMiscIdUnique, mkPreludeDataConUnique,
		    mkPreludeTyConUnique, mkPreludeClassUnique,
		    mkTupleTyConUnique
		  ) 
import BasicTypes ( Boxity(..), Arity )
import UniqFM	  ( UniqFM, listToUFM )
import Name	  ( Name, mkLocalName, mkKnownKeyGlobal, nameRdrName )
import RdrName    ( rdrNameOcc )
import SrcLoc     ( builtinSrcLoc, noSrcLoc )
import Util	  ( nOfThem )
import Panic	  ( panic )
\end{code}


%************************************************************************
%*									*
\subsection{Local Names}
%*									*
%************************************************************************

This *local* name is used by the interactive stuff

\begin{code}
itName uniq = mkLocalName uniq (mkOccFS varName FSLIT("it")) noSrcLoc
\end{code}

\begin{code}
-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkLocalName unboundKey (rdrNameOcc rdr_name) builtinSrcLoc

isUnboundName :: Name -> Bool
isUnboundName name = name `hasKey` unboundKey
\end{code}


%************************************************************************
%*									*
\subsection{Known key Names}
%*									*
%************************************************************************

This section tells what the compiler knows about the assocation of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in TysWiredIn etc.

\begin{code}
knownKeyNames :: [Name]
knownKeyNames
 =  [
	-- Type constructors (synonyms especially)
	ioTyConName, ioDataConName,
	runMainName,
	orderingTyConName,
	rationalTyConName,
	ratioDataConName,
	ratioTyConName,
	byteArrayTyConName,
	mutableByteArrayTyConName,
	foreignObjTyConName,
	foreignPtrTyConName,
	bcoPrimTyConName,
	stablePtrTyConName,
	stablePtrDataConName,

	--  Classes.  *Must* include:
	--  	classes that are grabbed by key (e.g., eqClassKey)
	--  	classes in "Class.standardClassKeys" (quite a few)
	eqClassName,			-- mentioned, derivable
	ordClassName,			-- derivable
	boundedClassName,		-- derivable
	numClassName,			-- mentioned, numeric
	enumClassName,			-- derivable
	monadClassName,
    	functorClassName,
	showClassName,			-- derivable
	realClassName,			-- numeric
	integralClassName,		-- numeric
	fractionalClassName,		-- numeric
	floatingClassName,		-- numeric
	realFracClassName,		-- numeric
	realFloatClassName,		-- numeric
	readClassName,			-- derivable
	ixClassName,			-- derivable (but it isn't Prelude.Ix; hmmm)
	cCallableClassName,		-- mentioned, ccallish
	cReturnableClassName,		-- mentioned, ccallish

	-- ClassOps 
	fromIntegerName,
	negateName,
	geName,
	minusName,
	enumFromName,
	enumFromThenName,
	enumFromToName,
	enumFromThenToName,
	fromEnumName,
	toEnumName,
	eqName,
	thenMName,
	returnMName,
	failMName,
	fromRationalName,

        -- not class methods, but overloaded (for parallel arrays)
	enumFromToPName,
	enumFromThenToPName,

	deRefStablePtrName,
	newStablePtrName,
	bindIOName,
	returnIOName,
	failIOName,

	-- Strings and lists
	mapName,
	appendName,
	unpackCStringName,
	unpackCStringAppendName,
	unpackCStringFoldrName,
	unpackCStringUtf8Name,

	-- List operations
	concatName,
	filterName,
	zipName,
	foldrName,
	buildName,
	augmentName,

        -- Parallel array operations
	nullPName,
	lengthPName,
	replicatePName,
	mapPName,
	filterPName,
	zipPName,
	crossPName,
	indexPName,
	toPName,
	bpermutePName,
	bpermuteDftPName,
	indexOfPName,

	-- FFI primitive types that are not wired-in.
	int8TyConName,
	int16TyConName,
	int32TyConName,
	int64TyConName,
	word8TyConName,
	word16TyConName,
	word32TyConName,
	word64TyConName,

	-- Others
	unsafeCoerceName,
	otherwiseIdName,
	plusIntegerName,
	timesIntegerName,
	eqStringName,
	assertName,
	runSTRepName,
	printName,
	splitName, fstName, sndName,	-- Used by splittery

	-- Others (needed for flattening and not mentioned before)
	andName,
	orName
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Module names}
%*									*
%************************************************************************

\begin{code}
pRELUDE_Name      = mkModuleName "Prelude"
gHC_PRIM_Name     = mkModuleName "GHC.Prim"	   -- Primitive types and values
gHC_BUILTIN_Name  = mkModuleName "GHC.Builtin"
pREL_BASE_Name    = mkModuleName "GHC.Base"
pREL_ENUM_Name    = mkModuleName "GHC.Enum"
pREL_SHOW_Name    = mkModuleName "GHC.Show"
pREL_READ_Name    = mkModuleName "GHC.Read"
pREL_NUM_Name     = mkModuleName "GHC.Num"
pREL_LIST_Name    = mkModuleName "GHC.List"
pREL_PARR_Name    = mkModuleName "GHC.PArr"
pREL_TUP_Name     = mkModuleName "Data.Tuple"
pREL_PACK_Name    = mkModuleName "GHC.Pack"
pREL_CONC_Name    = mkModuleName "GHC.Conc"
pREL_IO_BASE_Name = mkModuleName "GHC.IOBase"
pREL_ST_Name	  = mkModuleName "GHC.ST"
pREL_ARR_Name     = mkModuleName "GHC.Arr"
pREL_BYTEARR_Name = mkModuleName "PrelByteArr"
fOREIGN_PTR_Name  = mkModuleName "Foreign.ForeignPtr"
pREL_STABLE_Name  = mkModuleName "GHC.Stable"
pREL_SPLIT_Name   = mkModuleName "GHC.Split"
pREL_ADDR_Name    = mkModuleName "GHC.Addr"
pREL_PTR_Name     = mkModuleName "GHC.Ptr"
pREL_ERR_Name     = mkModuleName "GHC.Err"
pREL_REAL_Name    = mkModuleName "GHC.Real"
pREL_FLOAT_Name   = mkModuleName "GHC.Float"
pREL_TOP_HANDLER_Name = mkModuleName "GHC.TopHandler"
sYSTEM_IO_Name	  = mkModuleName "System.IO"

mAIN_Name	  = mkModuleName "Main"
pREL_INT_Name	  = mkModuleName "GHC.Int"
pREL_WORD_Name	  = mkModuleName "GHC.Word"

fOREIGNOBJ_Name	  = mkModuleName "ForeignObj"
aDDR_Name	  = mkModuleName "Addr"

gLA_EXTS_Name   = mkModuleName "GlaExts"

gHC_PRIM     	= mkPrelModule gHC_PRIM_Name
gHC_BUILTIN    	= mkPrelModule gHC_BUILTIN_Name
pREL_BASE    	= mkPrelModule pREL_BASE_Name
pREL_ADDR    	= mkPrelModule pREL_ADDR_Name
pREL_PTR    	= mkPrelModule pREL_PTR_Name
pREL_STABLE  	= mkPrelModule pREL_STABLE_Name
pREL_IO_BASE 	= mkPrelModule pREL_IO_BASE_Name
pREL_PACK    	= mkPrelModule pREL_PACK_Name
pREL_ERR     	= mkPrelModule pREL_ERR_Name
pREL_NUM     	= mkPrelModule pREL_NUM_Name
pREL_REAL    	= mkPrelModule pREL_REAL_Name
pREL_FLOAT   	= mkPrelModule pREL_FLOAT_Name
pRELUDE		= mkPrelModule pRELUDE_Name

iNTERACTIVE     = mkHomeModule (mkModuleName "$Interactive")
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupNameStr :: Boxity -> Int -> (ModuleName, UserFS)

mkTupNameStr Boxed 0 = (pREL_BASE_Name, FSLIT("()"))
mkTupNameStr Boxed 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr Boxed 2 = (pREL_TUP_Name, _PK_ "(,)")   -- not strictly necessary
mkTupNameStr Boxed 3 = (pREL_TUP_Name, _PK_ "(,,)")  -- ditto
mkTupNameStr Boxed 4 = (pREL_TUP_Name, _PK_ "(,,,)") -- ditto
mkTupNameStr Boxed n = (pREL_TUP_Name, _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")"))

mkTupNameStr Unboxed 0 = panic "Name.mkUbxTupNameStr: 0 ???"
mkTupNameStr Unboxed 1 = (gHC_PRIM_Name, _PK_ "(# #)") -- 1 and 0 both make sense!!!
mkTupNameStr Unboxed 2 = (gHC_PRIM_Name, _PK_ "(#,#)")
mkTupNameStr Unboxed 3 = (gHC_PRIM_Name, _PK_ "(#,,#)")
mkTupNameStr Unboxed 4 = (gHC_PRIM_Name, _PK_ "(#,,,#)")
mkTupNameStr Unboxed n = (gHC_PRIM_Name, _PK_ ("(#" ++ nOfThem (n-1) ',' ++ "#)"))

mkTupConRdrName :: NameSpace -> Boxity -> Arity -> RdrName 
mkTupConRdrName space boxity arity   = case mkTupNameStr boxity arity of
					  (mod, occ) -> mkOrig space mod occ
\end{code}


%************************************************************************
%*									*
\subsection{Unqualified RdrNames}
%*									*
%************************************************************************

\begin{code}
main_RDR_Unqual :: RdrName
main_RDR_Unqual = mkUnqual varName FSLIT("main")
-- Don't get a RdrName from PrelNames.mainName, because nameRdrName
-- gets an Orig RdrName, and we want a Qual or Unqual one.  An Unqual
-- one will do fine.
\end{code}


%************************************************************************
%*									*
\subsection{Commonly-used RdrNames}
%*									*
%************************************************************************

Many of these Names are not really "built in", but some parts of the
compiler (notably the deriving mechanism) need to mention their names,
and it's convenient to write them all down in one place.

\begin{code}
dollarMainName = varQual mAIN_Name FSLIT("$main") dollarMainKey
runMainName    = varQual pREL_TOP_HANDLER_Name FSLIT("runMain") runMainKey

-- Stuff from PrelGHC
usOnceTyConName  = kindQual FSLIT(".") usOnceTyConKey
usManyTyConName  = kindQual FSLIT("!") usManyTyConKey
superKindName    = kindQual FSLIT("KX") kindConKey
superBoxityName  = kindQual FSLIT("BX") boxityConKey
liftedConName    = kindQual FSLIT("*") liftedConKey
unliftedConName  = kindQual FSLIT("#") unliftedConKey
openKindConName  = kindQual FSLIT("?") anyBoxConKey
usageKindConName = kindQual FSLIT("$") usageConKey
typeConName	 = kindQual FSLIT("Type") typeConKey

funTyConName	    	      = tcQual  gHC_PRIM_Name FSLIT("(->)")  funTyConKey
charPrimTyConName    	      = tcQual  gHC_PRIM_Name FSLIT("Char#") charPrimTyConKey 
intPrimTyConName     	      = tcQual  gHC_PRIM_Name FSLIT("Int#") intPrimTyConKey 
int32PrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("Int32#") int32PrimTyConKey 
int64PrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("Int64#") int64PrimTyConKey 
wordPrimTyConName    	      = tcQual  gHC_PRIM_Name FSLIT("Word#") wordPrimTyConKey 
word32PrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Word32#") word32PrimTyConKey 
word64PrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Word64#") word64PrimTyConKey 
addrPrimTyConName    	      = tcQual  gHC_PRIM_Name FSLIT("Addr#") addrPrimTyConKey 
floatPrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("Float#") floatPrimTyConKey 
doublePrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Double#") doublePrimTyConKey 
statePrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("State#") statePrimTyConKey 
realWorldTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("RealWorld") realWorldTyConKey 
arrayPrimTyConName   	      = tcQual  gHC_PRIM_Name FSLIT("Array#") arrayPrimTyConKey 
byteArrayPrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("ByteArray#") byteArrayPrimTyConKey 
mutableArrayPrimTyConName     = tcQual  gHC_PRIM_Name FSLIT("MutableArray#") mutableArrayPrimTyConKey 
mutableByteArrayPrimTyConName = tcQual  gHC_PRIM_Name FSLIT("MutableByteArray#") mutableByteArrayPrimTyConKey 
mutVarPrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("MutVar#") mutVarPrimTyConKey 
mVarPrimTyConName	      = tcQual  gHC_PRIM_Name FSLIT("MVar#") mVarPrimTyConKey 
stablePtrPrimTyConName        = tcQual  gHC_PRIM_Name FSLIT("StablePtr#") stablePtrPrimTyConKey 
stableNamePrimTyConName       = tcQual  gHC_PRIM_Name FSLIT("StableName#") stableNamePrimTyConKey 
foreignObjPrimTyConName       = tcQual  gHC_PRIM_Name FSLIT("ForeignObj#") foreignObjPrimTyConKey 
bcoPrimTyConName 	      = tcQual  gHC_PRIM_Name FSLIT("BCO#") bcoPrimTyConKey 
weakPrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("Weak#") weakPrimTyConKey 
threadIdPrimTyConName  	      = tcQual  gHC_PRIM_Name FSLIT("ThreadId#") threadIdPrimTyConKey 
cCallableClassName   	      = clsQual gHC_BUILTIN_Name FSLIT("CCallable") cCallableClassKey
cReturnableClassName 	      = clsQual gHC_BUILTIN_Name FSLIT("CReturnable") cReturnableClassKey

-- PrelBase data types and constructors
charTyConName	  = tcQual   pREL_BASE_Name FSLIT("Char") charTyConKey
charDataConName   = dataQual pREL_BASE_Name FSLIT("C#") charDataConKey
intTyConName	  = tcQual   pREL_BASE_Name FSLIT("Int") intTyConKey
intDataConName	  = dataQual pREL_BASE_Name FSLIT("I#") intDataConKey
orderingTyConName = tcQual   pREL_BASE_Name FSLIT("Ordering") orderingTyConKey
boolTyConName	  = tcQual   pREL_BASE_Name FSLIT("Bool") boolTyConKey
falseDataConName  = dataQual pREL_BASE_Name FSLIT("False") falseDataConKey
trueDataConName	  = dataQual pREL_BASE_Name FSLIT("True") trueDataConKey
listTyConName	  = tcQual   pREL_BASE_Name FSLIT("[]") listTyConKey
nilDataConName 	  = dataQual pREL_BASE_Name FSLIT("[]") nilDataConKey
consDataConName	  = dataQual pREL_BASE_Name FSLIT(":") consDataConKey

-- PrelTup
fstName		  = varQual pREL_TUP_Name FSLIT("fst") fstIdKey
sndName		  = varQual pREL_TUP_Name FSLIT("snd") sndIdKey

-- Generics
crossTyConName     = tcQual   pREL_BASE_Name FSLIT(":*:") crossTyConKey
crossDataConName   = dataQual pREL_BASE_Name FSLIT(":*:") crossDataConKey
plusTyConName      = tcQual   pREL_BASE_Name FSLIT(":+:") plusTyConKey
inlDataConName     = dataQual pREL_BASE_Name FSLIT("Inl") inlDataConKey
inrDataConName     = dataQual pREL_BASE_Name FSLIT("Inr") inrDataConKey
genUnitTyConName   = tcQual   pREL_BASE_Name FSLIT("Unit") genUnitTyConKey
genUnitDataConName = dataQual pREL_BASE_Name FSLIT("Unit") genUnitDataConKey

-- Random PrelBase functions
unsafeCoerceName  = varQual pREL_BASE_Name FSLIT("unsafeCoerce") 
							     unsafeCoerceIdKey
otherwiseIdName   = varQual pREL_BASE_Name FSLIT("otherwise") otherwiseIdKey
appendName	  = varQual pREL_BASE_Name FSLIT("++")	     appendIdKey
foldrName	  = varQual pREL_BASE_Name FSLIT("foldr")     foldrIdKey
mapName	   	  = varQual pREL_BASE_Name FSLIT("map")	     mapIdKey
buildName	  = varQual pREL_BASE_Name FSLIT("build")     buildIdKey
augmentName	  = varQual pREL_BASE_Name FSLIT("augment")   augmentIdKey
eqStringName	  = varQual pREL_BASE_Name FSLIT("eqString")  eqStringIdKey
andName		  = varQual pREL_BASE_Name FSLIT("&&")	     andIdKey
orName		  = varQual pREL_BASE_Name FSLIT("||")	     orIdKey

-- Strings
unpackCStringName       = varQual pREL_BASE_Name FSLIT("unpackCString#") unpackCStringIdKey
unpackCStringAppendName = varQual pREL_BASE_Name FSLIT("unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual pREL_BASE_Name FSLIT("unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual pREL_BASE_Name FSLIT("unpackCStringUtf8#") unpackCStringUtf8IdKey

-- Classes Eq and Ord
eqClassName	  = clsQual pREL_BASE_Name FSLIT("Eq") eqClassKey
ordClassName	  = clsQual pREL_BASE_Name FSLIT("Ord") ordClassKey
eqName		  = varQual  pREL_BASE_Name FSLIT("==") eqClassOpKey
geName		  = varQual  pREL_BASE_Name FSLIT(">=") geClassOpKey

-- Class Monad
monadClassName	   = clsQual pREL_BASE_Name FSLIT("Monad") monadClassKey
thenMName	   = varQual pREL_BASE_Name FSLIT(">>=") thenMClassOpKey
returnMName	   = varQual pREL_BASE_Name FSLIT("return") returnMClassOpKey
failMName	   = varQual pREL_BASE_Name FSLIT("fail") failMClassOpKey

-- Class Functor
functorClassName  = clsQual pREL_BASE_Name FSLIT("Functor") functorClassKey

-- Class Show
showClassName	  = clsQual pREL_SHOW_Name FSLIT("Show") showClassKey

-- Class Read
readClassName	  = clsQual pREL_READ_Name FSLIT("Read") readClassKey

-- Module PrelNum
numClassName	  = clsQual pREL_NUM_Name FSLIT("Num") numClassKey
fromIntegerName   = varQual pREL_NUM_Name FSLIT("fromInteger") fromIntegerClassOpKey
minusName	  = varQual pREL_NUM_Name FSLIT("-") minusClassOpKey
negateName	  = varQual pREL_NUM_Name FSLIT("negate") negateClassOpKey
plusIntegerName   = varQual pREL_NUM_Name FSLIT("plusInteger") plusIntegerIdKey
timesIntegerName  = varQual pREL_NUM_Name FSLIT("timesInteger") timesIntegerIdKey
integerTyConName  = tcQual  pREL_NUM_Name FSLIT("Integer") integerTyConKey
smallIntegerDataConName = dataQual pREL_NUM_Name FSLIT("S#") smallIntegerDataConKey
largeIntegerDataConName = dataQual pREL_NUM_Name FSLIT("J#") largeIntegerDataConKey

-- PrelReal types and classes
rationalTyConName   = tcQual   pREL_REAL_Name  FSLIT("Rational") rationalTyConKey
ratioTyConName	    = tcQual   pREL_REAL_Name  FSLIT("Ratio") ratioTyConKey
ratioDataConName    = dataQual pREL_REAL_Name  FSLIT(":%") ratioDataConKey
realClassName	    = clsQual  pREL_REAL_Name  FSLIT("Real") realClassKey
integralClassName   = clsQual  pREL_REAL_Name  FSLIT("Integral") integralClassKey
realFracClassName   = clsQual  pREL_REAL_Name  FSLIT("RealFrac") realFracClassKey
fractionalClassName = clsQual  pREL_REAL_Name  FSLIT("Fractional") fractionalClassKey
fromRationalName    = varQual  pREL_REAL_Name  FSLIT("fromRational") fromRationalClassOpKey

-- PrelFloat classes
floatTyConName	   = tcQual   pREL_FLOAT_Name FSLIT("Float") floatTyConKey
floatDataConName   = dataQual pREL_FLOAT_Name FSLIT("F#") floatDataConKey
doubleTyConName    = tcQual   pREL_FLOAT_Name FSLIT("Double") doubleTyConKey
doubleDataConName  = dataQual pREL_FLOAT_Name FSLIT("D#") doubleDataConKey
floatingClassName  = clsQual  pREL_FLOAT_Name FSLIT("Floating") floatingClassKey
realFloatClassName = clsQual  pREL_FLOAT_Name FSLIT("RealFloat") realFloatClassKey

-- Class Ix
ixClassName	   = clsQual pREL_ARR_Name FSLIT("Ix") ixClassKey

-- Class Enum
enumClassName 	   = clsQual pREL_ENUM_Name FSLIT("Enum") enumClassKey
toEnumName	   = varQual pREL_ENUM_Name FSLIT("toEnum") toEnumClassOpKey
fromEnumName	   = varQual pREL_ENUM_Name FSLIT("fromEnum") fromEnumClassOpKey
enumFromName	   = varQual pREL_ENUM_Name FSLIT("enumFrom") enumFromClassOpKey
enumFromToName	   = varQual pREL_ENUM_Name FSLIT("enumFromTo") enumFromToClassOpKey
enumFromThenName   = varQual pREL_ENUM_Name FSLIT("enumFromThen") enumFromThenClassOpKey
enumFromThenToName = varQual pREL_ENUM_Name FSLIT("enumFromThenTo") enumFromThenToClassOpKey

-- Overloaded via Class Enum
enumFromToPName	   = varQual pREL_PARR_Name FSLIT("enumFromToP") enumFromToPIdKey
enumFromThenToPName= varQual pREL_PARR_Name FSLIT("enumFromThenToP") enumFromThenToPIdKey

-- Class Bounded
boundedClassName  = clsQual pREL_ENUM_Name FSLIT("Bounded") boundedClassKey

-- List functions
concatName	  = varQual pREL_LIST_Name FSLIT("concat") concatIdKey
filterName	  = varQual pREL_LIST_Name FSLIT("filter") filterIdKey
zipName	   	  = varQual pREL_LIST_Name FSLIT("zip") zipIdKey

-- parallel array types and functions
parrTyConName	  = tcQual  pREL_PARR_Name FSLIT("[::]")       parrTyConKey
parrDataConName   = dataQual pREL_PARR_Name FSLIT("PArr")      parrDataConKey
nullPName	  = varQual pREL_PARR_Name FSLIT("nullP")      nullPIdKey
lengthPName	  = varQual pREL_PARR_Name FSLIT("lengthP")    lengthPIdKey
replicatePName	  = varQual pREL_PARR_Name FSLIT("replicateP") replicatePIdKey
mapPName	  = varQual pREL_PARR_Name FSLIT("mapP")       mapPIdKey
filterPName	  = varQual pREL_PARR_Name FSLIT("filterP")    filterPIdKey
zipPName	  = varQual pREL_PARR_Name FSLIT("zipP")       zipPIdKey
crossPName	  = varQual pREL_PARR_Name FSLIT("crossP")     crossPIdKey
indexPName	  = varQual pREL_PARR_Name FSLIT("!:")	      indexPIdKey
toPName	          = varQual pREL_PARR_Name FSLIT("toP")	      toPIdKey
bpermutePName     = varQual pREL_PARR_Name FSLIT("bpermuteP")  bpermutePIdKey
bpermuteDftPName  = varQual pREL_PARR_Name FSLIT("bpermuteDftP") 
							      bpermuteDftPIdKey
indexOfPName      = varQual pREL_PARR_Name FSLIT("indexOfP")   indexOfPIdKey

-- IOBase things
ioTyConName	  = tcQual   pREL_IO_BASE_Name FSLIT("IO") ioTyConKey
ioDataConName     = dataQual pREL_IO_BASE_Name FSLIT("IO") ioDataConKey
bindIOName	  = varQual  pREL_IO_BASE_Name FSLIT("bindIO") bindIOIdKey
returnIOName	  = varQual  pREL_IO_BASE_Name FSLIT("returnIO") returnIOIdKey
failIOName	  = varQual  pREL_IO_BASE_Name FSLIT("failIO") failIOIdKey

-- IO things
printName	  = varQual sYSTEM_IO_Name FSLIT("print") printIdKey

-- Int, Word, and Addr things
int8TyConName     = tcQual pREL_INT_Name  FSLIT("Int8") int8TyConKey
int16TyConName    = tcQual pREL_INT_Name  FSLIT("Int16") int16TyConKey
int32TyConName    = tcQual pREL_INT_Name  FSLIT("Int32") int32TyConKey
int64TyConName    = tcQual pREL_INT_Name  FSLIT("Int64") int64TyConKey

word8TyConName    = tcQual pREL_WORD_Name FSLIT("Word8")  word8TyConKey
word16TyConName   = tcQual pREL_WORD_Name FSLIT("Word16") word16TyConKey
word32TyConName   = tcQual pREL_WORD_Name FSLIT("Word32") word32TyConKey
word64TyConName   = tcQual pREL_WORD_Name FSLIT("Word64") word64TyConKey

wordTyConName     = tcQual   pREL_WORD_Name FSLIT("Word")   wordTyConKey
wordDataConName   = dataQual pREL_WORD_Name FSLIT("W#")     wordDataConKey

addrTyConName	  = tcQual   aDDR_Name FSLIT("Addr") addrTyConKey
addrDataConName   = dataQual aDDR_Name FSLIT("A#") addrDataConKey

ptrTyConName	  = tcQual   pREL_PTR_Name FSLIT("Ptr") ptrTyConKey
ptrDataConName    = dataQual pREL_PTR_Name FSLIT("Ptr") ptrDataConKey

funPtrTyConName	  = tcQual   pREL_PTR_Name FSLIT("FunPtr") funPtrTyConKey
funPtrDataConName = dataQual pREL_PTR_Name FSLIT("FunPtr") funPtrDataConKey

-- Byte array types
byteArrayTyConName	  = tcQual pREL_BYTEARR_Name  FSLIT("ByteArray") byteArrayTyConKey
mutableByteArrayTyConName = tcQual pREL_BYTEARR_Name  FSLIT("MutableByteArray") mutableByteArrayTyConKey

-- Foreign objects and weak pointers
foreignObjTyConName   = tcQual   fOREIGNOBJ_Name FSLIT("ForeignObj") foreignObjTyConKey
foreignObjDataConName = dataQual fOREIGNOBJ_Name FSLIT("ForeignObj") foreignObjDataConKey
foreignPtrTyConName   = tcQual   fOREIGN_PTR_Name FSLIT("ForeignPtr") foreignPtrTyConKey
foreignPtrDataConName = dataQual fOREIGN_PTR_Name FSLIT("ForeignPtr") foreignPtrDataConKey
stablePtrTyConName    = tcQual   pREL_STABLE_Name FSLIT("StablePtr") stablePtrTyConKey
stablePtrDataConName  = dataQual pREL_STABLE_Name FSLIT("StablePtr") stablePtrDataConKey
deRefStablePtrName    = varQual  pREL_STABLE_Name FSLIT("deRefStablePtr") deRefStablePtrIdKey
newStablePtrName      = varQual  pREL_STABLE_Name FSLIT("newStablePtr") newStablePtrIdKey

errorName	   = varQual pREL_ERR_Name FSLIT("error") errorIdKey
assertName         = varQual gHC_BUILTIN_Name FSLIT("assert") assertIdKey
getTagName	   = varQual gHC_BUILTIN_Name FSLIT("getTag#") getTagIdKey
runSTRepName	   = varQual pREL_ST_Name  FSLIT("runSTRep") runSTRepIdKey

-- The "split" Id for splittable implicit parameters
splitName          = varQual pREL_SPLIT_Name FSLIT("split") splitIdKey
\end{code}

%************************************************************************
%*									*
\subsection{Known names}
%*									*
%************************************************************************

The following names are known to the compiler, but they don't require
pre-assigned keys.  Mostly these names are used in generating deriving
code, which is passed through the renamer anyway.

	THEY ARE ALL ORIGINAL NAMES, HOWEVER

\begin{code}
-- Lists and tuples
tupleCon_RDR, tupleTyCon_RDR		:: Int -> RdrName
ubxTupleCon_RDR, ubxTupleTyCon_RDR 	:: Int -> RdrName

tupleCon_RDR      = mkTupConRdrName dataName Boxed  
tupleTyCon_RDR    = mkTupConRdrName tcName   Boxed  
ubxTupleCon_RDR   = mkTupConRdrName dataName Unboxed
ubxTupleTyCon_RDR = mkTupConRdrName tcName   Unboxed

unitCon_RDR   	  = dataQual_RDR pREL_BASE_Name FSLIT("()")
unitTyCon_RDR 	  = tcQual_RDR   pREL_BASE_Name FSLIT("()")

and_RDR	   	   = varQual_RDR  pREL_BASE_Name FSLIT("&&")
not_RDR	   	   = varQual_RDR  pREL_BASE_Name FSLIT("not")
compose_RDR	   = varQual_RDR  pREL_BASE_Name FSLIT(".")
ne_RDR		   = varQual_RDR  pREL_BASE_Name FSLIT("/=")
le_RDR		   = varQual_RDR  pREL_BASE_Name FSLIT("<=")
lt_RDR		   = varQual_RDR  pREL_BASE_Name FSLIT("<")
gt_RDR		   = varQual_RDR  pREL_BASE_Name FSLIT(">")
ltTag_RDR      	   = dataQual_RDR pREL_BASE_Name FSLIT("LT")
eqTag_RDR      	   = dataQual_RDR pREL_BASE_Name FSLIT("EQ")
gtTag_RDR      	   = dataQual_RDR pREL_BASE_Name FSLIT("GT")
max_RDR		   = varQual_RDR  pREL_BASE_Name FSLIT("max")
min_RDR		   = varQual_RDR  pREL_BASE_Name FSLIT("min")
compare_RDR	   = varQual_RDR  pREL_BASE_Name FSLIT("compare")
showList_RDR	   = varQual_RDR  pREL_SHOW_Name FSLIT("showList")
showList___RDR     = varQual_RDR  pREL_SHOW_Name FSLIT("showList__")
showsPrec_RDR	   = varQual_RDR  pREL_SHOW_Name FSLIT("showsPrec")
showSpace_RDR	   = varQual_RDR  pREL_SHOW_Name FSLIT("showSpace")
showString_RDR	   = varQual_RDR  pREL_SHOW_Name FSLIT("showString")
showParen_RDR	   = varQual_RDR  pREL_SHOW_Name FSLIT("showParen")
readsPrec_RDR	   = varQual_RDR  pREL_READ_Name FSLIT("readsPrec")
readList_RDR	   = varQual_RDR  pREL_READ_Name FSLIT("readList")
readParen_RDR	   = varQual_RDR  pREL_READ_Name FSLIT("readParen")
lex_RDR		   = varQual_RDR  pREL_READ_Name FSLIT("lex")
readList___RDR     = varQual_RDR  pREL_READ_Name FSLIT("readList__")
times_RDR	   = varQual_RDR  pREL_NUM_Name FSLIT("*")
plus_RDR	   = varQual_RDR  pREL_NUM_Name FSLIT("+")
negate_RDR	   = varQual_RDR  pREL_NUM_Name FSLIT("negate")
range_RDR	   = varQual_RDR  pREL_ARR_Name FSLIT("range")
index_RDR	   = varQual_RDR  pREL_ARR_Name FSLIT("index")
inRange_RDR	   = varQual_RDR  pREL_ARR_Name FSLIT("inRange")
succ_RDR	   = varQual_RDR  pREL_ENUM_Name FSLIT("succ")
pred_RDR	   = varQual_RDR  pREL_ENUM_Name FSLIT("pred")
minBound_RDR	   = varQual_RDR  pREL_ENUM_Name FSLIT("minBound")
maxBound_RDR	   = varQual_RDR  pREL_ENUM_Name FSLIT("maxBound")
assertErr_RDR      = varQual_RDR  pREL_ERR_Name FSLIT("assertError")
\end{code}

These RDR names also have known keys, so we need to get back the RDR names to
populate the occurrence list above.

\begin{code}
funTyCon_RDR  		= nameRdrName funTyConName
nilCon_RDR    		= nameRdrName nilDataConName
listTyCon_RDR 		= nameRdrName listTyConName
parrTyCon_RDR 		= nameRdrName parrTyConName
ioTyCon_RDR		= nameRdrName ioTyConName
intTyCon_RDR 		= nameRdrName intTyConName
eq_RDR 			= nameRdrName eqName
ge_RDR 			= nameRdrName geName
numClass_RDR 		= nameRdrName numClassName
ordClass_RDR 		= nameRdrName ordClassName
map_RDR 		= nameRdrName mapName
append_RDR 		= nameRdrName appendName
foldr_RDR 		= nameRdrName foldrName
build_RDR 		= nameRdrName buildName
enumFromTo_RDR 		= nameRdrName enumFromToName
returnM_RDR 		= nameRdrName returnMName
thenM_RDR 		= nameRdrName thenMName
failM_RDR 		= nameRdrName failMName
false_RDR		= nameRdrName falseDataConName
true_RDR		= nameRdrName trueDataConName
error_RDR		= nameRdrName errorName
getTag_RDR		= nameRdrName getTagName
fromEnum_RDR		= nameRdrName fromEnumName
toEnum_RDR		= nameRdrName toEnumName
enumFrom_RDR		= nameRdrName enumFromName
mkInt_RDR		= nameRdrName intDataConName
enumFromThen_RDR	= nameRdrName enumFromThenName
enumFromThenTo_RDR	= nameRdrName enumFromThenToName
ratioDataCon_RDR	= nameRdrName ratioDataConName
plusInteger_RDR		= nameRdrName plusIntegerName
timesInteger_RDR	= nameRdrName timesIntegerName
enumClass_RDR		= nameRdrName enumClassName
monadClass_RDR		= nameRdrName monadClassName
ioDataCon_RDR		= nameRdrName ioDataConName
cCallableClass_RDR	= nameRdrName cCallableClassName
cReturnableClass_RDR	= nameRdrName cReturnableClassName
eqClass_RDR		= nameRdrName eqClassName
eqString_RDR		= nameRdrName eqStringName
unpackCString_RDR      	= nameRdrName unpackCStringName
unpackCStringFoldr_RDR 	= nameRdrName unpackCStringFoldrName
unpackCStringUtf8_RDR  	= nameRdrName unpackCStringUtf8Name
deRefStablePtr_RDR 	= nameRdrName deRefStablePtrName
newStablePtr_RDR 	= nameRdrName newStablePtrName
bindIO_RDR	  	= nameRdrName bindIOName
returnIO_RDR	  	= nameRdrName returnIOName
fromInteger_RDR		= nameRdrName fromIntegerName
fromRational_RDR	= nameRdrName fromRationalName
minus_RDR		= nameRdrName minusName
\end{code}

%************************************************************************
%*									*
\subsection{Local helpers}
%*									*
%************************************************************************

All these are original names; hence mkOrig

\begin{code}
varQual  mod str uq = mkKnownKeyGlobal (varQual_RDR  mod str) uq
dataQual mod str uq = mkKnownKeyGlobal (dataQual_RDR mod str) uq
tcQual   mod str uq = mkKnownKeyGlobal (tcQual_RDR   mod str) uq
clsQual  mod str uq = mkKnownKeyGlobal (clsQual_RDR  mod str) uq

kindQual str uq = mkLocalName uq (mkKindOccFS tcName str) builtinSrcLoc
	-- Kinds are not z-encoded in interface file, hence mkKindOccFS
	-- And they don't come from any particular module; indeed we always
	-- want to print them unqualified.  Hence the LocalName

varQual_RDR  mod str = mkOrig varName  mod str
tcQual_RDR   mod str = mkOrig tcName   mod str
clsQual_RDR  mod str = mkOrig clsName  mod str
dataQual_RDR mod str = mkOrig dataName mod str
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
%*									*
%************************************************************************

\begin{code}
boundedClassKey		= mkPreludeClassUnique 1 
enumClassKey		= mkPreludeClassUnique 2 
eqClassKey		= mkPreludeClassUnique 3 
floatingClassKey	= mkPreludeClassUnique 5 
fractionalClassKey	= mkPreludeClassUnique 6 
integralClassKey	= mkPreludeClassUnique 7 
monadClassKey		= mkPreludeClassUnique 8 
functorClassKey		= mkPreludeClassUnique 10
numClassKey		= mkPreludeClassUnique 11
ordClassKey		= mkPreludeClassUnique 12
readClassKey		= mkPreludeClassUnique 13
realClassKey		= mkPreludeClassUnique 14
realFloatClassKey	= mkPreludeClassUnique 15
realFracClassKey	= mkPreludeClassUnique 16
showClassKey		= mkPreludeClassUnique 17

cCallableClassKey	= mkPreludeClassUnique 18
cReturnableClassKey	= mkPreludeClassUnique 19

ixClassKey		= mkPreludeClassUnique 20
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
%*									*
%************************************************************************

\begin{code}
addrPrimTyConKey			= mkPreludeTyConUnique	1
addrTyConKey				= mkPreludeTyConUnique	2
arrayPrimTyConKey			= mkPreludeTyConUnique	3
boolTyConKey				= mkPreludeTyConUnique	4
byteArrayPrimTyConKey			= mkPreludeTyConUnique	5
charPrimTyConKey			= mkPreludeTyConUnique	7
charTyConKey				= mkPreludeTyConUnique  8
doublePrimTyConKey			= mkPreludeTyConUnique  9
doubleTyConKey				= mkPreludeTyConUnique 10 
floatPrimTyConKey			= mkPreludeTyConUnique 11
floatTyConKey				= mkPreludeTyConUnique 12
funTyConKey				= mkPreludeTyConUnique 13
intPrimTyConKey				= mkPreludeTyConUnique 14
intTyConKey				= mkPreludeTyConUnique 15
int8TyConKey				= mkPreludeTyConUnique 16
int16TyConKey				= mkPreludeTyConUnique 17
int32PrimTyConKey			= mkPreludeTyConUnique 18
int32TyConKey				= mkPreludeTyConUnique 19
int64PrimTyConKey			= mkPreludeTyConUnique 20
int64TyConKey				= mkPreludeTyConUnique 21
integerTyConKey				= mkPreludeTyConUnique 22
listTyConKey				= mkPreludeTyConUnique 23
foreignObjPrimTyConKey			= mkPreludeTyConUnique 24
foreignObjTyConKey			= mkPreludeTyConUnique 25
foreignPtrTyConKey			= mkPreludeTyConUnique 26
weakPrimTyConKey			= mkPreludeTyConUnique 27
mutableArrayPrimTyConKey		= mkPreludeTyConUnique 28
mutableByteArrayPrimTyConKey		= mkPreludeTyConUnique 29
orderingTyConKey			= mkPreludeTyConUnique 30
mVarPrimTyConKey		    	= mkPreludeTyConUnique 31
ratioTyConKey				= mkPreludeTyConUnique 32
rationalTyConKey			= mkPreludeTyConUnique 33
realWorldTyConKey			= mkPreludeTyConUnique 34
stablePtrPrimTyConKey			= mkPreludeTyConUnique 35
stablePtrTyConKey			= mkPreludeTyConUnique 36
statePrimTyConKey			= mkPreludeTyConUnique 50
stableNamePrimTyConKey			= mkPreludeTyConUnique 51
stableNameTyConKey		        = mkPreludeTyConUnique 52
mutableByteArrayTyConKey		= mkPreludeTyConUnique 53
mutVarPrimTyConKey			= mkPreludeTyConUnique 55
ioTyConKey				= mkPreludeTyConUnique 56
byteArrayTyConKey			= mkPreludeTyConUnique 57
wordPrimTyConKey			= mkPreludeTyConUnique 58
wordTyConKey				= mkPreludeTyConUnique 59
word8TyConKey				= mkPreludeTyConUnique 60
word16TyConKey				= mkPreludeTyConUnique 61 
word32PrimTyConKey			= mkPreludeTyConUnique 62 
word32TyConKey				= mkPreludeTyConUnique 63
word64PrimTyConKey			= mkPreludeTyConUnique 64
word64TyConKey				= mkPreludeTyConUnique 65
liftedConKey				= mkPreludeTyConUnique 66
unliftedConKey				= mkPreludeTyConUnique 67
anyBoxConKey				= mkPreludeTyConUnique 68
kindConKey				= mkPreludeTyConUnique 69
boxityConKey				= mkPreludeTyConUnique 70
typeConKey				= mkPreludeTyConUnique 71
threadIdPrimTyConKey			= mkPreludeTyConUnique 72
bcoPrimTyConKey				= mkPreludeTyConUnique 73
ptrTyConKey				= mkPreludeTyConUnique 74
funPtrTyConKey				= mkPreludeTyConUnique 75

-- Usage type constructors
usageConKey				= mkPreludeTyConUnique 76
usOnceTyConKey				= mkPreludeTyConUnique 77
usManyTyConKey				= mkPreludeTyConUnique 78

-- Generic Type Constructors
crossTyConKey		      		= mkPreludeTyConUnique 79
plusTyConKey		      		= mkPreludeTyConUnique 80
genUnitTyConKey				= mkPreludeTyConUnique 81

-- Parallel array type constructor
parrTyConKey				= mkPreludeTyConUnique 82

unitTyConKey = mkTupleTyConUnique Boxed 0
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*									*
%************************************************************************

\begin{code}
addrDataConKey				= mkPreludeDataConUnique  0
charDataConKey				= mkPreludeDataConUnique  1
consDataConKey				= mkPreludeDataConUnique  2
doubleDataConKey			= mkPreludeDataConUnique  3
falseDataConKey				= mkPreludeDataConUnique  4
floatDataConKey				= mkPreludeDataConUnique  5
intDataConKey				= mkPreludeDataConUnique  6
smallIntegerDataConKey			= mkPreludeDataConUnique  7
largeIntegerDataConKey			= mkPreludeDataConUnique  8
foreignObjDataConKey			= mkPreludeDataConUnique  9
foreignPtrDataConKey			= mkPreludeDataConUnique 10
nilDataConKey				= mkPreludeDataConUnique 11
ratioDataConKey				= mkPreludeDataConUnique 12
stablePtrDataConKey			= mkPreludeDataConUnique 13
stableNameDataConKey			= mkPreludeDataConUnique 14
trueDataConKey				= mkPreludeDataConUnique 15
wordDataConKey				= mkPreludeDataConUnique 16
ioDataConKey				= mkPreludeDataConUnique 17
ptrDataConKey				= mkPreludeDataConUnique 18
funPtrDataConKey			= mkPreludeDataConUnique 19

-- Generic data constructors
crossDataConKey		      		= mkPreludeDataConUnique 20
inlDataConKey		      		= mkPreludeDataConUnique 21
inrDataConKey		      		= mkPreludeDataConUnique 22
genUnitDataConKey			= mkPreludeDataConUnique 23

-- Data constructor for parallel arrays
parrDataConKey				= mkPreludeDataConUnique 24
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*									*
%************************************************************************

\begin{code}
absentErrorIdKey	      = mkPreludeMiscIdUnique  1
appendIdKey 		      = mkPreludeMiscIdUnique  2
augmentIdKey		      = mkPreludeMiscIdUnique  3
buildIdKey		      = mkPreludeMiscIdUnique  4
errorIdKey		      = mkPreludeMiscIdUnique  5
foldlIdKey		      = mkPreludeMiscIdUnique  6
foldrIdKey		      = mkPreludeMiscIdUnique  7
recSelErrIdKey		      = mkPreludeMiscIdUnique  8
integerMinusOneIdKey	      = mkPreludeMiscIdUnique  9
integerPlusOneIdKey	      = mkPreludeMiscIdUnique 10
integerPlusTwoIdKey	      = mkPreludeMiscIdUnique 11
integerZeroIdKey	      = mkPreludeMiscIdUnique 12
int2IntegerIdKey	      = mkPreludeMiscIdUnique 13
seqIdKey		      = mkPreludeMiscIdUnique 14
irrefutPatErrorIdKey	      = mkPreludeMiscIdUnique 15
eqStringIdKey		      = mkPreludeMiscIdUnique 16
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 17
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 18
errorCStringIdKey	      = mkPreludeMiscIdUnique 19 
parErrorIdKey		      = mkPreludeMiscIdUnique 20
parIdKey		      = mkPreludeMiscIdUnique 21
patErrorIdKey		      = mkPreludeMiscIdUnique 22
realWorldPrimIdKey	      = mkPreludeMiscIdUnique 23
recConErrorIdKey	      = mkPreludeMiscIdUnique 24
recUpdErrorIdKey	      = mkPreludeMiscIdUnique 25
traceIdKey		      = mkPreludeMiscIdUnique 26
unpackCStringUtf8IdKey	      = mkPreludeMiscIdUnique 27
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 28
unpackCStringFoldrIdKey	      = mkPreludeMiscIdUnique 29
unpackCStringIdKey	      = mkPreludeMiscIdUnique 30
ushowListIdKey		      = mkPreludeMiscIdUnique 31
unsafeCoerceIdKey	      = mkPreludeMiscIdUnique 32
concatIdKey		      = mkPreludeMiscIdUnique 33
filterIdKey		      = mkPreludeMiscIdUnique 34
zipIdKey		      = mkPreludeMiscIdUnique 35
bindIOIdKey		      = mkPreludeMiscIdUnique 36
returnIOIdKey		      = mkPreludeMiscIdUnique 37
deRefStablePtrIdKey	      = mkPreludeMiscIdUnique 38
newStablePtrIdKey	      = mkPreludeMiscIdUnique 39
getTagIdKey		      = mkPreludeMiscIdUnique 40
plusIntegerIdKey	      = mkPreludeMiscIdUnique 41
timesIntegerIdKey	      = mkPreludeMiscIdUnique 42
printIdKey		      = mkPreludeMiscIdUnique 43
failIOIdKey		      = mkPreludeMiscIdUnique 44
nullAddrIdKey		      = mkPreludeMiscIdUnique 46
voidArgIdKey		      = mkPreludeMiscIdUnique 47
splitIdKey		      = mkPreludeMiscIdUnique 48
fstIdKey		      = mkPreludeMiscIdUnique 49
sndIdKey		      = mkPreludeMiscIdUnique 50
otherwiseIdKey		      = mkPreludeMiscIdUnique 51
mapIdKey		      = mkPreludeMiscIdUnique 52
assertIdKey		      = mkPreludeMiscIdUnique 53
runSTRepIdKey		      = mkPreludeMiscIdUnique 54

dollarMainKey		      = mkPreludeMiscIdUnique 55
runMainKey		      = mkPreludeMiscIdUnique 56

andIdKey		      = mkPreludeMiscIdUnique 57
orIdKey			      = mkPreludeMiscIdUnique 58

-- Parallel array functions
nullPIdKey	              = mkPreludeMiscIdUnique 70
lengthPIdKey		      = mkPreludeMiscIdUnique 71
replicatePIdKey		      = mkPreludeMiscIdUnique 72
mapPIdKey		      = mkPreludeMiscIdUnique 73
filterPIdKey		      = mkPreludeMiscIdUnique 74
zipPIdKey		      = mkPreludeMiscIdUnique 75
crossPIdKey		      = mkPreludeMiscIdUnique 76
indexPIdKey		      = mkPreludeMiscIdUnique 77
toPIdKey		      = mkPreludeMiscIdUnique 78
enumFromToPIdKey              = mkPreludeMiscIdUnique 79
enumFromThenToPIdKey          = mkPreludeMiscIdUnique 80
bpermutePIdKey		      = mkPreludeMiscIdUnique 81
bpermuteDftPIdKey	      = mkPreludeMiscIdUnique 82
indexOfPIdKey		      = mkPreludeMiscIdUnique 83
\end{code}

Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.

\begin{code}
	-- Just a place holder for  unbound variables  produced by the renamer:
unboundKey		      = mkPreludeMiscIdUnique 101 
fromIntegerClassOpKey	      = mkPreludeMiscIdUnique 102
minusClassOpKey		      = mkPreludeMiscIdUnique 103
fromRationalClassOpKey	      = mkPreludeMiscIdUnique 104
enumFromClassOpKey	      = mkPreludeMiscIdUnique 105
enumFromThenClassOpKey	      = mkPreludeMiscIdUnique 106
enumFromToClassOpKey	      = mkPreludeMiscIdUnique 107
enumFromThenToClassOpKey      = mkPreludeMiscIdUnique 108
eqClassOpKey		      = mkPreludeMiscIdUnique 109
geClassOpKey		      = mkPreludeMiscIdUnique 110
negateClassOpKey	      = mkPreludeMiscIdUnique 111
failMClassOpKey		      = mkPreludeMiscIdUnique 112
thenMClassOpKey		      = mkPreludeMiscIdUnique 113 -- (>>=)
fromEnumClassOpKey	      = mkPreludeMiscIdUnique 115
returnMClassOpKey	      = mkPreludeMiscIdUnique 117
toEnumClassOpKey	      = mkPreludeMiscIdUnique 119
\end{code}


%************************************************************************
%*									*
\subsection{Standard groups of types}
%*									*
%************************************************************************

\begin{code}
numericTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, intTyConKey
	, integerTyConKey
	, doubleTyConKey
	, floatTyConKey
	]

	-- Renamer always imports these data decls replete with constructors
	-- so that desugarer can always see their constructors.  Ugh!
cCallishTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, byteArrayTyConKey
	, mutableByteArrayTyConKey
	, foreignObjTyConKey
	, foreignPtrTyConKey
	, stablePtrTyConKey
	, int8TyConKey
	, int16TyConKey
	, int32TyConKey
	, int64TyConKey
	, word8TyConKey
	, word16TyConKey
	, word32TyConKey
	, word64TyConKey
	]
\end{code}


%************************************************************************
%*									*
\subsection[Class-std-groups]{Standard groups of Prelude classes}
%*									*
%************************************************************************

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

@derivingOccurrences@ maps a class name to a list of the (qualified)
occurrences that will be mentioned by the derived code for the class
when it is later generated.  We don't need to put in things that are
WiredIn (because they are already mapped to their correct name by the
@NameSupply@.  The class itself, and all its class ops, is already
flagged as an occurrence so we don't need to mention that either.

@derivingOccurrences@ has an item for every derivable class, even if
that item is empty, because we treat lookup failure as indicating that
the class is illegal in a deriving clause.

\begin{code}
derivingOccurrences :: UniqFM [RdrName]
derivingOccurrences = listToUFM deriving_occ_info

derivableClassKeys  = map fst deriving_occ_info

deriving_occ_info
  = [ (eqClassKey, 	[intTyCon_RDR, and_RDR, not_RDR])
    , (ordClassKey, 	[intTyCon_RDR, compose_RDR, eqTag_RDR])
				-- EQ (from Ordering) is needed to force in the constructors
				-- as well as the type constructor.
    , (enumClassKey, 	[intTyCon_RDR, eq_RDR, ge_RDR, and_RDR, map_RDR, plus_RDR, showsPrec_RDR, append_RDR]) 
				-- The last two Enum deps are only used to produce better
				-- error msgs for derived toEnum methods.
    , (boundedClassKey,	[intTyCon_RDR])
    , (showClassKey,	[intTyCon_RDR, numClass_RDR, ordClass_RDR, compose_RDR, showString_RDR, 
			 showParen_RDR, showSpace_RDR, showList___RDR])
    , (readClassKey,	[intTyCon_RDR, numClass_RDR, ordClass_RDR, append_RDR,
                         foldr_RDR, build_RDR,
                             -- foldr and build required for list comprehension
                             -- KSW 2000-06
			 lex_RDR, readParen_RDR, readList___RDR, thenM_RDR])
			     -- returnM (and the rest of the Monad class decl) 
			     -- will be forced in as result of depending
			     -- on thenM.   -- SOF 1/99
    , (ixClassKey,	[intTyCon_RDR, numClass_RDR, and_RDR, map_RDR, enumFromTo_RDR,
                         foldr_RDR, build_RDR,
                             -- foldr and build required for list comprehension used
                             -- with single constructor types  -- KSW 2000-06
			 returnM_RDR, failM_RDR])
			     -- the last two are needed to force returnM, thenM and failM
			     -- in before typechecking the list(monad) comprehension
			     -- generated for derived Ix instances (range method)
			     -- of single constructor types.  -- SOF 8/97
    ]
	-- intTyCon: Practically any deriving needs Int, either for index calculations, 
	--		or for taggery.
	-- ordClass: really it's the methods that are actually used.
	-- numClass: for Int literals
\end{code}


NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.

\begin{code}
numericClassKeys =
	[ numClassKey
    	, realClassKey
    	, integralClassKey
	]
	++ fractionalClassKeys

fractionalClassKeys = 
    	[ fractionalClassKey
    	, floatingClassKey
    	, realFracClassKey
    	, realFloatClassKey
    	]

	-- the strictness analyser needs to know about numeric types
	-- (see SaAbsInt.lhs)
needsDataDeclCtxtClassKeys = -- see comments in TcDeriv
  	[ readClassKey
    	]

cCallishClassKeys = 
	[ cCallableClassKey
	, cReturnableClassKey
	]

standardClassKeys
  = derivableClassKeys ++ numericClassKeys ++ cCallishClassKeys
    --
    -- We have to have "CCallable" and "CReturnable" in the standard
    -- classes, so that if you go...
    --
    --	    _ccall_ foo ... 93{-numeric literal-} ...
    --
    -- ... it can do The Right Thing on the 93.

noDictClassKeys 	-- These classes are used only for type annotations;
			-- they are not implemented by dictionaries, ever.
  = cCallishClassKeys
\end{code}

