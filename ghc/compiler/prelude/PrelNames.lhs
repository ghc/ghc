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
itName uniq = mkLocalName uniq (mkOccFS varName SLIT("it")) noSrcLoc
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
	mainName,
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
    	monadPlusClassName,
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
    
	deRefStablePtrName,
	newStablePtrName,
	bindIOName,
	returnIOName,
	failIOName,

	-- Strings and lists
	mapName,
	appendName,
	unpackCStringName,
	unpackCStringListName,
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
	splitIdName, fstIdName, sndIdName	-- Used by splittery
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Module names}
%*									*
%************************************************************************

\begin{code}
pRELUDE_Name      = mkModuleName "Prelude"
pREL_GHC_Name     = mkModuleName "PrelGHC"	   -- Primitive types and values
pREL_BASE_Name    = mkModuleName "PrelBase"
pREL_ENUM_Name    = mkModuleName "PrelEnum"
pREL_SHOW_Name    = mkModuleName "PrelShow"
pREL_READ_Name    = mkModuleName "PrelRead"
pREL_NUM_Name     = mkModuleName "PrelNum"
pREL_LIST_Name    = mkModuleName "PrelList"
pREL_TUP_Name     = mkModuleName "PrelTup"
pREL_PACK_Name    = mkModuleName "PrelPack"
pREL_CONC_Name    = mkModuleName "PrelConc"
pREL_IO_BASE_Name = mkModuleName "PrelIOBase"
pREL_IO_Name	  = mkModuleName "PrelIO"
pREL_ST_Name	  = mkModuleName "PrelST"
pREL_ARR_Name     = mkModuleName "PrelArr"
pREL_BYTEARR_Name = mkModuleName "PrelByteArr"
pREL_FOREIGN_Name = mkModuleName "PrelForeign"
pREL_STABLE_Name  = mkModuleName "PrelStable"
pREL_SPLIT_Name   = mkModuleName "PrelSplit"
pREL_ADDR_Name    = mkModuleName "PrelAddr"
pREL_PTR_Name     = mkModuleName "PrelPtr"
pREL_ERR_Name     = mkModuleName "PrelErr"
pREL_REAL_Name    = mkModuleName "PrelReal"
pREL_FLOAT_Name   = mkModuleName "PrelFloat"

pREL_MAIN_Name    = mkModuleName "PrelMain"
mAIN_Name	  = mkModuleName "Main"
pREL_INT_Name	  = mkModuleName "PrelInt"
pREL_WORD_Name	  = mkModuleName "PrelWord"

fOREIGNOBJ_Name	  = mkModuleName "ForeignObj"
aDDR_Name	  = mkModuleName "Addr"

gLA_EXTS_Name   = mkModuleName "GlaExts"

pREL_GHC     	= mkPrelModule pREL_GHC_Name
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

iNTERACTIVE     = mkHomeModule (mkModuleName "$Interactive")
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupNameStr :: Boxity -> Int -> (ModuleName, UserFS)

mkTupNameStr Boxed 0 = (pREL_BASE_Name, SLIT("()"))
mkTupNameStr Boxed 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr Boxed 2 = (pREL_TUP_Name, _PK_ "(,)")   -- not strictly necessary
mkTupNameStr Boxed 3 = (pREL_TUP_Name, _PK_ "(,,)")  -- ditto
mkTupNameStr Boxed 4 = (pREL_TUP_Name, _PK_ "(,,,)") -- ditto
mkTupNameStr Boxed n = (pREL_TUP_Name, _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")"))

mkTupNameStr Unboxed 0 = panic "Name.mkUbxTupNameStr: 0 ???"
mkTupNameStr Unboxed 1 = (pREL_GHC_Name, _PK_ "(# #)") -- 1 and 0 both make sense!!!
mkTupNameStr Unboxed 2 = (pREL_GHC_Name, _PK_ "(#,#)")
mkTupNameStr Unboxed 3 = (pREL_GHC_Name, _PK_ "(#,,#)")
mkTupNameStr Unboxed 4 = (pREL_GHC_Name, _PK_ "(#,,,#)")
mkTupNameStr Unboxed n = (pREL_GHC_Name, _PK_ ("(#" ++ nOfThem (n-1) ',' ++ "#)"))

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
main_RDR_Unqual = mkUnqual varName SLIT("main")
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
mainName = varQual mAIN_Name SLIT("main") mainKey

-- Stuff from PrelGHC
usOnceTyConName  = kindQual SLIT(".") usOnceTyConKey
usManyTyConName  = kindQual SLIT("!") usManyTyConKey
superKindName    = kindQual SLIT("KX") kindConKey
superBoxityName  = kindQual SLIT("BX") boxityConKey
liftedConName    = kindQual SLIT("*") liftedConKey
unliftedConName  = kindQual SLIT("#") unliftedConKey
openKindConName  = kindQual SLIT("?") anyBoxConKey
usageKindConName = kindQual SLIT("$") usageConKey
typeConName	 = kindQual SLIT("Type") typeConKey

funTyConName	    	      = tcQual  pREL_GHC_Name SLIT("(->)")  funTyConKey
charPrimTyConName    	      = tcQual  pREL_GHC_Name SLIT("Char#") charPrimTyConKey 
intPrimTyConName     	      = tcQual  pREL_GHC_Name SLIT("Int#") intPrimTyConKey 
int32PrimTyConName	      = tcQual  pREL_GHC_Name SLIT("Int32#") int32PrimTyConKey 
int64PrimTyConName   	      = tcQual  pREL_GHC_Name SLIT("Int64#") int64PrimTyConKey 
wordPrimTyConName    	      = tcQual  pREL_GHC_Name SLIT("Word#") wordPrimTyConKey 
word32PrimTyConName  	      = tcQual  pREL_GHC_Name SLIT("Word32#") word32PrimTyConKey 
word64PrimTyConName  	      = tcQual  pREL_GHC_Name SLIT("Word64#") word64PrimTyConKey 
addrPrimTyConName    	      = tcQual  pREL_GHC_Name SLIT("Addr#") addrPrimTyConKey 
floatPrimTyConName   	      = tcQual  pREL_GHC_Name SLIT("Float#") floatPrimTyConKey 
doublePrimTyConName  	      = tcQual  pREL_GHC_Name SLIT("Double#") doublePrimTyConKey 
statePrimTyConName   	      = tcQual  pREL_GHC_Name SLIT("State#") statePrimTyConKey 
realWorldTyConName   	      = tcQual  pREL_GHC_Name SLIT("RealWorld") realWorldTyConKey 
arrayPrimTyConName   	      = tcQual  pREL_GHC_Name SLIT("Array#") arrayPrimTyConKey 
byteArrayPrimTyConName	      = tcQual  pREL_GHC_Name SLIT("ByteArray#") byteArrayPrimTyConKey 
mutableArrayPrimTyConName     = tcQual  pREL_GHC_Name SLIT("MutableArray#") mutableArrayPrimTyConKey 
mutableByteArrayPrimTyConName = tcQual  pREL_GHC_Name SLIT("MutableByteArray#") mutableByteArrayPrimTyConKey 
mutVarPrimTyConName	      = tcQual  pREL_GHC_Name SLIT("MutVar#") mutVarPrimTyConKey 
mVarPrimTyConName	      = tcQual  pREL_GHC_Name SLIT("MVar#") mVarPrimTyConKey 
stablePtrPrimTyConName        = tcQual  pREL_GHC_Name SLIT("StablePtr#") stablePtrPrimTyConKey 
stableNamePrimTyConName       = tcQual  pREL_GHC_Name SLIT("StableName#") stableNamePrimTyConKey 
foreignObjPrimTyConName       = tcQual  pREL_GHC_Name SLIT("ForeignObj#") foreignObjPrimTyConKey 
bcoPrimTyConName 	      = tcQual  pREL_GHC_Name SLIT("BCO#") bcoPrimTyConKey 
weakPrimTyConName  	      = tcQual  pREL_GHC_Name SLIT("Weak#") weakPrimTyConKey 
threadIdPrimTyConName  	      = tcQual  pREL_GHC_Name SLIT("ThreadId#") threadIdPrimTyConKey 
cCallableClassName   	      = clsQual pREL_GHC_Name SLIT("CCallable") cCallableClassKey
cReturnableClassName 	      = clsQual pREL_GHC_Name SLIT("CReturnable") cReturnableClassKey

-- PrelBase data types and constructors
charTyConName	  = tcQual   pREL_BASE_Name SLIT("Char") charTyConKey
charDataConName   = dataQual pREL_BASE_Name SLIT("C#") charDataConKey
intTyConName	  = tcQual   pREL_BASE_Name SLIT("Int") intTyConKey
intDataConName	  = dataQual pREL_BASE_Name SLIT("I#") intDataConKey
orderingTyConName = tcQual   pREL_BASE_Name SLIT("Ordering") orderingTyConKey
boolTyConName	  = tcQual   pREL_BASE_Name SLIT("Bool") boolTyConKey
falseDataConName  = dataQual pREL_BASE_Name SLIT("False") falseDataConKey
trueDataConName	  = dataQual pREL_BASE_Name SLIT("True") trueDataConKey
listTyConName	  = tcQual   pREL_BASE_Name SLIT("[]") listTyConKey
nilDataConName 	  = dataQual pREL_BASE_Name SLIT("[]") nilDataConKey
consDataConName	  = dataQual pREL_BASE_Name SLIT(":") consDataConKey

-- PrelTup
fstIdName	  = varQual pREL_TUP_Name SLIT("fst") fstIdKey
sndIdName	  = varQual pREL_TUP_Name SLIT("snd") sndIdKey

-- Generics
crossTyConName     = tcQual   pREL_BASE_Name SLIT(":*:") crossTyConKey
crossDataConName   = dataQual pREL_BASE_Name SLIT(":*:") crossDataConKey
plusTyConName      = tcQual   pREL_BASE_Name SLIT(":+:") plusTyConKey
inlDataConName     = dataQual pREL_BASE_Name SLIT("Inl") inlDataConKey
inrDataConName     = dataQual pREL_BASE_Name SLIT("Inr") inrDataConKey
genUnitTyConName   = tcQual   pREL_BASE_Name SLIT("Unit") genUnitTyConKey
genUnitDataConName = dataQual pREL_BASE_Name SLIT("Unit") genUnitDataConKey

-- Random PrelBase functions
unsafeCoerceName  = varQual pREL_BASE_Name SLIT("unsafeCoerce") unsafeCoerceIdKey
otherwiseIdName   = varQual pREL_BASE_Name SLIT("otherwise") otherwiseIdKey
appendName	  = varQual pREL_BASE_Name SLIT("++") appendIdKey
foldrName	  = varQual pREL_BASE_Name SLIT("foldr") foldrIdKey
mapName	   	  = varQual pREL_BASE_Name SLIT("map") mapIdKey
buildName	  = varQual pREL_BASE_Name SLIT("build") buildIdKey
augmentName	  = varQual pREL_BASE_Name SLIT("augment") augmentIdKey
eqStringName	  = varQual pREL_BASE_Name SLIT("eqString") eqStringIdKey

-- Strings
unpackCStringName       = varQual pREL_BASE_Name SLIT("unpackCString#") unpackCStringIdKey
unpackCStringListName   = varQual pREL_BASE_Name SLIT("unpackCStringList#") unpackCStringListIdKey
unpackCStringAppendName = varQual pREL_BASE_Name SLIT("unpackAppendCString#") unpackCStringAppendIdKey
unpackCStringFoldrName  = varQual pREL_BASE_Name SLIT("unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual pREL_BASE_Name SLIT("unpackCStringUtf8#") unpackCStringUtf8IdKey

-- Classes Eq and Ord
eqClassName	  = clsQual pREL_BASE_Name SLIT("Eq") eqClassKey
ordClassName	  = clsQual pREL_BASE_Name SLIT("Ord") ordClassKey
eqName		  = varQual  pREL_BASE_Name SLIT("==") eqClassOpKey
geName		  = varQual  pREL_BASE_Name SLIT(">=") geClassOpKey

-- Class Monad
monadClassName	   = clsQual pREL_BASE_Name SLIT("Monad") monadClassKey
monadPlusClassName = clsQual pREL_BASE_Name SLIT("MonadPlus") monadPlusClassKey
thenMName	   = varQual pREL_BASE_Name SLIT(">>=") thenMClassOpKey
returnMName	   = varQual pREL_BASE_Name SLIT("return") returnMClassOpKey
failMName	   = varQual pREL_BASE_Name SLIT("fail") failMClassOpKey

-- Class Functor
functorClassName  = clsQual pREL_BASE_Name SLIT("Functor") functorClassKey

-- Class Show
showClassName	  = clsQual pREL_SHOW_Name SLIT("Show") showClassKey

-- Class Read
readClassName	  = clsQual pREL_READ_Name SLIT("Read") readClassKey

-- Module PrelNum
numClassName	  = clsQual pREL_NUM_Name SLIT("Num") numClassKey
fromIntegerName   = varQual pREL_NUM_Name SLIT("fromInteger") fromIntegerClassOpKey
minusName	  = varQual pREL_NUM_Name SLIT("-") minusClassOpKey
negateName	  = varQual pREL_NUM_Name SLIT("negate") negateClassOpKey
plusIntegerName   = varQual pREL_NUM_Name SLIT("plusInteger") plusIntegerIdKey
timesIntegerName  = varQual pREL_NUM_Name SLIT("timesInteger") timesIntegerIdKey
integerTyConName  = tcQual  pREL_NUM_Name SLIT("Integer") integerTyConKey
smallIntegerDataConName = dataQual pREL_NUM_Name SLIT("S#") smallIntegerDataConKey
largeIntegerDataConName = dataQual pREL_NUM_Name SLIT("J#") largeIntegerDataConKey

-- PrelReal types and classes
rationalTyConName   = tcQual   pREL_REAL_Name  SLIT("Rational") rationalTyConKey
ratioTyConName	    = tcQual   pREL_REAL_Name  SLIT("Ratio") ratioTyConKey
ratioDataConName    = dataQual pREL_REAL_Name  SLIT(":%") ratioDataConKey
realClassName	    = clsQual  pREL_REAL_Name  SLIT("Real") realClassKey
integralClassName   = clsQual  pREL_REAL_Name  SLIT("Integral") integralClassKey
realFracClassName   = clsQual  pREL_REAL_Name  SLIT("RealFrac") realFracClassKey
fractionalClassName = clsQual  pREL_REAL_Name  SLIT("Fractional") fractionalClassKey
fromRationalName    = varQual  pREL_REAL_Name  SLIT("fromRational") fromRationalClassOpKey

-- PrelFloat classes
floatTyConName	   = tcQual   pREL_FLOAT_Name SLIT("Float") floatTyConKey
floatDataConName   = dataQual pREL_FLOAT_Name SLIT("F#") floatDataConKey
doubleTyConName    = tcQual   pREL_FLOAT_Name SLIT("Double") doubleTyConKey
doubleDataConName  = dataQual pREL_FLOAT_Name SLIT("D#") doubleDataConKey
floatingClassName  = clsQual  pREL_FLOAT_Name SLIT("Floating") floatingClassKey
realFloatClassName = clsQual  pREL_FLOAT_Name SLIT("RealFloat") realFloatClassKey

-- Class Ix
ixClassName	   = clsQual pREL_ARR_Name SLIT("Ix") ixClassKey

-- Class Enum
enumClassName 	   = clsQual pREL_ENUM_Name SLIT("Enum") enumClassKey
toEnumName	   = varQual pREL_ENUM_Name SLIT("toEnum") toEnumClassOpKey
fromEnumName	   = varQual pREL_ENUM_Name SLIT("fromEnum") fromEnumClassOpKey
enumFromName	   = varQual pREL_ENUM_Name SLIT("enumFrom") enumFromClassOpKey
enumFromToName	   = varQual pREL_ENUM_Name SLIT("enumFromTo") enumFromToClassOpKey
enumFromThenName   = varQual pREL_ENUM_Name SLIT("enumFromThen") enumFromThenClassOpKey
enumFromThenToName = varQual pREL_ENUM_Name SLIT("enumFromThenTo") enumFromThenToClassOpKey

-- Class Bounded
boundedClassName  = clsQual pREL_ENUM_Name SLIT("Bounded") boundedClassKey

-- List functions
concatName	  = varQual pREL_LIST_Name SLIT("concat") concatIdKey
filterName	  = varQual pREL_LIST_Name SLIT("filter") filterIdKey
zipName	   	  = varQual pREL_LIST_Name SLIT("zip") zipIdKey

-- IOBase things
ioTyConName	  = tcQual   pREL_IO_BASE_Name SLIT("IO") ioTyConKey
ioDataConName     = dataQual pREL_IO_BASE_Name SLIT("IO") ioDataConKey
bindIOName	  = varQual  pREL_IO_BASE_Name SLIT("bindIO") bindIOIdKey
returnIOName	  = varQual  pREL_IO_BASE_Name SLIT("returnIO") returnIOIdKey
failIOName	  = varQual  pREL_IO_BASE_Name SLIT("failIO") failIOIdKey

-- IO things
printName	  = varQual pREL_IO_Name SLIT("print") printIdKey

-- Int, Word, and Addr things
int8TyConName     = tcQual pREL_INT_Name  SLIT("Int8") int8TyConKey
int16TyConName    = tcQual pREL_INT_Name  SLIT("Int16") int16TyConKey
int32TyConName    = tcQual pREL_INT_Name  SLIT("Int32") int32TyConKey
int64TyConName    = tcQual pREL_INT_Name  SLIT("Int64") int64TyConKey

word8TyConName    = tcQual pREL_WORD_Name SLIT("Word8")  word8TyConKey
word16TyConName   = tcQual pREL_WORD_Name SLIT("Word16") word16TyConKey
word32TyConName   = tcQual pREL_WORD_Name SLIT("Word32") word32TyConKey
word64TyConName   = tcQual pREL_WORD_Name SLIT("Word64") word64TyConKey

wordTyConName     = tcQual   pREL_WORD_Name SLIT("Word")   wordTyConKey
wordDataConName   = dataQual pREL_WORD_Name SLIT("W#")     wordDataConKey

addrTyConName	  = tcQual   aDDR_Name SLIT("Addr") addrTyConKey
addrDataConName   = dataQual aDDR_Name SLIT("A#") addrDataConKey

ptrTyConName	  = tcQual   pREL_PTR_Name SLIT("Ptr") ptrTyConKey
ptrDataConName    = dataQual pREL_PTR_Name SLIT("Ptr") ptrDataConKey

funPtrTyConName	  = tcQual   pREL_PTR_Name SLIT("FunPtr") funPtrTyConKey
funPtrDataConName = dataQual pREL_PTR_Name SLIT("FunPtr") funPtrDataConKey

-- Byte array types
byteArrayTyConName	  = tcQual pREL_BYTEARR_Name  SLIT("ByteArray") byteArrayTyConKey
mutableByteArrayTyConName = tcQual pREL_BYTEARR_Name  SLIT("MutableByteArray") mutableByteArrayTyConKey

-- Forign objects and weak pointers
foreignObjTyConName   = tcQual   fOREIGNOBJ_Name SLIT("ForeignObj") foreignObjTyConKey
foreignObjDataConName = dataQual fOREIGNOBJ_Name SLIT("ForeignObj") foreignObjDataConKey
foreignPtrTyConName   = tcQual   pREL_FOREIGN_Name SLIT("ForeignPtr") foreignPtrTyConKey
foreignPtrDataConName = dataQual pREL_FOREIGN_Name SLIT("ForeignPtr") foreignPtrDataConKey
stablePtrTyConName    = tcQual   pREL_STABLE_Name SLIT("StablePtr") stablePtrTyConKey
stablePtrDataConName  = dataQual pREL_STABLE_Name SLIT("StablePtr") stablePtrDataConKey
deRefStablePtrName    = varQual  pREL_STABLE_Name SLIT("deRefStablePtr") deRefStablePtrIdKey
newStablePtrName      = varQual  pREL_STABLE_Name SLIT("newStablePtr") newStablePtrIdKey

errorName	   = varQual pREL_ERR_Name SLIT("error") errorIdKey
assertName         = varQual pREL_GHC_Name SLIT("assert") assertIdKey
getTagName	   = varQual pREL_GHC_Name SLIT("getTag#") getTagIdKey
runSTRepName	   = varQual pREL_ST_Name  SLIT("runSTRep") runSTRepIdKey

-- The "split" Id for splittable implicit parameters
splitIdName = varQual pREL_SPLIT_Name SLIT("split") splitIdKey
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

unitCon_RDR   	  = dataQual_RDR pREL_BASE_Name SLIT("()")
unitTyCon_RDR 	  = tcQual_RDR   pREL_BASE_Name SLIT("()")

and_RDR	   	   = varQual_RDR  pREL_BASE_Name SLIT("&&")
not_RDR	   	   = varQual_RDR  pREL_BASE_Name SLIT("not")
compose_RDR	   = varQual_RDR  pREL_BASE_Name SLIT(".")
ne_RDR		   = varQual_RDR  pREL_BASE_Name SLIT("/=")
le_RDR		   = varQual_RDR  pREL_BASE_Name SLIT("<=")
lt_RDR		   = varQual_RDR  pREL_BASE_Name SLIT("<")
gt_RDR		   = varQual_RDR  pREL_BASE_Name SLIT(">")
ltTag_RDR      	   = dataQual_RDR pREL_BASE_Name SLIT("LT")
eqTag_RDR      	   = dataQual_RDR pREL_BASE_Name SLIT("EQ")
gtTag_RDR      	   = dataQual_RDR pREL_BASE_Name SLIT("GT")
max_RDR		   = varQual_RDR  pREL_BASE_Name SLIT("max")
min_RDR		   = varQual_RDR  pREL_BASE_Name SLIT("min")
compare_RDR	   = varQual_RDR  pREL_BASE_Name SLIT("compare")
showList_RDR	   = varQual_RDR  pREL_SHOW_Name SLIT("showList")
showList___RDR     = varQual_RDR  pREL_SHOW_Name SLIT("showList__")
showsPrec_RDR	   = varQual_RDR  pREL_SHOW_Name SLIT("showsPrec")
showSpace_RDR	   = varQual_RDR  pREL_SHOW_Name SLIT("showSpace")
showString_RDR	   = varQual_RDR  pREL_SHOW_Name SLIT("showString")
showParen_RDR	   = varQual_RDR  pREL_SHOW_Name SLIT("showParen")
readsPrec_RDR	   = varQual_RDR  pREL_READ_Name SLIT("readsPrec")
readList_RDR	   = varQual_RDR  pREL_READ_Name SLIT("readList")
readParen_RDR	   = varQual_RDR  pREL_READ_Name SLIT("readParen")
lex_RDR		   = varQual_RDR  pREL_READ_Name SLIT("lex")
readList___RDR     = varQual_RDR  pREL_READ_Name SLIT("readList__")
times_RDR	   = varQual_RDR  pREL_NUM_Name SLIT("*")
plus_RDR	   = varQual_RDR  pREL_NUM_Name SLIT("+")
negate_RDR	   = varQual_RDR  pREL_NUM_Name SLIT("negate")
range_RDR	   = varQual_RDR  pREL_ARR_Name SLIT("range")
index_RDR	   = varQual_RDR  pREL_ARR_Name SLIT("index")
inRange_RDR	   = varQual_RDR  pREL_ARR_Name SLIT("inRange")
succ_RDR	   = varQual_RDR  pREL_ENUM_Name SLIT("succ")
pred_RDR	   = varQual_RDR  pREL_ENUM_Name SLIT("pred")
minBound_RDR	   = varQual_RDR  pREL_ENUM_Name SLIT("minBound")
maxBound_RDR	   = varQual_RDR  pREL_ENUM_Name SLIT("maxBound")
assertErr_RDR      = varQual_RDR  pREL_ERR_Name SLIT("assertError")
\end{code}

These RDR names also have known keys, so we need to get back the RDR names to
populate the occurrence list above.

\begin{code}
funTyCon_RDR  		= nameRdrName funTyConName
nilCon_RDR    		= nameRdrName nilDataConName
listTyCon_RDR 		= nameRdrName listTyConName
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
monadPlusClassKey	= mkPreludeClassUnique 9
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
unpackCStringListIdKey	      = mkPreludeMiscIdUnique 45
nullAddrIdKey		      = mkPreludeMiscIdUnique 46
voidArgIdKey		      = mkPreludeMiscIdUnique 47
splitIdKey		      = mkPreludeMiscIdUnique 48
fstIdKey		      = mkPreludeMiscIdUnique 49
sndIdKey		      = mkPreludeMiscIdUnique 50
\end{code}

Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.

\begin{code}
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
	-- Just a place holder for  unbound variables  produced by the renamer:
unboundKey		      = mkPreludeMiscIdUnique 114 
fromEnumClassOpKey	      = mkPreludeMiscIdUnique 115
			      
mainKey			      = mkPreludeMiscIdUnique 116
returnMClassOpKey	      = mkPreludeMiscIdUnique 117
otherwiseIdKey		      = mkPreludeMiscIdUnique 118
toEnumClassOpKey	      = mkPreludeMiscIdUnique 119
mapIdKey		      = mkPreludeMiscIdUnique 120
\end{code}

\begin{code}
assertIdKey		      = mkPreludeMiscIdUnique 121
runSTRepIdKey		      = mkPreludeMiscIdUnique 122
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

