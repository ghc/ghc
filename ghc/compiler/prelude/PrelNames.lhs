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
	knownKeyRdrNames, 
        mkTupNameStr, mkTupConRdrName,

	------------------------------------------------------------
	-- Prelude modules
	pREL_GHC, pREL_BASE, pREL_ADDR, pREL_STABLE,
	pREL_IO_BASE, pREL_PACK, pREL_ERR, pREL_NUM, pREL_FLOAT, pREL_REAL,

	------------------------------------------------------------
	-- Module names (both Prelude and otherwise)
	pREL_GHC_Name, pRELUDE_Name, pREL_MAIN_Name, mAIN_Name, 

	------------------------------------------------------------
	-- Original RdrNames for a few things
        main_RDR, 
	deRefStablePtr_RDR, makeStablePtr_RDR, 
	ioTyCon_RDR, ioDataCon_RDR, bindIO_RDR, returnIO_RDR,
	unpackCString_RDR, unpackCStringFoldr_RDR, unpackCStringUtf8_RDR,
	eqClass_RDR, foldr_RDR, build_RDR,
	ccallableClass_RDR, creturnableClass_RDR, 
	monadClass_RDR, enumClass_RDR, ordClass_RDR,
	ratioDataCon_RDR, negate_RDR, assertErr_RDR,
	plusInteger_RDR, timesInteger_RDR, eqString_RDR,

	-- Plus a whole lot more needed only in TcGenDeriv
	eq_RDR, ne_RDR, not_RDR, compare_RDR, ge_RDR, le_RDR, gt_RDR,
	ltTag_RDR, eqTag_RDR, gtTag_RDR, getTag_RDR,
	and_RDR, true_RDR, false_RDR,
	succ_RDR, pred_RDR, toEnum_RDR, fromEnum_RDR, 
	minBound_RDR, maxBound_RDR,
	enumFrom_RDR, enumFromThen_RDR, enumFromTo_RDR, enumFromThenTo_RDR,
	map_RDR, append_RDR, compose_RDR,
	plus_RDR, times_RDR, mkInt_RDR, 
	error_RDR,
	range_RDR, inRange_RDR, index_RDR,
	readList___RDR, readList_RDR, readsPrec_RDR, lex_RDR, readParen_RDR,
	showList_RDR, showList___RDR, showsPrec_RDR, showString_RDR, showSpace_RDR, showParen_RDR,

	------------------------------------------------------------
	-- Goups of classes and types
	needsDataDeclCtxtClassKeys, cCallishClassKeys, noDictClassKeys,
	fractionalClassKeys, numericClassKeys, standardClassKeys,
	derivingOccurrences, 	-- For a given class C, this tells what other 
	derivableClassKeys,	-- things are needed as a result of a 
				-- deriving(C) clause
	numericTyKeys, cCallishTyKeys, 

	------------------------------------------------------------
	-- Keys
	absentErrorIdKey, addrDataConKey, addrPrimTyConKey, addrTyConKey,
	appendIdKey, arrayPrimTyConKey, assertIdKey, augmentIdKey,
	bcoPrimTyConKey, bindIOIdKey, boolTyConKey, boundedClassKey,
	boxedConKey, buildIdKey, byteArrayPrimTyConKey, byteArrayTyConKey,
	cCallableClassKey, cReturnableClassKey, charDataConKey,
	charPrimTyConKey, charTyConKey, concatIdKey, consDataConKey,
	deRefStablePtrIdKey, doubleDataConKey, doublePrimTyConKey,
	doubleTyConKey, enumClassKey, enumFromClassOpKey,
	enumFromThenClassOpKey, enumFromThenToClassOpKey,
	enumFromToClassOpKey, eqClassKey, eqClassOpKey, eqStringIdKey,
	errorIdKey, falseDataConKey, failMClassOpKey, filterIdKey,
	floatDataConKey, floatPrimTyConKey, floatTyConKey, floatingClassKey,
	foldlIdKey, foldrIdKey, foreignObjDataConKey, foreignObjPrimTyConKey,
	foreignObjTyConKey, fractionalClassKey, fromEnumClassOpKey,
	fromIntClassOpKey, fromIntegerClassOpKey, fromRationalClassOpKey,
	funTyConKey, functorClassKey, geClassOpKey, getTagIdKey,
	intDataConKey, intPrimTyConKey, intTyConKey, int8TyConKey,
	int16TyConKey, int32TyConKey, int64PrimTyConKey, int64TyConKey,
	smallIntegerDataConKey, largeIntegerDataConKey, integerMinusOneIdKey,
	integerPlusOneIdKey, integerPlusTwoIdKey, int2IntegerIdKey,
	integerTyConKey, integerZeroIdKey, integralClassKey,
	irrefutPatErrorIdKey, ixClassKey, listTyConKey, mainKey,
	makeStablePtrIdKey, mapIdKey, minusClassOpKey, monadClassKey,
	monadPlusClassKey, mutableArrayPrimTyConKey,
	mutableByteArrayPrimTyConKey, mutableByteArrayTyConKey,
	mutVarPrimTyConKey, nilDataConKey, noMethodBindingErrorIdKey,
	nonExhaustiveGuardsErrorIdKey, numClassKey, anyBoxConKey, ordClassKey,
	orderingTyConKey, otherwiseIdKey, parErrorIdKey, parIdKey,
	patErrorIdKey, plusIntegerIdKey, ratioDataConKey, ratioTyConKey,
	rationalTyConKey, readClassKey, realClassKey, realFloatClassKey,
	realFracClassKey, realWorldPrimIdKey, realWorldTyConKey,
	recConErrorIdKey, recSelErrIdKey, recUpdErrorIdKey, returnIOIdKey,
	returnMClassOpKey, runSTRepIdKey, showClassKey, ioTyConKey,
	ioDataConKey, stablePtrDataConKey, stablePtrPrimTyConKey,
	stablePtrTyConKey, stableNameDataConKey, stableNamePrimTyConKey,
	stableNameTyConKey, statePrimTyConKey, timesIntegerIdKey, typeConKey,
	kindConKey, boxityConKey, mVarPrimTyConKey, thenMClassOpKey,
	threadIdPrimTyConKey, toEnumClassOpKey, traceIdKey, trueDataConKey,
	unboundKey, unboxedConKey, unpackCStringUtf8IdKey,
	unpackCStringAppendIdKey, unpackCStringFoldrIdKey, unpackCStringIdKey,
	unsafeCoerceIdKey, ushowListIdKey, weakPrimTyConKey, wordDataConKey,
	wordPrimTyConKey, wordTyConKey, word8TyConKey, word16TyConKey,
	word32TyConKey, word64PrimTyConKey, word64TyConKey, zipIdKey

    ) where

#include "HsVersions.h"

import Module	  ( ModuleName, mkPrelModule, mkSrcModule )
import OccName	  ( NameSpace, varName, dataName, tcName, clsName )
import RdrName	  ( RdrName, mkPreludeQual )
import UniqFM
import Unique	  ( Unique, Uniquable(..), hasKey,
		    mkPreludeMiscIdUnique, mkPreludeDataConUnique,
		    mkPreludeTyConUnique, mkPreludeClassUnique
		  ) 
import BasicTypes ( Boxity(..), Arity )
import UniqFM	  ( UniqFM, listToUFM )
import Util	  ( nOfThem )
import Panic	  ( panic )
\end{code}


%************************************************************************
%*									*
\subsection{Known key RdrNames}
%*									*
%************************************************************************

This section tells what the compiler knows about the
assocation of names with uniques

\begin{code}
knownKeyRdrNames :: [(RdrName, Unique)]
knownKeyRdrNames
 =  [
	-- Type constructors (synonyms especially)
      (ioTyCon_RDR,		ioTyConKey)
    , (main_RDR,		mainKey)
    , (orderingTyCon_RDR,  	orderingTyConKey)
    , (rationalTyCon_RDR,  	rationalTyConKey)
    , (ratioDataCon_RDR,   	ratioDataConKey)
    , (ratioTyCon_RDR,     	ratioTyConKey)
    , (byteArrayTyCon_RDR, 	byteArrayTyConKey)
    , (mutableByteArrayTyCon_RDR, mutableByteArrayTyConKey)
    , (foreignObjTyCon_RDR, 	foreignObjTyConKey)
    , (bcoPrimTyCon_RDR, 	bcoPrimTyConKey)
    , (stablePtrTyCon_RDR, 	stablePtrTyConKey)
    , (stablePtrDataCon_RDR,    stablePtrDataConKey)

	--  Classes.  *Must* include:
	--  	classes that are grabbed by key (e.g., eqClassKey)
	--  	classes in "Class.standardClassKeys" (quite a few)
    , (eqClass_RDR,		eqClassKey)		-- mentioned, derivable
    , (ordClass_RDR,		ordClassKey)		-- derivable
    , (boundedClass_RDR, 	boundedClassKey)	-- derivable
    , (numClass_RDR, 		numClassKey)		-- mentioned, numeric
    , (enumClass_RDR,		enumClassKey)		-- derivable
    , (monadClass_RDR,		monadClassKey)
    , (monadPlusClass_RDR,	monadPlusClassKey)
    , (functorClass_RDR,	functorClassKey)
    , (showClass_RDR, 		showClassKey)		-- derivable
    , (realClass_RDR, 		realClassKey)		-- numeric
    , (integralClass_RDR,	integralClassKey)	-- numeric
    , (fractionalClass_RDR,	fractionalClassKey)	-- numeric
    , (floatingClass_RDR,	floatingClassKey)	-- numeric
    , (realFracClass_RDR,	realFracClassKey)	-- numeric
    , (realFloatClass_RDR,	realFloatClassKey)	-- numeric
    , (readClass_RDR,		readClassKey)		-- derivable
    , (ixClass_RDR,		ixClassKey)		-- derivable (but it isn't Prelude.Ix; hmmm)
    , (ccallableClass_RDR, 	cCallableClassKey)	-- mentioned, ccallish
    , (creturnableClass_RDR, 	cReturnableClassKey)	-- mentioned, ccallish

	-- ClassOps 
    , (fromInt_RDR,		fromIntClassOpKey)
    , (fromInteger_RDR,		fromIntegerClassOpKey)
    , (ge_RDR,			geClassOpKey) 
    , (minus_RDR,		minusClassOpKey)
    , (enumFrom_RDR,		enumFromClassOpKey)
    , (enumFromThen_RDR,	enumFromThenClassOpKey)
    , (enumFromTo_RDR,		enumFromToClassOpKey)
    , (enumFromThenTo_RDR,	enumFromThenToClassOpKey)
    , (fromEnum_RDR,		fromEnumClassOpKey)
    , (toEnum_RDR,		toEnumClassOpKey)
    , (eq_RDR,			eqClassOpKey)
    , (thenM_RDR,		thenMClassOpKey)
    , (returnM_RDR,		returnMClassOpKey)
    , (failM_RDR,		failMClassOpKey)
    , (fromRational_RDR,	fromRationalClassOpKey)
    
    , (deRefStablePtr_RDR,	deRefStablePtrIdKey)
    , (makeStablePtr_RDR,	makeStablePtrIdKey)
    , (bindIO_RDR,		bindIOIdKey)
    , (returnIO_RDR,		returnIOIdKey)

	-- Strings and lists
    , (map_RDR,			mapIdKey)
    , (append_RDR,		appendIdKey)
    , (unpackCString_RDR, 	unpackCStringIdKey)
    , (unpackCStringAppend_RDR,	unpackCStringAppendIdKey)
    , (unpackCStringFoldr_RDR,	unpackCStringFoldrIdKey)
    , (unpackCStringUtf8_RDR,  	unpackCStringUtf8IdKey)

	-- List operations
    , (concat_RDR,		concatIdKey)
    , (filter_RDR,		filterIdKey)
    , (zip_RDR,			zipIdKey)
    , (foldr_RDR,		foldrIdKey)
    , (build_RDR,		buildIdKey)
    , (augment_RDR,		augmentIdKey)

	-- FFI primitive types that are not wired-in.
    , (int8TyCon_RDR,           int8TyConKey)
    , (int16TyCon_RDR,          int16TyConKey)
    , (int32TyCon_RDR,          int32TyConKey)
    , (int64TyCon_RDR,          int64TyConKey)
    , (word8TyCon_RDR,          word8TyConKey)
    , (word16TyCon_RDR,         word16TyConKey)
    , (word32TyCon_RDR,         word32TyConKey)
    , (word64TyCon_RDR,         word64TyConKey)

	-- Others
    , (otherwiseId_RDR,		otherwiseIdKey)
    , (plusInteger_RDR,		plusIntegerIdKey)
    , (timesInteger_RDR,	timesIntegerIdKey)
    , (eqString_RDR,		eqStringIdKey)
    , (assert_RDR,		assertIdKey)
    , (runSTRep_RDR,		runSTRepIdKey)
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Module names}
%*									*
%************************************************************************

\begin{code}
pRELUDE_Name      = mkSrcModule "Prelude"
pREL_GHC_Name     = mkSrcModule "PrelGHC"	   -- Primitive types and values
pREL_BASE_Name    = mkSrcModule "PrelBase"
pREL_ENUM_Name    = mkSrcModule "PrelEnum"
pREL_SHOW_Name    = mkSrcModule "PrelShow"
pREL_READ_Name    = mkSrcModule "PrelRead"
pREL_NUM_Name     = mkSrcModule "PrelNum"
pREL_LIST_Name    = mkSrcModule "PrelList"
pREL_TUP_Name     = mkSrcModule "PrelTup"
pREL_PACK_Name    = mkSrcModule "PrelPack"
pREL_CONC_Name    = mkSrcModule "PrelConc"
pREL_IO_BASE_Name = mkSrcModule "PrelIOBase"
pREL_ST_Name	  = mkSrcModule "PrelST"
pREL_ARR_Name     = mkSrcModule "PrelArr"
pREL_BYTEARR_Name = mkSrcModule "PrelByteArr"
pREL_FOREIGN_Name = mkSrcModule "PrelForeign"
pREL_STABLE_Name  = mkSrcModule "PrelStable"
pREL_ADDR_Name    = mkSrcModule "PrelAddr"
pREL_ERR_Name     = mkSrcModule "PrelErr"
pREL_REAL_Name    = mkSrcModule "PrelReal"
pREL_FLOAT_Name   = mkSrcModule "PrelFloat"

pREL_MAIN_Name   = mkSrcModule "PrelMain"
mAIN_Name	 = mkSrcModule "Main"
iNT_Name	 = mkSrcModule "Int"
wORD_Name	 = mkSrcModule "Word"

pREL_GHC     = mkPrelModule pREL_GHC_Name
pREL_BASE    = mkPrelModule pREL_BASE_Name
pREL_ADDR    = mkPrelModule pREL_ADDR_Name
pREL_STABLE  = mkPrelModule pREL_STABLE_Name
pREL_IO_BASE = mkPrelModule pREL_IO_BASE_Name
pREL_PACK    = mkPrelModule pREL_PACK_Name
pREL_ERR     = mkPrelModule pREL_ERR_Name
pREL_NUM     = mkPrelModule pREL_NUM_Name
pREL_REAL    = mkPrelModule pREL_REAL_Name
pREL_FLOAT   = mkPrelModule pREL_FLOAT_Name
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupNameStr :: Boxity -> Int -> (ModuleName, FAST_STRING)

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
					  (mod, occ) -> mkPreludeQual space mod occ
\end{code}



%************************************************************************
%*									*
\subsection{Commonly-used RdrNames}
%*									*
%************************************************************************

These RdrNames are not really "built in", but some parts of the compiler
(notably the deriving mechanism) need to mention their names, and it's convenient
to write them all down in one place.

\begin{code}
main_RDR		= varQual mAIN_Name      SLIT("main")

ioTyCon_RDR		= tcQual   pREL_IO_BASE_Name SLIT("IO")
ioDataCon_RDR  	   	= dataQual pREL_IO_BASE_Name SLIT("IO")
bindIO_RDR	        = varQual  pREL_IO_BASE_Name SLIT("bindIO")
returnIO_RDR	        = varQual  pREL_IO_BASE_Name SLIT("returnIO")


rationalTyCon_RDR	= tcQual   pREL_REAL_Name  SLIT("Rational")
ratioTyCon_RDR		= tcQual   pREL_REAL_Name  SLIT("Ratio")
ratioDataCon_RDR	= dataQual pREL_REAL_Name  SLIT(":%")

byteArrayTyCon_RDR		= tcQual pREL_BYTEARR_Name  SLIT("ByteArray")
mutableByteArrayTyCon_RDR	= tcQual pREL_BYTEARR_Name  SLIT("MutableByteArray")

foreignObjTyCon_RDR	= tcQual   pREL_IO_BASE_Name SLIT("ForeignObj")
bcoPrimTyCon_RDR	= tcQual   pREL_BASE_Name SLIT("BCO#")
stablePtrTyCon_RDR	= tcQual   pREL_STABLE_Name SLIT("StablePtr")
stablePtrDataCon_RDR	= dataQual pREL_STABLE_Name SLIT("StablePtr")
deRefStablePtr_RDR      = varQual  pREL_STABLE_Name SLIT("deRefStablePtr")
makeStablePtr_RDR       = varQual  pREL_STABLE_Name SLIT("makeStablePtr")

-- Random PrelBase data types and constructors
intTyCon_RDR	   = tcQual   pREL_BASE_Name SLIT("Int")
orderingTyCon_RDR  = tcQual   pREL_BASE_Name SLIT("Ordering")
mkInt_RDR	   = dataQual pREL_BASE_Name SLIT("I#")
false_RDR	   = dataQual pREL_BASE_Name SLIT("False")
true_RDR	   = dataQual pREL_BASE_Name SLIT("True")

-- Random PrelBase functions
otherwiseId_RDR    = varQual pREL_BASE_Name SLIT("otherwise")
and_RDR		   = varQual pREL_BASE_Name SLIT("&&")
not_RDR		   = varQual pREL_BASE_Name SLIT("not")
compose_RDR	   = varQual pREL_BASE_Name SLIT(".")
append_RDR	   = varQual pREL_BASE_Name SLIT("++")
foldr_RDR	   = varQual pREL_BASE_Name SLIT("foldr")
map_RDR		   = varQual pREL_BASE_Name SLIT("map")
build_RDR	   = varQual pREL_BASE_Name SLIT("build")
augment_RDR	   = varQual pREL_BASE_Name SLIT("augment")
eqString_RDR	   = varQual pREL_BASE_Name SLIT("eqString")

-- Strings
unpackCString_RDR       = varQual pREL_BASE_Name SLIT("unpackCString#")
unpackCStringAppend_RDR = varQual pREL_BASE_Name SLIT("unpackAppendCString#")
unpackCStringFoldr_RDR  = varQual pREL_BASE_Name SLIT("unpackFoldrCString#")
unpackCStringUtf8_RDR   = varQual pREL_BASE_Name SLIT("unpackCStringUtf8#")

-- Classes Eq and Ord
eqClass_RDR		= clsQual pREL_BASE_Name SLIT("Eq")
ordClass_RDR		= clsQual pREL_BASE_Name SLIT("Ord")
eq_RDR		   = varQual pREL_BASE_Name SLIT("==")
ne_RDR		   = varQual pREL_BASE_Name SLIT("/=")
le_RDR		   = varQual pREL_BASE_Name SLIT("<=")
lt_RDR		   = varQual pREL_BASE_Name SLIT("<")
ge_RDR		   = varQual pREL_BASE_Name SLIT(">=")
gt_RDR		   = varQual pREL_BASE_Name SLIT(">")
ltTag_RDR	   = dataQual pREL_BASE_Name SLIT("LT")
eqTag_RDR	   = dataQual pREL_BASE_Name SLIT("EQ")
gtTag_RDR	   = dataQual pREL_BASE_Name SLIT("GT")
max_RDR		   = varQual pREL_BASE_Name SLIT("max")
min_RDR		   = varQual pREL_BASE_Name SLIT("min")
compare_RDR	   = varQual pREL_BASE_Name SLIT("compare")

-- Class Monad
monadClass_RDR	   = clsQual pREL_BASE_Name SLIT("Monad")
monadPlusClass_RDR = clsQual pREL_BASE_Name SLIT("MonadPlus")
thenM_RDR	   = varQual pREL_BASE_Name SLIT(">>=")
returnM_RDR	   = varQual pREL_BASE_Name SLIT("return")
failM_RDR	   = varQual pREL_BASE_Name SLIT("fail")

-- Class Functor
functorClass_RDR	= clsQual pREL_BASE_Name SLIT("Functor")

-- Class Show
showClass_RDR	   = clsQual pREL_SHOW_Name SLIT("Show")
showList___RDR     = varQual pREL_SHOW_Name SLIT("showList__")
showsPrec_RDR	   = varQual pREL_SHOW_Name SLIT("showsPrec")
showList_RDR	   = varQual pREL_SHOW_Name SLIT("showList")
showSpace_RDR	   = varQual pREL_SHOW_Name SLIT("showSpace")
showString_RDR	   = varQual pREL_SHOW_Name SLIT("showString")
showParen_RDR	   = varQual pREL_SHOW_Name SLIT("showParen")


-- Class Read
readClass_RDR	   = clsQual pREL_READ_Name SLIT("Read")
readsPrec_RDR	   = varQual pREL_READ_Name SLIT("readsPrec")
readList_RDR	   = varQual pREL_READ_Name SLIT("readList")
readParen_RDR	   = varQual pREL_READ_Name SLIT("readParen")
lex_RDR		   = varQual pREL_READ_Name SLIT("lex")
readList___RDR     = varQual pREL_READ_Name SLIT("readList__")


-- Class Num
numClass_RDR	   = clsQual pREL_NUM_Name SLIT("Num")
fromInt_RDR	   = varQual pREL_NUM_Name SLIT("fromInt")
fromInteger_RDR	   = varQual pREL_NUM_Name SLIT("fromInteger")
minus_RDR	   = varQual pREL_NUM_Name SLIT("-")
negate_RDR	   = varQual pREL_NUM_Name SLIT("negate")
plus_RDR	   = varQual pREL_NUM_Name SLIT("+")
times_RDR	   = varQual pREL_NUM_Name SLIT("*")
plusInteger_RDR	   = varQual pREL_NUM_Name SLIT("plusInteger")
timesInteger_RDR   = varQual pREL_NUM_Name SLIT("timesInteger")

-- Other numberic classes
realClass_RDR		= clsQual pREL_REAL_Name  SLIT("Real")
integralClass_RDR	= clsQual pREL_REAL_Name  SLIT("Integral")
realFracClass_RDR	= clsQual pREL_REAL_Name  SLIT("RealFrac")
fractionalClass_RDR	= clsQual pREL_REAL_Name  SLIT("Fractional")
fromRational_RDR   	= varQual pREL_REAL_Name  SLIT("fromRational")

floatingClass_RDR	= clsQual pREL_FLOAT_Name  SLIT("Floating")
realFloatClass_RDR	= clsQual pREL_FLOAT_Name  SLIT("RealFloat")

-- Class Ix
ixClass_RDR	   = clsQual pREL_ARR_Name SLIT("Ix")
range_RDR	   = varQual pREL_ARR_Name SLIT("range")
index_RDR	   = varQual pREL_ARR_Name SLIT("index")
inRange_RDR	   = varQual pREL_ARR_Name SLIT("inRange")

-- Class CCallable and CReturnable
ccallableClass_RDR	= clsQual pREL_GHC_Name  SLIT("CCallable")
creturnableClass_RDR	= clsQual pREL_GHC_Name  SLIT("CReturnable")

-- Class Enum
enumClass_RDR 	   = clsQual pREL_ENUM_Name SLIT("Enum")
succ_RDR	   = varQual pREL_ENUM_Name SLIT("succ")
pred_RDR	   = varQual pREL_ENUM_Name SLIT("pred")
toEnum_RDR	   = varQual pREL_ENUM_Name SLIT("toEnum")
fromEnum_RDR	   = varQual pREL_ENUM_Name SLIT("fromEnum")
enumFrom_RDR	   = varQual pREL_ENUM_Name SLIT("enumFrom")
enumFromTo_RDR	   = varQual pREL_ENUM_Name SLIT("enumFromTo")
enumFromThen_RDR   = varQual pREL_ENUM_Name SLIT("enumFromThen")
enumFromThenTo_RDR = varQual pREL_ENUM_Name SLIT("enumFromThenTo")

-- Class Bounded
boundedClass_RDR   = clsQual pREL_ENUM_Name SLIT("Bounded")
minBound_RDR	   = varQual pREL_ENUM_Name SLIT("minBound")
maxBound_RDR	   = varQual pREL_ENUM_Name SLIT("maxBound")


-- List functions
concat_RDR	   = varQual pREL_LIST_Name SLIT("concat")
filter_RDR	   = varQual pREL_LIST_Name SLIT("filter")
zip_RDR		   = varQual pREL_LIST_Name SLIT("zip")

int8TyCon_RDR    = tcQual iNT_Name       SLIT("Int8")
int16TyCon_RDR   = tcQual iNT_Name       SLIT("Int16")
int32TyCon_RDR   = tcQual iNT_Name       SLIT("Int32")
int64TyCon_RDR   = tcQual pREL_ADDR_Name SLIT("Int64")

word8TyCon_RDR    = tcQual wORD_Name      SLIT("Word8")
word16TyCon_RDR   = tcQual wORD_Name      SLIT("Word16")
word32TyCon_RDR   = tcQual wORD_Name      SLIT("Word32")
word64TyCon_RDR   = tcQual pREL_ADDR_Name SLIT("Word64")

error_RDR	   = varQual pREL_ERR_Name SLIT("error")
assert_RDR         = varQual pREL_GHC_Name SLIT("assert")
getTag_RDR	   = varQual pREL_GHC_Name SLIT("getTag#")
assertErr_RDR      = varQual pREL_ERR_Name SLIT("assertError")
runSTRep_RDR	   = varQual pREL_ST_Name  SLIT("runSTRep")
\end{code}


%************************************************************************
%*									*
\subsection{Local helpers}
%*									*
%************************************************************************

\begin{code}
varQual  = mkPreludeQual varName
dataQual = mkPreludeQual dataName
tcQual   = mkPreludeQual tcName
clsQual  = mkPreludeQual clsName
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
int32TyConKey				= mkPreludeTyConUnique 18
int64PrimTyConKey			= mkPreludeTyConUnique 19
int64TyConKey				= mkPreludeTyConUnique 20
integerTyConKey				= mkPreludeTyConUnique 21
listTyConKey				= mkPreludeTyConUnique 22
foreignObjPrimTyConKey			= mkPreludeTyConUnique 23
foreignObjTyConKey			= mkPreludeTyConUnique 24
weakPrimTyConKey			= mkPreludeTyConUnique 25
mutableArrayPrimTyConKey		= mkPreludeTyConUnique 26
mutableByteArrayPrimTyConKey		= mkPreludeTyConUnique 27
orderingTyConKey			= mkPreludeTyConUnique 28
mVarPrimTyConKey		    	= mkPreludeTyConUnique 29
ratioTyConKey				= mkPreludeTyConUnique 30
rationalTyConKey			= mkPreludeTyConUnique 31
realWorldTyConKey			= mkPreludeTyConUnique 32
stablePtrPrimTyConKey			= mkPreludeTyConUnique 33
stablePtrTyConKey			= mkPreludeTyConUnique 34
statePrimTyConKey			= mkPreludeTyConUnique 35
stableNamePrimTyConKey			= mkPreludeTyConUnique 50
stableNameTyConKey		        = mkPreludeTyConUnique 51
mutableByteArrayTyConKey		= mkPreludeTyConUnique 52
mutVarPrimTyConKey			= mkPreludeTyConUnique 53
ioTyConKey				= mkPreludeTyConUnique 55
byteArrayTyConKey			= mkPreludeTyConUnique 56
wordPrimTyConKey			= mkPreludeTyConUnique 57
wordTyConKey				= mkPreludeTyConUnique 58
word8TyConKey				= mkPreludeTyConUnique 59
word16TyConKey				= mkPreludeTyConUnique 60
word32TyConKey				= mkPreludeTyConUnique 61
word64PrimTyConKey			= mkPreludeTyConUnique 62
word64TyConKey				= mkPreludeTyConUnique 63
boxedConKey				= mkPreludeTyConUnique 64
unboxedConKey				= mkPreludeTyConUnique 65
anyBoxConKey				= mkPreludeTyConUnique 66
kindConKey				= mkPreludeTyConUnique 67
boxityConKey				= mkPreludeTyConUnique 68
typeConKey				= mkPreludeTyConUnique 69
threadIdPrimTyConKey			= mkPreludeTyConUnique 70
bcoPrimTyConKey				= mkPreludeTyConUnique 71
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
nilDataConKey				= mkPreludeDataConUnique 10
ratioDataConKey				= mkPreludeDataConUnique 11
stablePtrDataConKey			= mkPreludeDataConUnique 12
stableNameDataConKey			= mkPreludeDataConUnique 13
trueDataConKey				= mkPreludeDataConUnique 14
wordDataConKey				= mkPreludeDataConUnique 15
ioDataConKey				= mkPreludeDataConUnique 16
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
irrefutPatErrorIdKey	      = mkPreludeMiscIdUnique 15
eqStringIdKey		      = mkPreludeMiscIdUnique 16
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 17
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 18
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
makeStablePtrIdKey	      = mkPreludeMiscIdUnique 39
getTagIdKey		      = mkPreludeMiscIdUnique 40
plusIntegerIdKey	      = mkPreludeMiscIdUnique 41
timesIntegerIdKey	      = mkPreludeMiscIdUnique 42
\end{code}

Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.

\begin{code}					  
fromIntClassOpKey	      = mkPreludeMiscIdUnique 101
fromIntegerClassOpKey	      = mkPreludeMiscIdUnique 102
minusClassOpKey		      = mkPreludeMiscIdUnique 103
fromRationalClassOpKey	      = mkPreludeMiscIdUnique 104
enumFromClassOpKey	      = mkPreludeMiscIdUnique 105
enumFromThenClassOpKey	      = mkPreludeMiscIdUnique 106
enumFromToClassOpKey	      = mkPreludeMiscIdUnique 107
enumFromThenToClassOpKey      = mkPreludeMiscIdUnique 108
eqClassOpKey		      = mkPreludeMiscIdUnique 109
geClassOpKey		      = mkPreludeMiscIdUnique 110
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
\subsection[Class-std-groups]{Standard groups of Prelude classes}
%*									*
%************************************************************************

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

@derivingOccurrences@ maps a class name to a list of the (qualified) occurrences
that will be mentioned by  the derived code for the class when it is later generated.
We don't need to put in things that are WiredIn (because they are already mapped to their
correct name by the @NameSupply@.  The class itself, and all its class ops, is
already flagged as an occurrence so we don't need to mention that either.

@derivingOccurrences@ has an item for every derivable class, even if that item is empty,
because we treat lookup failure as indicating that the class is illegal in a deriving clause.

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
numericTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, intTyConKey
	, integerTyConKey
	, doubleTyConKey
	, floatTyConKey
	]

needsDataDeclCtxtClassKeys = -- see comments in TcDeriv
  	[ readClassKey
    	]

cCallishClassKeys = 
	[ cCallableClassKey
	, cReturnableClassKey
	]

	-- Renamer always imports these data decls replete with constructors
	-- so that desugarer can always see their constructors.  Ugh!
cCallishTyKeys = 
	[ addrTyConKey
	, wordTyConKey
	, byteArrayTyConKey
	, mutableByteArrayTyConKey
	, foreignObjTyConKey
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

