%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
module PrelInfo (
	builtinNames, 	-- Names of things whose *unique* must be known, but 
			-- that is all. If something is in here, you know that
			-- if it's used at all then it's Name will be just as
			-- it is here, unique and all.  Includes all the 
			-- wired-in names.

	thinAirIdNames,	-- Names of non-wired-in Ids that may be used out of
	setThinAirIds,	-- thin air in any compilation. If they are not wired in
	thinAirModules,	-- we must be sure to import them from some Prelude 
			-- interface file even if they are not overtly 
			-- mentioned.  Subset of builtinNames.
	noRepIntegerIds,
	noRepStrIds,

	derivingOccurrences, 	-- For a given class C, this tells what other 
				-- things are needed as a result of a 
				-- deriving(C) clause


	-- Here are the thin-air Ids themselves
	addr2IntegerId,
	packStringForCId, unpackCStringId, unpackCString2Id,
	unpackCStringAppendId, unpackCStringFoldrId,
	foldrId,

	-- Random other things
	main_NAME, ioTyCon_NAME,
	deRefStablePtr_NAME, makeStablePtr_NAME,
	bindIO_NAME, 

	maybeCharLikeCon, maybeIntLikeCon,
	needsDataDeclCtxtClassKeys, cCallishClassKeys, cCallishTyKeys, 
	isNoDictClass, isNumericClass, isStandardClass, isCcallishClass, 
	isCreturnableClass, numericTyKeys,

	-- RdrNames for lots of things, mainly used in derivings
	eq_RDR, ne_RDR, le_RDR, lt_RDR, ge_RDR, gt_RDR, max_RDR, min_RDR, 
	compare_RDR, minBound_RDR, maxBound_RDR, enumFrom_RDR, enumFromTo_RDR,
	enumFromThen_RDR, enumFromThenTo_RDR, succ_RDR, pred_RDR, fromEnum_RDR, toEnum_RDR, 
	ratioDataCon_RDR, range_RDR, index_RDR, inRange_RDR, readsPrec_RDR,
	readList_RDR, showsPrec_RDR, showList_RDR, plus_RDR, times_RDR,
	ltTag_RDR, eqTag_RDR, gtTag_RDR, eqH_Char_RDR, ltH_Char_RDR, 
	eqH_Word_RDR, ltH_Word_RDR, eqH_Addr_RDR, ltH_Addr_RDR, eqH_Float_RDR,
	ltH_Float_RDR, eqH_Double_RDR, ltH_Double_RDR, eqH_Int_RDR, 
	ltH_Int_RDR, geH_RDR, leH_RDR, minusH_RDR, false_RDR, true_RDR,
	and_RDR, not_RDR, append_RDR, map_RDR, compose_RDR, mkInt_RDR,
	error_RDR, assertErr_RDR, getTag_RDR, tagToEnumH_RDR,
	showString_RDR, showParen_RDR, readParen_RDR, lex_RDR,
	showSpace_RDR, showList___RDR, readList___RDR, negate_RDR,

	numClass_RDR, fractionalClass_RDR, eqClass_RDR, 
	ccallableClass_RDR, creturnableClass_RDR,
	monadClass_RDR, enumClass_RDR, ordClass_RDR,
	ioDataCon_RDR,

	mkTupConRdrName, mkUbxTupConRdrName

    ) where

#include "HsVersions.h"


-- friends:
import PrelMods		-- Prelude module names
import PrelVals		-- VALUES
import MkId		( mkPrimitiveId )
import PrimOp		( PrimOp(..), allThePrimOps )
import DataCon		( DataCon )
import PrimRep		( PrimRep(..) )
import TysPrim		-- TYPES
import TysWiredIn

-- others:
import RdrName		( RdrName, mkPreludeQual )
import Var		( varUnique, Id )
import Name		( Name, OccName, Provenance(..), 
			  NameSpace, tcName, clsName, varName, dataName,
			  getName, mkGlobalName, nameRdrName, systemProvenance
			)
import RdrName		( rdrNameModule, rdrNameOcc, mkSrcQual )
import Class		( Class, classKey )
import TyCon		( tyConDataCons, TyCon )
import Type		( funTyCon )
import Bag
import Unique		-- *Key stuff
import UniqFM		( UniqFM, listToUFM, lookupWithDefaultUFM ) 
import Util		( isIn )
import Panic		( panic )

import IOExts
\end{code}

%************************************************************************
%*									*
\subsection[builtinNameInfo]{Lookup built-in names}
%*									*
%************************************************************************

We have two ``builtin name funs,'' one to look up @TyCons@ and
@Classes@, the other to look up values.

\begin{code}
builtinNames :: Bag Name
builtinNames
  = unionManyBags
	[	-- Wired in TyCons
	  unionManyBags (map getTyConNames wired_in_tycons)

		-- Wired in Ids
	, listToBag (map getName wired_in_ids)

		-- PrimOps
	, listToBag (map (getName . mkPrimitiveId) allThePrimOps)

 		-- Thin-air ids
	, listToBag thinAirIdNames

		-- Other names with magic keys
	, listToBag knownKeyNames
	]
\end{code}


\begin{code}
getTyConNames :: TyCon -> Bag Name
getTyConNames tycon
    = getName tycon `consBag` 
      listToBag (map getName (tyConDataCons tycon))
	-- Synonyms return empty list of constructors
\end{code}

We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.


%************************************************************************
%*									*
\subsection{Wired in TyCons}
%*									*
%************************************************************************

\begin{code}
wired_in_tycons = [funTyCon] ++
		  prim_tycons ++
		  tuple_tycons ++
		  unboxed_tuple_tycons ++
		  data_tycons

prim_tycons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int64PrimTyCon
    , foreignObjPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , statePrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word64PrimTyCon
    ]

tuple_tycons = unitTyCon : [tupleTyCon i | i <- [2..37] ]
unboxed_tuple_tycons = [unboxedTupleTyCon i | i <- [1..37] ]

data_tycons
  = [ addrTyCon
    , boolTyCon
    , charTyCon
    , doubleTyCon
    , floatTyCon
    , intTyCon
    , integerTyCon
    , listTyCon
    , wordTyCon
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Wired in Ids}
%*									*
%************************************************************************

\begin{code}
wired_in_ids
  = [ 	-- These error-y things are wired in because we don't yet have
	-- a way to express in an interface file that the result type variable
	-- is 'open'; that is can be unified with an unboxed type
	-- 
	-- [The interface file format now carry such information, but there's
	--  no way yet of expressing at the definition site for these error-reporting
	--  functions that they have an 'open' result type. -- sof 1/99]
	-- 
      aBSENT_ERROR_ID
    , eRROR_ID
    , iRREFUT_PAT_ERROR_ID
    , nON_EXHAUSTIVE_GUARDS_ERROR_ID
    , nO_METHOD_BINDING_ERROR_ID
    , pAR_ERROR_ID
    , pAT_ERROR_ID
    , rEC_CON_ERROR_ID
    , rEC_UPD_ERROR_ID

	-- These three can't be defined in Haskell
    , realWorldPrimId
    , unsafeCoerceId
    , getTagId
    ]

\end{code}

%************************************************************************
%*									*
\subsection{Thin air entities}
%*									*
%************************************************************************

These are Ids that we need to reference in various parts of the
system, and we'd like to pull them out of thin air rather than pass
them around.  We'd also like to have all the IdInfo available for each
one: i.e. everything that gets pulled out of the interface file.

The solution is to generate this map of global Ids after the
typechecker, and assign it to a global variable.  Any subsequent
pass may refer to the map to pull Ids out.  Any invalid
(i.e. pre-typechecker) access to the map will result in a panic.

\begin{code}
thinAirIdNames 
  = map mkKnownKeyGlobal
    [
	-- Needed for converting literals to Integers (used in tidyCoreExpr)
      (varQual pREL_BASE SLIT("addr2Integer"), addr2IntegerIdKey)

	-- String literals
    , (varQual pREL_PACK SLIT("packCString#"),   packCStringIdKey)
    , (varQual pREL_PACK SLIT("unpackCString#"), unpackCStringIdKey)
    , (varQual pREL_PACK SLIT("unpackNBytes#"),  unpackCString2IdKey)
    , (varQual pREL_PACK SLIT("unpackAppendCString#"), unpackCStringAppendIdKey)
    , (varQual pREL_PACK SLIT("unpackFoldrCString#"),  unpackCStringFoldrIdKey)

	-- Folds; introduced by desugaring list comprehensions
    , (varQual pREL_BASE SLIT("foldr"), foldrIdKey)
    ]

thinAirModules = [pREL_PACK]	-- See notes with RnIfaces.findAndReadIface

noRepIntegerIds = [addr2IntegerId]

noRepStrIds = [unpackCString2Id, unpackCStringId]

addr2IntegerId = lookupThinAirId addr2IntegerIdKey

packStringForCId = lookupThinAirId packCStringIdKey
unpackCStringId  = lookupThinAirId unpackCStringIdKey
unpackCString2Id = lookupThinAirId unpackCString2IdKey 
unpackCStringAppendId = lookupThinAirId unpackCStringAppendIdKey 
unpackCStringFoldrId  = lookupThinAirId unpackCStringFoldrIdKey 

foldrId = lookupThinAirId foldrIdKey
\end{code}


\begin{code}
\end{code}

\begin{code}
thinAirIdMapRef :: IORef (UniqFM Id)
thinAirIdMapRef = unsafePerformIO (newIORef (panic "thinAirIdMap: still empty"))

setThinAirIds :: [Id] -> IO ()
setThinAirIds thin_air_ids
  = writeIORef thinAirIdMapRef the_map
  where
    the_map = listToUFM [(varUnique id, id) | id <- thin_air_ids]

thinAirIdMap :: UniqFM Id
thinAirIdMap = unsafePerformIO (readIORef thinAirIdMapRef)
  -- Read it just once, the first time someone tugs on thinAirIdMap

lookupThinAirId :: Unique -> Id
lookupThinAirId uniq = lookupWithDefaultUFM thinAirIdMap
			(panic "lookupThinAirId: no mapping") uniq 
\end{code}


%************************************************************************
%*									*
\subsection{Built-in keys}
%*									*
%************************************************************************

Ids, Synonyms, Classes and ClassOps with builtin keys. 

\begin{code}
mkKnownKeyGlobal :: (RdrName, Unique) -> Name
mkKnownKeyGlobal (rdr_name, uniq)
  = mkGlobalName uniq (rdrNameModule rdr_name) (rdrNameOcc rdr_name)
		 systemProvenance

ioTyCon_NAME	  = mkKnownKeyGlobal (ioTyCon_RDR,       ioTyConKey)
main_NAME	  = mkKnownKeyGlobal (main_RDR,	         mainKey)

 -- Operations needed when compiling FFI decls
bindIO_NAME	    = mkKnownKeyGlobal (bindIO_RDR,	    bindIOIdKey)
deRefStablePtr_NAME = mkKnownKeyGlobal (deRefStablePtr_RDR, deRefStablePtrIdKey)
makeStablePtr_NAME  = mkKnownKeyGlobal (makeStablePtr_RDR,  makeStablePtrIdKey)

knownKeyNames :: [Name]
knownKeyNames
  = [main_NAME, ioTyCon_NAME]
    ++
    map mkKnownKeyGlobal
    [
	-- Type constructors (synonyms especially)
      (orderingTyCon_RDR,  	orderingTyConKey)
    , (rationalTyCon_RDR,  	rationalTyConKey)
    , (ratioDataCon_RDR,   	ratioDataConKey)
    , (ratioTyCon_RDR,     	ratioTyConKey)
    , (byteArrayTyCon_RDR, 	byteArrayTyConKey)
    , (mutableByteArrayTyCon_RDR, mutableByteArrayTyConKey)
    , (foreignObjTyCon_RDR, 	foreignObjTyConKey)
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

    , (map_RDR,			mapIdKey)
    , (append_RDR,		appendIdKey)

	-- List operations
    , (concat_RDR,		concatIdKey)
    , (filter_RDR,		filterIdKey)
    , (zip_RDR,			zipIdKey)

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
    , (assert_RDR,		assertIdKey)
    ]
\end{code}

ToDo: make it do the ``like'' part properly (as in 0.26 and before).

\begin{code}
maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = getUnique con == charDataConKey
maybeIntLikeCon  con = getUnique con == intDataConKey
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
prelude_primop op = nameRdrName (getName (mkPrimitiveId op))

main_RDR		= varQual mAIN      SLIT("main")
otherwiseId_RDR 	= varQual pREL_BASE SLIT("otherwise")

intTyCon_RDR		= nameRdrName (getName intTyCon)
ioTyCon_RDR		= tcQual   pREL_IO_BASE SLIT("IO")
ioDataCon_RDR  	   	= dataQual pREL_IO_BASE SLIT("IO")
bindIO_RDR	        = varQual  pREL_IO_BASE SLIT("bindIO")

orderingTyCon_RDR	= tcQual   pREL_BASE SLIT("Ordering")
rationalTyCon_RDR	= tcQual   pREL_NUM  SLIT("Rational")
ratioTyCon_RDR		= tcQual   pREL_NUM  SLIT("Ratio")
ratioDataCon_RDR	= dataQual pREL_NUM  SLIT(":%")

byteArrayTyCon_RDR		= tcQual pREL_ARR  SLIT("ByteArray")
mutableByteArrayTyCon_RDR	= tcQual pREL_ARR  SLIT("MutableByteArray")

foreignObjTyCon_RDR	= tcQual   pREL_IO_BASE SLIT("ForeignObj")
stablePtrTyCon_RDR	= tcQual   pREL_STABLE SLIT("StablePtr")
stablePtrDataCon_RDR	= dataQual pREL_STABLE SLIT("StablePtr")
deRefStablePtr_RDR      = varQual  pREL_STABLE SLIT("deRefStablePtr")
makeStablePtr_RDR       = varQual  pREL_STABLE SLIT("makeStablePtr")

eqClass_RDR		= clsQual pREL_BASE SLIT("Eq")
ordClass_RDR		= clsQual pREL_BASE SLIT("Ord")
boundedClass_RDR	= clsQual pREL_BASE SLIT("Bounded")
numClass_RDR		= clsQual pREL_BASE SLIT("Num")
enumClass_RDR 		= clsQual pREL_BASE SLIT("Enum")
monadClass_RDR		= clsQual pREL_BASE SLIT("Monad")
monadPlusClass_RDR	= clsQual pREL_BASE SLIT("MonadPlus")
functorClass_RDR	= clsQual pREL_BASE SLIT("Functor")
showClass_RDR		= clsQual pREL_BASE SLIT("Show")
realClass_RDR		= clsQual pREL_NUM  SLIT("Real")
integralClass_RDR	= clsQual pREL_NUM  SLIT("Integral")
fractionalClass_RDR	= clsQual pREL_NUM  SLIT("Fractional")
floatingClass_RDR	= clsQual pREL_NUM  SLIT("Floating")
realFracClass_RDR	= clsQual pREL_NUM  SLIT("RealFrac")
realFloatClass_RDR	= clsQual pREL_NUM  SLIT("RealFloat")
readClass_RDR		= clsQual pREL_READ SLIT("Read")
ixClass_RDR		= clsQual iX	    SLIT("Ix")
ccallableClass_RDR	= clsQual pREL_GHC  SLIT("CCallable")
creturnableClass_RDR	= clsQual pREL_GHC  SLIT("CReturnable")

fromInt_RDR	   = varQual pREL_BASE SLIT("fromInt")
fromInteger_RDR	   = varQual pREL_BASE SLIT("fromInteger")
minus_RDR	   = varQual pREL_BASE SLIT("-")
succ_RDR	   = varQual pREL_BASE SLIT("succ")
pred_RDR	   = varQual pREL_BASE SLIT("pred")
toEnum_RDR	   = varQual pREL_BASE SLIT("toEnum")
fromEnum_RDR	   = varQual pREL_BASE SLIT("fromEnum")
enumFrom_RDR	   = varQual pREL_BASE SLIT("enumFrom")
enumFromTo_RDR	   = varQual pREL_BASE SLIT("enumFromTo")
enumFromThen_RDR   = varQual pREL_BASE SLIT("enumFromThen")
enumFromThenTo_RDR = varQual pREL_BASE SLIT("enumFromThenTo")

thenM_RDR	   = varQual pREL_BASE SLIT(">>=")
returnM_RDR	   = varQual pREL_BASE SLIT("return")
failM_RDR	   = varQual pREL_BASE SLIT("fail")

fromRational_RDR   = varQual pREL_NUM  SLIT("fromRational")
negate_RDR	   = varQual pREL_BASE SLIT("negate")
eq_RDR		   = varQual pREL_BASE SLIT("==")
ne_RDR		   = varQual pREL_BASE SLIT("/=")
le_RDR		   = varQual pREL_BASE SLIT("<=")
lt_RDR		   = varQual pREL_BASE SLIT("<")
ge_RDR		   = varQual pREL_BASE SLIT(">=")
gt_RDR		   = varQual pREL_BASE SLIT(">")
ltTag_RDR	   = dataQual pREL_BASE SLIT("LT")
eqTag_RDR	   = dataQual pREL_BASE SLIT("EQ")
gtTag_RDR	   = dataQual pREL_BASE SLIT("GT")
max_RDR		   = varQual pREL_BASE SLIT("max")
min_RDR		   = varQual pREL_BASE SLIT("min")
compare_RDR	   = varQual pREL_BASE SLIT("compare")
minBound_RDR	   = varQual pREL_BASE SLIT("minBound")
maxBound_RDR	   = varQual pREL_BASE SLIT("maxBound")
false_RDR	   = dataQual pREL_BASE SLIT("False")
true_RDR	   = dataQual pREL_BASE SLIT("True")
and_RDR		   = varQual pREL_BASE SLIT("&&")
not_RDR		   = varQual pREL_BASE SLIT("not")
compose_RDR	   = varQual pREL_BASE SLIT(".")
append_RDR	   = varQual pREL_BASE SLIT("++")
map_RDR		   = varQual pREL_BASE SLIT("map")
concat_RDR	   = varQual mONAD     SLIT("concat")
filter_RDR	   = varQual mONAD     SLIT("filter")
zip_RDR		   = varQual pREL_LIST SLIT("zip")

showList___RDR     = varQual pREL_BASE  SLIT("showList__")
showsPrec_RDR	   = varQual pREL_BASE SLIT("showsPrec")
showList_RDR	   = varQual pREL_BASE SLIT("showList")
showSpace_RDR	   = varQual pREL_BASE SLIT("showSpace")
showString_RDR	   = varQual pREL_BASE SLIT("showString")
showParen_RDR	   = varQual pREL_BASE SLIT("showParen")

range_RDR	   = varQual iX   SLIT("range")
index_RDR	   = varQual iX   SLIT("index")
inRange_RDR	   = varQual iX   SLIT("inRange")

readsPrec_RDR	   = varQual pREL_READ SLIT("readsPrec")
readList_RDR	   = varQual pREL_READ SLIT("readList")
readParen_RDR	   = varQual pREL_READ SLIT("readParen")
lex_RDR		   = varQual pREL_READ SLIT("lex")
readList___RDR     = varQual pREL_READ SLIT("readList__")

plus_RDR	   = varQual pREL_BASE SLIT("+")
times_RDR	   = varQual pREL_BASE SLIT("*")
mkInt_RDR	   = dataQual pREL_BASE SLIT("I#")

int8TyCon_RDR    = tcQual iNT       SLIT("Int8")
int16TyCon_RDR   = tcQual iNT       SLIT("Int16")
int32TyCon_RDR   = tcQual iNT       SLIT("Int32")
int64TyCon_RDR   = tcQual pREL_ADDR SLIT("Int64")

word8TyCon_RDR    = tcQual wORD      SLIT("Word8")
word16TyCon_RDR   = tcQual wORD      SLIT("Word16")
word32TyCon_RDR   = tcQual wORD      SLIT("Word32")
word64TyCon_RDR   = tcQual pREL_ADDR SLIT("Word64")

error_RDR	   = varQual pREL_ERR SLIT("error")
assert_RDR         = varQual pREL_GHC SLIT("assert")
assertErr_RDR      = varQual pREL_ERR SLIT("assertError")

eqH_Char_RDR	= prelude_primop CharEqOp
ltH_Char_RDR	= prelude_primop CharLtOp
eqH_Word_RDR	= prelude_primop WordEqOp
ltH_Word_RDR	= prelude_primop WordLtOp
eqH_Addr_RDR	= prelude_primop AddrEqOp
ltH_Addr_RDR	= prelude_primop AddrLtOp
eqH_Float_RDR	= prelude_primop FloatEqOp
ltH_Float_RDR	= prelude_primop FloatLtOp
eqH_Double_RDR	= prelude_primop DoubleEqOp
ltH_Double_RDR	= prelude_primop DoubleLtOp
eqH_Int_RDR	= prelude_primop IntEqOp
ltH_Int_RDR	= prelude_primop IntLtOp
geH_RDR		= prelude_primop IntGeOp
leH_RDR		= prelude_primop IntLeOp
minusH_RDR	= prelude_primop IntSubOp
tagToEnumH_RDR	= prelude_primop TagToEnumOp

getTag_RDR	= varQual pREL_GHC SLIT("getTag#")
\end{code}

\begin{code}
mkTupConRdrName :: Int -> RdrName 
mkTupConRdrName arity = case mkTupNameStr arity of
			  (mod, occ) -> dataQual mod occ

mkUbxTupConRdrName :: Int -> RdrName
mkUbxTupConRdrName arity = case mkUbxTupNameStr arity of
				(mod, occ) -> dataQual mod occ
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
			 lex_RDR, readParen_RDR, readList___RDR, thenM_RDR])
			     -- returnM (and the rest of the Monad class decl) 
			     -- will be forced in as result of depending
			     -- on thenM.   -- SOF 1/99
    , (ixClassKey,	[intTyCon_RDR, numClass_RDR, and_RDR, map_RDR, enumFromTo_RDR, 
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
isCcallishClass, isCreturnableClass, isNoDictClass, 
  isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys
isCcallishClass	   clas = classKey clas `is_elem` cCallishClassKeys
isCreturnableClass clas = classKey clas == cReturnableClassKey
isNoDictClass      clas = classKey clas `is_elem` noDictClassKeys
is_elem = isIn "is_X_Class"

numericClassKeys =
	[ numClassKey
    	, realClassKey
    	, integralClassKey
    	, fractionalClassKey
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

