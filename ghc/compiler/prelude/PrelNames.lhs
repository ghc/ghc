%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelNames]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod 

[oh dear, looks like the recursive module monster caught up with
 and gobbled whoever was writing the above :-) -- SOF ]

\begin{code}
module PrelNames
        (
	-- Prelude modules
	pREL_GHC, pREL_BASE, pREL_ADDR, pREL_STABLE,
	pREL_IO_BASE, pREL_PACK, pREL_ERR, pREL_NUM, pREL_FLOAT, pREL_REAL,

	-- Module names (both Prelude and otherwise)
	pREL_GHC_Name, pRELUDE_Name, 
	mAIN_Name, pREL_MAIN_Name, pREL_ERR_Name,
	pREL_BASE_Name, pREL_NUM_Name, pREL_LIST_Name, 
	pREL_TUP_Name, pREL_ADDR_Name, pREL_READ_Name,
	pREL_PACK_Name, pREL_CONC_Name, pREL_IO_BASE_Name, 
	pREL_ST_Name, pREL_ARR_Name, pREL_BYTEARR_Name, pREL_FOREIGN_Name,
	pREL_STABLE_Name, pREL_SHOW_Name, pREL_ENUM_Name, iNT_Name, wORD_Name,
	pREL_REAL_Name, pREL_FLOAT_Name,

	-- RdrNames for lots of things, mainly used in derivings
	eq_RDR, ne_RDR, le_RDR, lt_RDR, ge_RDR, gt_RDR, max_RDR, min_RDR, 
	compare_RDR, minBound_RDR, maxBound_RDR, enumFrom_RDR, enumFromTo_RDR,
	enumFromThen_RDR, enumFromThenTo_RDR, succ_RDR, pred_RDR, fromEnum_RDR, toEnum_RDR, 
	ratioDataCon_RDR, range_RDR, index_RDR, inRange_RDR, readsPrec_RDR,
	readList_RDR, showsPrec_RDR, showList_RDR, plus_RDR, times_RDR,
	ltTag_RDR, eqTag_RDR, gtTag_RDR, false_RDR, true_RDR,
	and_RDR, not_RDR, append_RDR, map_RDR, compose_RDR, mkInt_RDR,
	error_RDR, assertErr_RDR, 
	showString_RDR, showParen_RDR, readParen_RDR, lex_RDR,
	showSpace_RDR, showList___RDR, readList___RDR, negate_RDR,
	addr2Integer_RDR, ioTyCon_RDR,
	foldr_RDR, build_RDR, getTag_RDR, 

	orderingTyCon_RDR, rationalTyCon_RDR, ratioTyCon_RDR, byteArrayTyCon_RDR,
	mutableByteArrayTyCon_RDR, foreignObjTyCon_RDR,
	intTyCon_RDR, stablePtrTyCon_RDR, stablePtrDataCon_RDR, 
	int8TyCon_RDR, int16TyCon_RDR, int32TyCon_RDR, int64TyCon_RDR,
	word8TyCon_RDR, word16TyCon_RDR, word32TyCon_RDR, word64TyCon_RDR,

	boundedClass_RDR, monadPlusClass_RDR, functorClass_RDR, showClass_RDR, 
	realClass_RDR, integralClass_RDR, floatingClass_RDR, realFracClass_RDR,
	realFloatClass_RDR, readClass_RDR, ixClass_RDR, 
	fromInt_RDR, fromInteger_RDR, minus_RDR, fromRational_RDR, 

	bindIO_RDR, returnIO_RDR, thenM_RDR, returnM_RDR, failM_RDR,

	deRefStablePtr_RDR, makeStablePtr_RDR, 
	concat_RDR, filter_RDR, zip_RDR, augment_RDR,
	otherwiseId_RDR, assert_RDR, runSTRep_RDR,

	unpackCString_RDR, unpackCString2_RDR, unpackCStringAppend_RDR, unpackCStringFoldr_RDR,
	numClass_RDR, fractionalClass_RDR, eqClass_RDR, 
	ccallableClass_RDR, creturnableClass_RDR,
	monadClass_RDR, enumClass_RDR, ordClass_RDR,
	ioDataCon_RDR,

        main_RDR,

        mkTupNameStr, mkTupConRdrName

	) where

#include "HsVersions.h"

import Module	  ( Module, ModuleName, mkPrelModule, mkSrcModule )
import OccName	  ( NameSpace, varName, dataName, tcName, clsName )
import RdrName	  ( RdrName, mkPreludeQual )
import BasicTypes ( Boxity(..), Arity )
import Util	  ( nOfThem )
import Panic	  ( panic )
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

-- Strings
unpackCString_RDR       = varQual pREL_BASE_Name SLIT("unpackCString#")
unpackCString2_RDR      = varQual pREL_BASE_Name SLIT("unpackNBytes#")
unpackCStringAppend_RDR = varQual pREL_BASE_Name SLIT("unpackAppendCString#")
unpackCStringFoldr_RDR  = varQual pREL_BASE_Name SLIT("unpackFoldrCString#")

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
addr2Integer_RDR   = varQual pREL_NUM_Name   SLIT("addr2Integer")

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

