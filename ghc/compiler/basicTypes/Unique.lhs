%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Unique]{The @Unique@ data type and a (monadic) supply thereof}

@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@
comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).

\begin{code}
#include "HsVersions.h"

module Unique (
	Unique,
	UniqueSupply,			-- abstract types
	u2i,				-- hack: used in UniqFM
	getUnique, getUniques,		-- basic ops
	eqUnique, cmpUnique,		-- comparison is everything!

--not exported:	mkUnique, unpkUnique,
	mkUniqueGrimily,		-- use in SplitUniq only!
	mkUniqueSupplyGrimily,		-- ditto! (but FALSE: WDP 95/01)
	mkUnifiableTyVarUnique,
	unpkUnifiableTyVarUnique,
	showUnique, pprUnique, pprUnique10,

	UniqSM(..),		-- type: unique supply monad
	initUs, thenUs, returnUs,
	mapUs, mapAndUnzipUs,

	-- the pre-defined unique supplies:
{- NOT exported:
	uniqSupply_r, uniqSupply_t, uniqSupply_d,
	uniqSupply_s, uniqSupply_c, uniqSupply_T,
	uniqSupply_f,
	uniqSupply_P,
-}
	uniqSupply_u,
#ifdef DPH
	-- otherwise, not exported
	uniqSupply_p, uniqSupply_S, uniqSupply_L,
#endif

	-- and the access functions for the `builtin' UniqueSupply
	getBuiltinUniques, mkBuiltinUnique, runBuiltinUs,
	mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,

	-- now all the built-in Uniques (and functions to make them)
	-- [the Oh-So-Wonderful Haskell module system wins again...]
	mkPrimOpIdUnique,
	mkTupleDataConUnique,

	absentErrorIdKey,
	runSTIdKey, realWorldPrimIdKey,
	arrayPrimTyConKey,
	byteArrayPrimTyConKey, --UNUSED: byteArrayDataConKey, byteArrayTyConKey,
	binaryClassKey,
	boolTyConKey, buildDataConKey, buildIdKey, charDataConKey,
	charPrimTyConKey, charTyConKey, cmpTagTyConKey,
	consDataConKey,
	dialogueTyConKey,
	doubleDataConKey,
	doublePrimTyConKey,
	doubleTyConKey,
	enumClassKey, eqClassKey,
	eqTagDataConKey, errorIdKey,
	falseDataConKey, floatDataConKey,
	floatPrimTyConKey, floatTyConKey, floatingClassKey,
	foldlIdKey, foldrIdKey,
	forkIdKey,
	fractionalClassKey,
	gtTagDataConKey, --UNUSED: iOErrorTyConKey,
--UNUSED:	iOIntPrimTyConKey, -- UNUSED: int2IntegerIdKey,
	iOTyConKey,
	intDataConKey,
	wordPrimTyConKey, wordTyConKey, wordDataConKey,
	addrPrimTyConKey, addrTyConKey, addrDataConKey,
	intPrimTyConKey, intTyConKey,
	integerDataConKey, integerTyConKey, integralClassKey,
	ixClassKey,
--UNUSED:	lexIdKey,
	liftDataConKey, liftTyConKey, listTyConKey,
	ltTagDataConKey,
	mutableArrayPrimTyConKey, -- UNUSED: mutableArrayDataConKey, mutableArrayTyConKey,
	mutableByteArrayPrimTyConKey, -- UNUSED: mutableByteArrayDataConKey,
--UNUSED:	mutableByteArrayTyConKey,
	synchVarPrimTyConKey,
	nilDataConKey, numClassKey, ordClassKey,
	parIdKey, parErrorIdKey,
#ifdef GRAN
	parGlobalIdKey, parLocalIdKey, copyableIdKey, noFollowIdKey,
#endif
	patErrorIdKey,
	ratioDataConKey, ratioTyConKey,
	rationalTyConKey,
--UNUSED:	readParenIdKey,
	realClassKey, realFloatClassKey,
	realFracClassKey,
--UNUSED:	requestTyConKey, responseTyConKey,
	return2GMPsDataConKey, return2GMPsTyConKey,
	returnIntAndGMPDataConKey, returnIntAndGMPTyConKey,
	seqIdKey, -- UNUSED: seqIntPrimTyConKey,
--UNUSED:	seqTyConKey,
--UNUSED:	showParenIdKey,
--UNUSED:	showSpaceIdKey,
	statePrimTyConKey, stateTyConKey, stateDataConKey,
	voidPrimTyConKey,
	realWorldTyConKey,
	stablePtrPrimTyConKey, stablePtrTyConKey, stablePtrDataConKey,
	mallocPtrPrimTyConKey, mallocPtrTyConKey, mallocPtrDataConKey,
	stateAndPtrPrimTyConKey,
	stateAndPtrPrimDataConKey,
	stateAndCharPrimTyConKey,
	stateAndCharPrimDataConKey,
	stateAndIntPrimTyConKey,
	stateAndIntPrimDataConKey,
	stateAndWordPrimTyConKey,
	stateAndWordPrimDataConKey,
	stateAndAddrPrimTyConKey,
	stateAndAddrPrimDataConKey,
	stateAndStablePtrPrimTyConKey,
	stateAndStablePtrPrimDataConKey,
	stateAndMallocPtrPrimTyConKey,
	stateAndMallocPtrPrimDataConKey,
	stateAndFloatPrimTyConKey,
	stateAndFloatPrimDataConKey,
	stateAndDoublePrimTyConKey,
	stateAndDoublePrimDataConKey,
	stateAndArrayPrimTyConKey,
	stateAndArrayPrimDataConKey,
	stateAndMutableArrayPrimTyConKey,
	stateAndMutableArrayPrimDataConKey,
	stateAndByteArrayPrimTyConKey,
	stateAndByteArrayPrimDataConKey,
	stateAndMutableByteArrayPrimTyConKey,
	stateAndMutableByteArrayPrimDataConKey,
	stateAndSynchVarPrimTyConKey,
	stateAndSynchVarPrimDataConKey,
	stringTyConKey,
	stTyConKey, primIoTyConKey,
--UNUSED:	ioResultTyConKey,
	textClassKey,
	traceIdKey,
	trueDataConKey,
	unpackCStringIdKey, unpackCString2IdKey, unpackCStringAppendIdKey,
	packCStringIdKey,
	integerZeroIdKey, integerPlusOneIdKey, integerMinusOneIdKey,
	voidPrimIdKey,
	cCallableClassKey,
	cReturnableClassKey,
--UNUSED:	packedStringTyConKey, psDataConKey, cpsDataConKey,

	-- to make interface self-sufficient
	PrimOp, SplitUniqSupply, CSeq

#ifndef __GLASGOW_HASKELL__
	, TAG_
#endif
    ) where

import Outputable	-- class for printing, forcing
import Pretty
import PrimOps		-- ** DIRECTLY **
import SplitUniq
import Util

#ifndef __GLASGOW_HASKELL__
{-hide import from mkdependHS-}
import
	Word
#endif
#ifdef __GLASGOW_HASKELL__
import PreludeGlaST
#endif

infixr 9 `thenUs`
\end{code}

%************************************************************************
%*									*
\subsection[Unique-type]{@Unique@ type and operations}
%*									*
%************************************************************************

The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:

\begin{code}
u2i :: Unique -> FAST_INT

#ifdef __GLASGOW_HASKELL__

data Unique = MkUnique Int#
u2i (MkUnique i) = i

#else

data Unique = MkUnique Word{-#STRICT#-}
u2i (MkUnique w) = wordToInt w

#endif
\end{code}

Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.

\begin{code}
mkUnique		 :: Char -> Int -> Unique	-- Builds a unique from pieces
unpkUnique		 :: Unique -> (Char, Int)	-- The reverse

mkUnifiableTyVarUnique	 :: Int -> Unique	-- Injects a subst-array index into the Unique type
unpkUnifiableTyVarUnique :: Unique -> Int	-- The reverse process

#ifdef __GLASGOW_HASKELL__
mkUniqueGrimily :: Int# -> Unique		-- A trap-door for SplitUniq
#else
mkUniqueGrimily :: Int -> Unique
#endif
\end{code}


\begin{code}
#ifdef __GLASGOW_HASKELL__
mkUniqueGrimily x = MkUnique x
#else
mkUniqueGrimily x = MkUnique (fromInteger (toInteger x))
#endif

mkUnifiableTyVarUnique i = mkUnique '_'{-MAGIC CHAR-} i

unpkUnifiableTyVarUnique uniq
  = case (unpkUnique uniq) of { (tag, i) ->
    ASSERT(tag == '_'{-MAGIC CHAR-})
    i }

-- pop the Char in the top 8 bits of the Unique(Supply)

#ifdef __GLASGOW_HASKELL__

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x::Int#)

mkUnique (MkChar c#) (MkInt i#)
  = MkUnique (w2i (((i2w (ord# c#)) `shiftL#` (i2w_s 24#)) `or#` (i2w i#)))

unpkUnique (MkUnique u)
  = let
	tag = MkChar (chr# (w2i ((i2w u) `shiftr` (i2w_s 24#))))
	i   = MkInt  (w2i ((i2w u) `and#` (i2w 16777215#){-``0x00ffffff''-}))
    in
    (tag, i)
# if __GLASGOW_HASKELL__ >= 23
  where
    shiftr x y = shiftRA# x y
# else
    shiftr x y = shiftR#  x y
# endif

#else {-probably HBC-}

mkUnique c i
  = MkUnique (((fromInt (ord c)) `bitLsh` 24) `bitOr` (fromInt i))

unpkUnique (MkUnique u)
  = let
	tag = chr (wordToInt (u `bitRsh` 24))
	i   = wordToInt (u `bitAnd` 16777215 {-0x00ffffff-})
    in
    (tag, i)

#endif	{-probably HBC-}
\end{code}

%************************************************************************
%*									*
\subsection[Unique-instances]{Instance declarations for @Unique@}
%*									*
%************************************************************************

And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).

\begin{code}
#ifdef __GLASGOW_HASKELL__

{-# INLINE eqUnique  #-} -- this is Hammered City here...
{-# INLINE cmpUnique #-}

eqUnique (MkUnique u1) (MkUnique u2) = u1 ==# u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <#  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <=# u2

cmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 ==# u2 then EQ_ else if u1 <# u2 then LT_ else GT_

#else
eqUnique (MkUnique u1) (MkUnique u2) = u1 == u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <= u2

cmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 == u2 then EQ_ else if u1 < u2 then LT_ else GT_
#endif

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpUnique a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}

And for output:
\begin{code}
{- OLD:
instance Outputable Unique where
   ppr any_style uniq
     = case unpkUnique uniq of
	 (tag, u) -> ppStr (tag : iToBase62 u)
-}
\end{code}

We do sometimes make strings with @Uniques@ in them:
\begin{code}
pprUnique, pprUnique10 :: Unique -> Pretty

pprUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> ppBeside (ppChar tag) (iToBase62 u)

pprUnique10 uniq	-- in base-10, dudes
  = case unpkUnique uniq of
      (tag, u) -> ppBeside (ppChar tag) (ppInt u)

showUnique :: Unique -> FAST_STRING
showUnique uniq = _PK_ (ppShow 80 (pprUnique uniq))

instance Text Unique where
    showsPrec p uniq rest = _UNPK_ (showUnique uniq)
    readsPrec p = panic "no readsPrec for Unique"
\end{code}

%************************************************************************
%*									*
\subsection[Utils-base62]{Base-62 numbers}
%*									*
%************************************************************************

A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.
\begin{code}
iToBase62 :: Int -> Pretty

#ifdef __GLASGOW_HASKELL__
iToBase62 n@(I# n#)
  = ASSERT(n >= 0)
    let
	bytes = case chars62 of { _ByteArray bounds_who_needs_'em bytes -> bytes }
    in
    if n# <# 62# then 
	case (indexCharArray# bytes n#) of { c ->
	ppChar (C# c) }
    else
	case (quotRem n 62)		of { (q, I# r#) ->
	case (indexCharArray# bytes r#) of { c  ->
	ppBeside (iToBase62 q) (ppChar (C# c)) }}

-- keep this at top level! (bug on 94/10/24 WDP)
chars62 :: _ByteArray Int
chars62
  = _runST (
	newCharArray (0, 61)	`thenStrictlyST` \ ch_array ->
	fill_in ch_array 0 62 "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
				`seqStrictlyST`
	unsafeFreezeByteArray ch_array
    )
  where
    fill_in ch_array i lim str
      | i == lim
      = returnStrictlyST ()
      | otherwise
      = writeCharArray ch_array i (str !! i)	`seqStrictlyST`
	fill_in ch_array (i+1) lim str

#else {- not GHC -}
iToBase62 n
  = ASSERT(n >= 0)
    if n < 62 then 
	ppChar (chars62 ! n)
    else
	case (quotRem n 62) of { (q, r) ->
	ppBeside (iToBase62 q) (ppChar (chars62 ! r)) }

-- keep this at top level! (bug on 94/10/24 WDP)
chars62 :: Array Int Char
chars62
  = array (0,61) (zipWith (:=) [0..] "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
#endif {- not GHC -}
\end{code}

%************************************************************************
%*									*
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
%*									*
%************************************************************************

\begin{code}
mkPreludeClassUnique i		= mkUnique '1' i
mkPreludeTyConUnique i		= mkUnique '2' i
mkPreludeDataConUnique i	= mkUnique 'Y' i -- must be alphabetic
mkTupleDataConUnique i		= mkUnique 'Z' i -- ditto (*may* be used in C labels)
-- mkPrimOpIdUnique op: see below (uses '5')
mkPreludeMiscIdUnique i		= mkUnique '7' i
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
%*									*
%************************************************************************

\begin{code}
eqClassKey		= mkPreludeClassUnique 1
ordClassKey		= mkPreludeClassUnique 2
numClassKey		= mkPreludeClassUnique 3
integralClassKey	= mkPreludeClassUnique 4
fractionalClassKey	= mkPreludeClassUnique 5
floatingClassKey	= mkPreludeClassUnique 6
realClassKey		= mkPreludeClassUnique 7
realFracClassKey	= mkPreludeClassUnique 8
realFloatClassKey	= mkPreludeClassUnique 9
ixClassKey		= mkPreludeClassUnique 10
enumClassKey		= mkPreludeClassUnique 11
textClassKey		= mkPreludeClassUnique 12
binaryClassKey		= mkPreludeClassUnique 13
cCallableClassKey	= mkPreludeClassUnique 14
cReturnableClassKey	= mkPreludeClassUnique 15
#ifdef DPH
pidClassKey		= mkPreludeClassUnique 16
processorClassKey	= mkPreludeClassUnique 17
#endif {- Data Parallel Haskell -}
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
--UNUSED:byteArrayTyConKey			= mkPreludeTyConUnique	6
charPrimTyConKey			= mkPreludeTyConUnique	7
charTyConKey				= mkPreludeTyConUnique  8
cmpTagTyConKey				= mkPreludeTyConUnique  9
dialogueTyConKey			= mkPreludeTyConUnique 10
doublePrimTyConKey			= mkPreludeTyConUnique 11
doubleTyConKey				= mkPreludeTyConUnique 12
floatPrimTyConKey			= mkPreludeTyConUnique 13
floatTyConKey				= mkPreludeTyConUnique 14
--UNUSED:iOErrorTyConKey				= mkPreludeTyConUnique 14
--UNUSED:iOIntPrimTyConKey			= mkPreludeTyConUnique 15
iOTyConKey				= mkPreludeTyConUnique 16
intPrimTyConKey				= mkPreludeTyConUnique 17
intTyConKey				= mkPreludeTyConUnique 18
integerTyConKey				= mkPreludeTyConUnique 19
liftTyConKey				= mkPreludeTyConUnique 20
listTyConKey				= mkPreludeTyConUnique 21
mallocPtrPrimTyConKey			= mkPreludeTyConUnique 22
mallocPtrTyConKey			= mkPreludeTyConUnique 23
mutableArrayPrimTyConKey		= mkPreludeTyConUnique 24
--UNUSED:mutableArrayTyConKey			= mkPreludeTyConUnique 25
mutableByteArrayPrimTyConKey		= mkPreludeTyConUnique 26
--UNUSED:mutableByteArrayTyConKey		= mkPreludeTyConUnique 27
--UNUSED:packedStringTyConKey			= mkPreludeTyConUnique 28
synchVarPrimTyConKey		    	= mkPreludeTyConUnique 29
ratioTyConKey				= mkPreludeTyConUnique 30
rationalTyConKey			= mkPreludeTyConUnique 31
realWorldTyConKey			= mkPreludeTyConUnique 32
--UNUSED:requestTyConKey				= mkPreludeTyConUnique 33
--UNUSED:responseTyConKey			= mkPreludeTyConUnique 34
return2GMPsTyConKey			= mkPreludeTyConUnique 35
returnIntAndGMPTyConKey			= mkPreludeTyConUnique 36
--UNUSED:seqIntPrimTyConKey			= mkPreludeTyConUnique 37
--UNUSED:seqTyConKey				= mkPreludeTyConUnique 38
stablePtrPrimTyConKey			= mkPreludeTyConUnique 39
stablePtrTyConKey			= mkPreludeTyConUnique 40
stateAndAddrPrimTyConKey		= mkPreludeTyConUnique 41
stateAndArrayPrimTyConKey		= mkPreludeTyConUnique 42
stateAndByteArrayPrimTyConKey		= mkPreludeTyConUnique 43
stateAndCharPrimTyConKey		= mkPreludeTyConUnique 44
stateAndDoublePrimTyConKey		= mkPreludeTyConUnique 45
stateAndFloatPrimTyConKey		= mkPreludeTyConUnique 46
stateAndIntPrimTyConKey			= mkPreludeTyConUnique 47
stateAndMallocPtrPrimTyConKey		= mkPreludeTyConUnique 48
stateAndMutableArrayPrimTyConKey	= mkPreludeTyConUnique 49
stateAndMutableByteArrayPrimTyConKey	= mkPreludeTyConUnique 50
stateAndSynchVarPrimTyConKey	    	= mkPreludeTyConUnique 51
stateAndPtrPrimTyConKey			= mkPreludeTyConUnique 52
stateAndStablePtrPrimTyConKey		= mkPreludeTyConUnique 53
stateAndWordPrimTyConKey		= mkPreludeTyConUnique 54
statePrimTyConKey			= mkPreludeTyConUnique 55
stateTyConKey				= mkPreludeTyConUnique 56
stringTyConKey				= mkPreludeTyConUnique 57
stTyConKey				= mkPreludeTyConUnique 58
primIoTyConKey				= mkPreludeTyConUnique 59
--UNUSED:ioResultTyConKey			= mkPreludeTyConUnique 60
voidPrimTyConKey			= mkPreludeTyConUnique 61
wordPrimTyConKey			= mkPreludeTyConUnique 62 
wordTyConKey				= mkPreludeTyConUnique 63
							       
#ifdef DPH
podTyConKey				= mkPreludeTyConUnique 64
interfacePodTyConKey			= mkPreludeTyConUnique 65

podizedPodTyConKey _ = panic "ToDo:DPH:podizedPodTyConKey"
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*									*
%************************************************************************

\begin{code}
addrDataConKey				= mkPreludeDataConUnique  1
buildDataConKey				= mkPreludeDataConUnique  2
--UNUSED:byteArrayDataConKey			= mkPreludeDataConUnique  3
charDataConKey				= mkPreludeDataConUnique  4
consDataConKey				= mkPreludeDataConUnique  5
doubleDataConKey			= mkPreludeDataConUnique  6
eqTagDataConKey				= mkPreludeDataConUnique  7
falseDataConKey				= mkPreludeDataConUnique  8
floatDataConKey				= mkPreludeDataConUnique  9
gtTagDataConKey				= mkPreludeDataConUnique 10
intDataConKey				= mkPreludeDataConUnique 11
integerDataConKey			= mkPreludeDataConUnique 12
liftDataConKey				= mkPreludeDataConUnique 13
ltTagDataConKey				= mkPreludeDataConUnique 14
mallocPtrDataConKey			= mkPreludeDataConUnique 15
--UNUSED:mutableArrayDataConKey			= mkPreludeDataConUnique 16
--UNUSED:mutableByteArrayDataConKey		= mkPreludeDataConUnique 17
nilDataConKey				= mkPreludeDataConUnique 18
--UNUSED:psDataConKey				= mkPreludeDataConUnique 19
--UNUSED:cpsDataConKey				= mkPreludeDataConUnique 20
ratioDataConKey				= mkPreludeDataConUnique 21
return2GMPsDataConKey			= mkPreludeDataConUnique 22
returnIntAndGMPDataConKey		= mkPreludeDataConUnique 23
stablePtrDataConKey			= mkPreludeDataConUnique 24
stateAndAddrPrimDataConKey		= mkPreludeDataConUnique 25
stateAndArrayPrimDataConKey		= mkPreludeDataConUnique 26
stateAndByteArrayPrimDataConKey		= mkPreludeDataConUnique 27
stateAndCharPrimDataConKey		= mkPreludeDataConUnique 28
stateAndDoublePrimDataConKey		= mkPreludeDataConUnique 29
stateAndFloatPrimDataConKey		= mkPreludeDataConUnique 30
stateAndIntPrimDataConKey		= mkPreludeDataConUnique 31
stateAndMallocPtrPrimDataConKey		= mkPreludeDataConUnique 32
stateAndMutableArrayPrimDataConKey	= mkPreludeDataConUnique 33
stateAndMutableByteArrayPrimDataConKey	= mkPreludeDataConUnique 34
stateAndSynchVarPrimDataConKey	    	= mkPreludeDataConUnique 35
stateAndPtrPrimDataConKey		= mkPreludeDataConUnique 36
stateAndStablePtrPrimDataConKey		= mkPreludeDataConUnique 37
stateAndWordPrimDataConKey		= mkPreludeDataConUnique 38
stateDataConKey				= mkPreludeDataConUnique 39
trueDataConKey				= mkPreludeDataConUnique 40
wordDataConKey				= mkPreludeDataConUnique 41

#ifdef DPH
interfacePodDataConKey			= mkPreludeDataConUnique 42
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*									*
%************************************************************************

First, for raw @PrimOps@ and their boxed versions:
\begin{code}
mkPrimOpIdUnique :: PrimOp -> Unique

mkPrimOpIdUnique op = mkUnique '5' IBOX((tagOf_PrimOp op))
\end{code}

Now for other non-@DataCon@ @Ids@:
\begin{code}
absentErrorIdKey	= mkPreludeMiscIdUnique	 1
buildIdKey		= mkPreludeMiscIdUnique	 2
errorIdKey		= mkPreludeMiscIdUnique	 3
foldlIdKey		= mkPreludeMiscIdUnique	 4
foldrIdKey		= mkPreludeMiscIdUnique	 5
forkIdKey   	    	= mkPreludeMiscIdUnique  6 
int2IntegerIdKey	= mkPreludeMiscIdUnique	 7
integerMinusOneIdKey	= mkPreludeMiscIdUnique	 8
integerPlusOneIdKey	= mkPreludeMiscIdUnique	 9
integerZeroIdKey	= mkPreludeMiscIdUnique	10
--UNUSED:lexIdKey		= mkPreludeMiscIdUnique 11
packCStringIdKey	= mkPreludeMiscIdUnique	12
parIdKey		= mkPreludeMiscIdUnique	13
parErrorIdKey		= mkPreludeMiscIdUnique	14
patErrorIdKey		= mkPreludeMiscIdUnique	15
--UNUSED:readParenIdKey		= mkPreludeMiscIdUnique 16
realWorldPrimIdKey	= mkPreludeMiscIdUnique 17
runSTIdKey		= mkPreludeMiscIdUnique 18
seqIdKey		= mkPreludeMiscIdUnique 19
--UNUSED:showParenIdKey		= mkPreludeMiscIdUnique	20
--UNUSED:showSpaceIdKey		= mkPreludeMiscIdUnique	21
traceIdKey		= mkPreludeMiscIdUnique	22
unpackCStringIdKey	= mkPreludeMiscIdUnique	23
unpackCString2IdKey	= mkPreludeMiscIdUnique	20 -- NB: NB: NB
unpackCStringAppendIdKey= mkPreludeMiscIdUnique	21 -- NB: NB: NB
voidPrimIdKey		= mkPreludeMiscIdUnique	24

#ifdef GRAN
parLocalIdKey		= mkPreludeMiscIdUnique 25
parGlobalIdKey		= mkPreludeMiscIdUnique 26
noFollowIdKey		= mkPreludeMiscIdUnique 27
copyableIdKey		= mkPreludeMiscIdUnique 28
#endif

#ifdef DPH
podSelectorIdKey	= mkPreludeMiscIdUnique 29
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[UniqueSupply-type]{@UniqueSupply@ type and operations}
%*									*
%************************************************************************

\begin{code}
#ifdef __GLASGOW_HASKELL__
data UniqueSupply
  = MkUniqueSupply  Int#
  | MkNewSupply	    SplitUniqSupply

#else
data UniqueSupply
  = MkUniqueSupply  Word{-#STRICT#-}
  | MkNewSupply	    SplitUniqSupply
#endif
\end{code}

@mkUniqueSupply@ is used to get a @UniqueSupply@ started.
\begin{code}
mkUniqueSupply :: Char -> UniqueSupply

#ifdef __GLASGOW_HASKELL__

mkUniqueSupply (MkChar c#)
  = MkUniqueSupply (w2i ((i2w (ord# c#)) `shiftL#` (i2w_s 24#)))

#else

mkUniqueSupply c
  = MkUniqueSupply ((fromInt (ord c)) `bitLsh` 24)

#endif

mkUniqueSupplyGrimily s = MkNewSupply s
\end{code}

The basic operation on a @UniqueSupply@ is to get a @Unique@ (or a
few).  It's just plain different when splittable vs.~not...
\begin{code}
getUnique :: UniqueSupply -> (UniqueSupply, Unique)

getUnique (MkUniqueSupply n)
#ifdef __GLASGOW_HASKELL__
  = (MkUniqueSupply (n +# 1#), MkUnique n)
#else
  = (MkUniqueSupply (n + 1), MkUnique n)
#endif
getUnique (MkNewSupply s)
  = let
	(u, s1) = getSUniqueAndDepleted s
    in
    (MkNewSupply s1, u)

getUniques :: Int		-- how many you want
	   -> UniqueSupply
	   -> (UniqueSupply, [Unique])

#ifdef __GLASGOW_HASKELL__
getUniques i@(MkInt i#) (MkUniqueSupply n)
  = (MkUniqueSupply (n +# i#),
     [ case x of { MkInt x# ->
	 MkUnique (n +# x#) } | x <- [0 .. i-1] ])
#else
getUniques i (MkUniqueSupply n)
  = (MkUniqueSupply (n + fromInt i), [ MkUnique (n + fromInt x) | x <- [0 .. i-1] ])
#endif
getUniques i (MkNewSupply s)
  = let
	(us, s1) = getSUniquesAndDepleted i s
    in
    (MkNewSupply s1, us)
\end{code}

[OLD-ish NOTE] Simon says: The last line is preferable over @(n+i,
<mumble> [n .. (n+i-1)])@, because it is a little lazier.  If n=bot
you get ([bot, bot, bot], bot) back instead of (bot,bot).  This is
sometimes important for knot-tying.

Alternatively, if you hate the inefficiency:
\begin{pseudocode}
(range 0, n+i)	where range m | m=i = []
		      range m	    = n+m : range (m+1)
\end{pseudocode}

%************************************************************************
%*									*
\subsection[UniqueSupplies-compiler]{@UniqueSupplies@ specific to the compiler}
%*									*
%************************************************************************

Different parts of the compiler have their own @UniqueSupplies@, each
identified by their ``tag letter:''
\begin{verbatim}
    B		builtin; for when the compiler conjures @Uniques@ out of
		thin air
    b		a second builtin; we need two in mkWrapperUnfolding (False)
    r		renamer
    t		typechecker
    d		desugarer
    p		``podizer'' (DPH only)
    s		core-to-core simplifier
    S		``pod'' simplifier (DPH only)
    c		core-to-stg
    T		stg-to-stg simplifier
    f		flattener (of abstract~C)
    L		Assembly labels (for native-code generators)
    u		Printing out unfoldings (so don't have constant renaming)
    P		profiling (finalCCstg)

    v		used in specialised TyVarUniques (see TyVar.lhs)

    1-9		used for ``prelude Uniques'' (wired-in things; see below)
		1 = classes
		2 = tycons
		3 = data cons
		4 = tuple datacons
		5 = unboxed-primop ids
		6 = boxed-primop ids
		7 = misc ids
\end{verbatim}

\begin{code}
uniqSupply_r = mkUniqueSupply 'r'
uniqSupply_t = mkUniqueSupply 't'
uniqSupply_d = mkUniqueSupply 'd'
uniqSupply_p = mkUniqueSupply 'p'
uniqSupply_s = mkUniqueSupply 's'
uniqSupply_S = mkUniqueSupply 'S'
uniqSupply_c = mkUniqueSupply 'c'
uniqSupply_T = mkUniqueSupply 'T'
uniqSupply_f = mkUniqueSupply 'f'
uniqSupply_L = mkUniqueSupply 'L'
uniqSupply_u = mkUniqueSupply 'u'
uniqSupply_P = mkUniqueSupply 'P'
\end{code}

The ``builtin UniqueSupplies'' are more magical.  You don't use the
supply, you ask for @Uniques@ directly from it.	 (They probably aren't
unique, but you know that!)

\begin{code}
uniqSupply_B = mkUniqueSupply 'B' -- not exported!
uniqSupply_b = mkUniqueSupply 'b' -- not exported!
\end{code}

\begin{code}
mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,
 mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUnique1 i = mkUnique 'C' i -- used for getTheUnique on Regs
mkPseudoUnique2 i = mkUnique 'D' i -- ditto
mkPseudoUnique3 i = mkUnique 'E' i -- ditto

getBuiltinUniques :: Int -> [Unique]
getBuiltinUniques n = map (mkUnique 'B') [1 .. n]
\end{code}

The following runs a uniq monad expression, using builtin uniq values:
\begin{code}
runBuiltinUs :: UniqSM a -> a
runBuiltinUs m = snd (initUs uniqSupply_B m)
\end{code}

%************************************************************************
%*									*
\subsection[Unique-monad]{Unique supply monad}
%*									*
%************************************************************************

A very plain unique-supply monad.

\begin{code}
type UniqSM result = UniqueSupply -> (UniqueSupply, result)

-- the initUs function also returns the final UniqueSupply

initUs :: UniqueSupply -> UniqSM a -> (UniqueSupply, a)

initUs init_us m = m init_us

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenUs #-}
{-# INLINE returnUs #-}
#endif
\end{code}

@thenUs@ is are where we split the @UniqueSupply@.
\begin{code}
thenUs :: UniqSM a -> (a -> UniqSM b) -> UniqSM b

thenUs expr cont us
  = case (expr us) of
      (us1, result) -> cont result us1
\end{code}

\begin{code}
returnUs :: a -> UniqSM a
returnUs result us = (us, result)

mapUs :: (a -> UniqSM b) -> [a] -> UniqSM [b]

mapUs f []     = returnUs []
mapUs f (x:xs)
  = f x		`thenUs` \ r  ->
    mapUs f xs	`thenUs` \ rs ->
    returnUs (r:rs)

mapAndUnzipUs  :: (a -> UniqSM (b,c))	-> [a] -> UniqSM ([b],[c])

mapAndUnzipUs f [] = returnUs ([],[])
mapAndUnzipUs f (x:xs)
  = f x			`thenUs` \ (r1,	 r2)  ->
    mapAndUnzipUs f xs	`thenUs` \ (rs1, rs2) ->
    returnUs (r1:rs1, r2:rs2)
\end{code}
