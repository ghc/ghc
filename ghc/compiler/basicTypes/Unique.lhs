%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Unique]{The @Unique@ data type}

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

--<mkdependHS:friends> UniqSupply

module Unique (
	Unique, Uniquable(..),
	u2i,				-- hack: used in UniqFM

	pprUnique, pprUnique10, showUnique,

	mkUnique,			-- Used in UniqSupply
	mkUniqueGrimily,		-- Used in UniqSupply only!

	-- now all the built-in Uniques (and functions to make them)
	-- [the Oh-So-Wonderful Haskell module system wins again...]
	mkAlphaTyVarUnique,
	mkPrimOpIdUnique,
	mkTupleDataConUnique,
	mkTupleTyConUnique,

	absentErrorIdKey,	-- alphabetical...
	addrDataConKey,
	addrPrimTyConKey,
	addrTyConKey,
	appendIdKey,
	arrayPrimTyConKey,
	augmentIdKey,
	binaryClassKey,
	boolTyConKey,
	buildDataConKey,
	buildIdKey,
	byteArrayPrimTyConKey,
	cCallableClassKey,
	cReturnableClassKey,
	charDataConKey,
	charPrimTyConKey,
	charTyConKey,
	consDataConKey,
	doubleDataConKey,
	doublePrimTyConKey,
	doubleTyConKey,
	enumClassKey,
	enumFromClassOpKey,
	enumFromThenClassOpKey,
	enumFromThenToClassOpKey,
	enumFromToClassOpKey,
	eqClassKey,
	eqClassOpKey,
	eqDataConKey,
	errorIdKey,
	falseDataConKey,
	floatDataConKey,
	floatPrimTyConKey,
	floatTyConKey,
	floatingClassKey,
	foldlIdKey,
	foldrIdKey,
	forkIdKey,
	fractionalClassKey,
	fromIntClassOpKey,
	fromIntegerClassOpKey,
	fromRationalClassOpKey,
	funTyConKey,
	geClassOpKey,
	gtDataConKey,
	iOTyConKey,
	intDataConKey,
	intPrimTyConKey,
	intTyConKey,
	integerDataConKey,
	integerMinusOneIdKey,
	integerPlusOneIdKey,
	integerPlusTwoIdKey,
	integerTyConKey,
	integerZeroIdKey,
	integralClassKey,
	ixClassKey,
	liftDataConKey,
	liftTyConKey,
	listTyConKey,
	ltDataConKey,
	mainIdKey,
	mainPrimIOIdKey,
	mallocPtrDataConKey,
	mallocPtrPrimTyConKey,
	mallocPtrTyConKey,
	monadClassKey,
	monadZeroClassKey,
	mutableArrayPrimTyConKey,
	mutableByteArrayPrimTyConKey,
	nilDataConKey,
	numClassKey,
	ordClassKey,
	orderingTyConKey,
	packCStringIdKey,
	parErrorIdKey,
	parIdKey,
	patErrorIdKey,
	primIoTyConKey,
	ratioDataConKey,
	ratioTyConKey,
	rationalTyConKey,
	readClassKey,
	realClassKey,
	realFloatClassKey,
	realFracClassKey,
	realWorldPrimIdKey,
	realWorldTyConKey,
	return2GMPsDataConKey,
	return2GMPsTyConKey,
	returnIntAndGMPDataConKey,
	returnIntAndGMPTyConKey,
	runSTIdKey,
	seqIdKey,
	showClassKey,
	stTyConKey,
	stablePtrDataConKey,
	stablePtrPrimTyConKey,
	stablePtrTyConKey,
	stateAndAddrPrimDataConKey,
	stateAndAddrPrimTyConKey,
	stateAndArrayPrimDataConKey,
	stateAndArrayPrimTyConKey,
	stateAndByteArrayPrimDataConKey,
	stateAndByteArrayPrimTyConKey,
	stateAndCharPrimDataConKey,
	stateAndCharPrimTyConKey,
	stateAndDoublePrimDataConKey,
	stateAndDoublePrimTyConKey,
	stateAndFloatPrimDataConKey,
	stateAndFloatPrimTyConKey,
	stateAndIntPrimDataConKey,
	stateAndIntPrimTyConKey,
	stateAndMallocPtrPrimDataConKey,
	stateAndMallocPtrPrimTyConKey,
	stateAndMutableArrayPrimDataConKey,
	stateAndMutableArrayPrimTyConKey,
	stateAndMutableByteArrayPrimDataConKey,
	stateAndMutableByteArrayPrimTyConKey,
	stateAndPtrPrimDataConKey,
	stateAndPtrPrimTyConKey,
	stateAndStablePtrPrimDataConKey,
	stateAndStablePtrPrimTyConKey,
	stateAndSynchVarPrimDataConKey,
	stateAndSynchVarPrimTyConKey,
	stateAndWordPrimDataConKey,
	stateAndWordPrimTyConKey,
	stateDataConKey,
	statePrimTyConKey,
	stateTyConKey,
	stringTyConKey,
	synchVarPrimTyConKey,
	traceIdKey,
	trueDataConKey,
	unpackCString2IdKey,
	unpackCStringAppendIdKey,
	unpackCStringFoldrIdKey,
	unpackCStringIdKey,
	voidPrimIdKey,
	voidPrimTyConKey,
	wordDataConKey,
	wordPrimTyConKey,
	wordTyConKey
#ifdef GRAN
	, copyableIdKey
	, noFollowIdKey
	, parGlobalIdKey
	, parLocalIdKey
#endif
	-- to make interface self-sufficient
    ) where

import PreludeGlaST

import Ubiq{-uitous-}

import Pretty
import Util
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

data Unique = MkUnique Int#
u2i (MkUnique i) = i
\end{code}

Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.

\begin{code}
mkUnique		 :: Char -> Int -> Unique	-- Builds a unique from pieces
unpkUnique		 :: Unique -> (Char, Int)	-- The reverse

mkUnifiableTyVarUnique	 :: Int -> Unique	-- Injects a subst-array index into the Unique type
unpkUnifiableTyVarUnique :: Unique -> Int	-- The reverse process

mkUniqueGrimily :: Int# -> Unique		-- A trap-door for UniqSupply
\end{code}


\begin{code}
mkUniqueGrimily x = MkUnique x

mkUnifiableTyVarUnique i = mkUnique '_'{-MAGIC CHAR-} i

unpkUnifiableTyVarUnique uniq
  = case (unpkUnique uniq) of { (tag, i) ->
    ASSERT(tag == '_'{-MAGIC CHAR-})
    i }

-- pop the Char in the top 8 bits of the Unique(Supply)

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
  where
    shiftr x y = shiftRA# x y
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
eqUnique (MkUnique u1) (MkUnique u2) = u1 ==# u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <#  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <=# u2

cmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 ==# u2 then EQ_ else if u1 <# u2 then LT_ else GT_

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
    _tagCmp a b = case cmpUnique a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }

instance Ord3 Unique where
    cmp = cmpUnique

-----------------
class Uniquable a where
    uniqueOf :: a -> Unique

instance Uniquable Unique where
    uniqueOf u = u
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

instance Outputable Unique where
    ppr sty u = pprUnique u

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
\end{code}

%************************************************************************
%*									*
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
%*									*
%************************************************************************

Allocation of unique supply characters:
	a-z: lower case chars for unique supplies (see Main.lhs)
	B:   builtin		(see UniqSupply.lhs)
	C-E: pseudo uniques	(see UniqSupply.lhs)
	_:   unifiable tyvars   (above)
	1-8: prelude things below

\begin{code}
mkAlphaTyVarUnique i		= mkUnique '1' i

mkPreludeClassUnique i		= mkUnique '2' i
mkPreludeTyConUnique i		= mkUnique '3' i
mkTupleTyConUnique a		= mkUnique '4' a

mkPreludeDataConUnique i	= mkUnique '5' i 	-- must be alphabetic
mkTupleDataConUnique a		= mkUnique '6' a	-- ditto (*may* be used in C labels)

mkPrimOpIdUnique op		= mkUnique '7' op
mkPreludeMiscIdUnique i		= mkUnique '8' i
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
showClassKey		= mkPreludeClassUnique 12
readClassKey		= mkPreludeClassUnique 13
monadClassKey		= mkPreludeClassUnique 14
monadZeroClassKey	= mkPreludeClassUnique 15
binaryClassKey		= mkPreludeClassUnique 16
cCallableClassKey	= mkPreludeClassUnique 17	
cReturnableClassKey	= mkPreludeClassUnique 18
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
iOTyConKey				= mkPreludeTyConUnique 14
intPrimTyConKey				= mkPreludeTyConUnique 15
intTyConKey				= mkPreludeTyConUnique 16
integerTyConKey				= mkPreludeTyConUnique 17
liftTyConKey				= mkPreludeTyConUnique 18
listTyConKey				= mkPreludeTyConUnique 19
mallocPtrPrimTyConKey			= mkPreludeTyConUnique 20
mallocPtrTyConKey			= mkPreludeTyConUnique 21
mutableArrayPrimTyConKey		= mkPreludeTyConUnique 22
mutableByteArrayPrimTyConKey		= mkPreludeTyConUnique 23
orderingTyConKey			= mkPreludeTyConUnique 24
synchVarPrimTyConKey		    	= mkPreludeTyConUnique 25
ratioTyConKey				= mkPreludeTyConUnique 26
rationalTyConKey			= mkPreludeTyConUnique 27
realWorldTyConKey			= mkPreludeTyConUnique 28
return2GMPsTyConKey			= mkPreludeTyConUnique 29
returnIntAndGMPTyConKey			= mkPreludeTyConUnique 30
stablePtrPrimTyConKey			= mkPreludeTyConUnique 31
stablePtrTyConKey			= mkPreludeTyConUnique 32
stateAndAddrPrimTyConKey		= mkPreludeTyConUnique 33
stateAndArrayPrimTyConKey		= mkPreludeTyConUnique 34
stateAndByteArrayPrimTyConKey		= mkPreludeTyConUnique 35
stateAndCharPrimTyConKey		= mkPreludeTyConUnique 36
stateAndDoublePrimTyConKey		= mkPreludeTyConUnique 37
stateAndFloatPrimTyConKey		= mkPreludeTyConUnique 38
stateAndIntPrimTyConKey			= mkPreludeTyConUnique 39
stateAndMallocPtrPrimTyConKey		= mkPreludeTyConUnique 40
stateAndMutableArrayPrimTyConKey	= mkPreludeTyConUnique 41
stateAndMutableByteArrayPrimTyConKey	= mkPreludeTyConUnique 42
stateAndSynchVarPrimTyConKey	    	= mkPreludeTyConUnique 43
stateAndPtrPrimTyConKey			= mkPreludeTyConUnique 44
stateAndStablePtrPrimTyConKey		= mkPreludeTyConUnique 45
stateAndWordPrimTyConKey		= mkPreludeTyConUnique 46
statePrimTyConKey			= mkPreludeTyConUnique 47
stateTyConKey				= mkPreludeTyConUnique 48
stringTyConKey				= mkPreludeTyConUnique 49
stTyConKey				= mkPreludeTyConUnique 50
primIoTyConKey				= mkPreludeTyConUnique 51
voidPrimTyConKey			= mkPreludeTyConUnique 52
wordPrimTyConKey			= mkPreludeTyConUnique 53
wordTyConKey				= mkPreludeTyConUnique 54
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*									*
%************************************************************************

\begin{code}
addrDataConKey				= mkPreludeDataConUnique  1
buildDataConKey				= mkPreludeDataConUnique  2
charDataConKey				= mkPreludeDataConUnique  4
consDataConKey				= mkPreludeDataConUnique  5
doubleDataConKey			= mkPreludeDataConUnique  6
eqDataConKey				= mkPreludeDataConUnique  7
falseDataConKey				= mkPreludeDataConUnique  8
floatDataConKey				= mkPreludeDataConUnique  9
gtDataConKey				= mkPreludeDataConUnique 10
intDataConKey				= mkPreludeDataConUnique 11
integerDataConKey			= mkPreludeDataConUnique 12
liftDataConKey				= mkPreludeDataConUnique 13
ltDataConKey				= mkPreludeDataConUnique 14
mallocPtrDataConKey			= mkPreludeDataConUnique 15
nilDataConKey				= mkPreludeDataConUnique 18
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
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*									*
%************************************************************************

\begin{code}
absentErrorIdKey	= mkPreludeMiscIdUnique	 1
appendIdKey 		= mkPreludeMiscIdUnique  2
augmentIdKey		= mkPreludeMiscIdUnique	 3
buildIdKey		= mkPreludeMiscIdUnique	 4
errorIdKey		= mkPreludeMiscIdUnique	 5
foldlIdKey		= mkPreludeMiscIdUnique	 6
foldrIdKey		= mkPreludeMiscIdUnique	 7
forkIdKey   	    	= mkPreludeMiscIdUnique  8
int2IntegerIdKey	= mkPreludeMiscIdUnique	 9
integerMinusOneIdKey	= mkPreludeMiscIdUnique	10
integerPlusOneIdKey	= mkPreludeMiscIdUnique	11
integerPlusTwoIdKey	= mkPreludeMiscIdUnique	12
integerZeroIdKey	= mkPreludeMiscIdUnique	13
packCStringIdKey	= mkPreludeMiscIdUnique	14
parErrorIdKey		= mkPreludeMiscIdUnique	15
parIdKey		= mkPreludeMiscIdUnique	16
patErrorIdKey		= mkPreludeMiscIdUnique	17
realWorldPrimIdKey	= mkPreludeMiscIdUnique 18
runSTIdKey		= mkPreludeMiscIdUnique 19
seqIdKey		= mkPreludeMiscIdUnique 20
traceIdKey		= mkPreludeMiscIdUnique	21
unpackCString2IdKey	= mkPreludeMiscIdUnique	22
unpackCStringAppendIdKey= mkPreludeMiscIdUnique	23
unpackCStringFoldrIdKey	= mkPreludeMiscIdUnique 24
unpackCStringIdKey	= mkPreludeMiscIdUnique	25
voidPrimIdKey		= mkPreludeMiscIdUnique	26
mainIdKey		= mkPreludeMiscIdUnique	27
mainPrimIOIdKey		= mkPreludeMiscIdUnique	28

#ifdef GRAN
parLocalIdKey		= mkPreludeMiscIdUnique 29
parGlobalIdKey		= mkPreludeMiscIdUnique 30
noFollowIdKey		= mkPreludeMiscIdUnique 31
copyableIdKey		= mkPreludeMiscIdUnique 32
#endif
\end{code}

Certain class operations from Prelude classes.  They get
their own uniques so we can look them up easily when we want
to conjure them up during type checking.        
\begin{code}					  
fromIntClassOpKey	= mkPreludeMiscIdUnique 33
fromIntegerClassOpKey	= mkPreludeMiscIdUnique 34
fromRationalClassOpKey	= mkPreludeMiscIdUnique 35
enumFromClassOpKey	= mkPreludeMiscIdUnique 36
enumFromThenClassOpKey	= mkPreludeMiscIdUnique 37
enumFromToClassOpKey	= mkPreludeMiscIdUnique 38
enumFromThenToClassOpKey= mkPreludeMiscIdUnique 39
eqClassOpKey		= mkPreludeMiscIdUnique 40
geClassOpKey		= mkPreludeMiscIdUnique 41
\end{code}




