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

	incrUnique,			-- Used for renumbering
	initRenumberingUniques,

	-- now all the built-in Uniques (and functions to make them)
	-- [the Oh-So-Wonderful Haskell module system wins again...]
	mkAlphaTyVarUnique,
	mkPrimOpIdUnique,
	mkTupleDataConUnique,
	mkTupleTyConUnique,

	getBuiltinUniques, mkBuiltinUnique,
	mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,

	absentErrorIdKey,	-- alphabetical...
	addrDataConKey,
	addrPrimTyConKey,
	addrTyConKey,
	andandIdKey,
	appendIdKey,
	arrayPrimTyConKey,
	augmentIdKey,
	boolTyConKey,
	boundedClassKey,
	buildDataConKey,
	buildIdKey,
	byteArrayPrimTyConKey,
	cCallableClassKey,
	cReturnableClassKey,
	charDataConKey,
	charPrimTyConKey,
	charTyConKey,
	composeIdKey,
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
	evalClassKey,
	falseDataConKey,
	floatDataConKey,
	floatPrimTyConKey,
	floatTyConKey,
	floatingClassKey,
	foldlIdKey,
	foldrIdKey,
	foreignObjDataConKey,
	foreignObjPrimTyConKey,
	foreignObjTyConKey,
	forkIdKey,
	fractionalClassKey,
	fromEnumClassOpKey,
	fromIntClassOpKey,
	fromIntegerClassOpKey,
	fromRationalClassOpKey,
	funTyConKey,
	functorClassKey,
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
	irrefutPatErrorIdKey,
	ixClassKey,
	lexIdKey,
	liftDataConKey,
	liftTyConKey,
	listTyConKey,
	ltDataConKey,
	mainKey, mainPrimIoKey,
	minusClassOpKey,
	monadClassKey,
	monadPlusClassKey,
	monadZeroClassKey,
	mutableArrayPrimTyConKey,
	mutableByteArrayPrimTyConKey,
	nilDataConKey,
	noDefaultMethodErrorIdKey,
	nonExhaustiveGuardsErrorIdKey,
	nonExplicitMethodErrorIdKey,
	notIdKey,
	numClassKey,
	ordClassKey,
	orderingTyConKey,
	otherwiseIdKey,
	packCStringIdKey,
	parErrorIdKey,
	parIdKey,
	patErrorIdKey,
	primIoTyConKey,
	ratioDataConKey,
	ratioTyConKey,
	rationalTyConKey,
	readClassKey,
	readParenIdKey,
	realClassKey,
	realFloatClassKey,
	realFracClassKey,
	realWorldPrimIdKey,
	realWorldTyConKey,
	recConErrorIdKey,
	recUpdErrorIdKey,
	return2GMPsDataConKey,
	return2GMPsTyConKey,
	returnIntAndGMPDataConKey,
	returnIntAndGMPTyConKey,
	returnMClassOpKey,
	runSTIdKey,
	seqIdKey,
	showClassKey,
	showParenIdKey,
	showSpaceIdKey,
	showStringIdKey,
	stTyConKey,
	stDataConKey,
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
	stateAndForeignObjPrimDataConKey,
	stateAndForeignObjPrimTyConKey,
	stateAndIntPrimDataConKey,
	stateAndIntPrimTyConKey,
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
	synchVarPrimTyConKey,
	thenMClassOpKey,
	toEnumClassOpKey,
	traceIdKey,
	trueDataConKey,
	unpackCString2IdKey,
	unpackCStringAppendIdKey,
	unpackCStringFoldrIdKey,
	unpackCStringIdKey,
	ureadListIdKey,
	ushowListIdKey,
	voidIdKey,
	voidTyConKey,
	wordDataConKey,
	wordPrimTyConKey,
	wordTyConKey,
	zeroClassOpKey
	, copyableIdKey
	, noFollowIdKey
	, parAtAbsIdKey
	, parAtForNowIdKey
	, parAtIdKey
	, parAtRelIdKey
	, parGlobalIdKey
	, parLocalIdKey
	, unboundKey
	, byteArrayTyConKey
	, mutableByteArrayTyConKey
	, allClassKey
    ) where

#if __GLASGOW_HASKELL__ <= 201
import PreludeGlaST
#else
import GlaExts
import ST
#if __GLASGOW_HASKELL__ == 202
import PrelBase ( Char(..) )
#endif
#endif

IMP_Ubiq(){-uitous-}

import Outputable
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
data Unique = MkUnique Int#

class Uniquable a where
    uniqueOf :: a -> Unique
\end{code}

\begin{code}
u2i :: Unique -> FAST_INT
u2i (MkUnique i) = i
\end{code}

Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.

\begin{code}
mkUnique	:: Char -> Int -> Unique	-- Builds a unique from pieces
unpkUnique	:: Unique -> (Char, Int)	-- The reverse

mkUniqueGrimily :: Int# -> Unique		-- A trap-door for UniqSupply

incrUnique	:: Unique -> Unique
\end{code}


\begin{code}
mkUniqueGrimily x = MkUnique x

incrUnique (MkUnique i) = MkUnique (i +# 1#)

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x::Int#)

mkUnique (C# c) (I# i)
  = MkUnique (w2i (((i2w (ord# c)) `shiftL#` (i2w_s 24#)) `or#` (i2w i)))

unpkUnique (MkUnique u)
  = let
	tag = C# (chr# (w2i ((i2w u) `shiftr` (i2w_s 24#))))
	i   = I#  (w2i ((i2w u) `and#` (i2w 16777215#){-``0x00ffffff''-}))
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
instance Uniquable Unique where
    uniqueOf u = u
\end{code}

We do sometimes make strings with @Uniques@ in them:
\begin{code}
pprUnique, pprUnique10 :: Unique -> Doc

pprUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (iToBase62 u)

pprUnique10 uniq	-- in base-10, dudes
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (int u)

finish_ppr tag u pp_u
  = if tag /= 't' -- this is just to make v common tyvars, t1, t2, ...
		  -- come out as a, b, ... (shorter, easier to read)
    then pp_all
    else case u of
	   1 -> char 'a'
	   2 -> char 'b'
	   3 -> char 'c'
	   4 -> char 'd'
	   5 -> char 'e'
	   _ -> pp_all
  where
    pp_all = (<>) (char tag) pp_u

showUnique :: Unique -> FAST_STRING
showUnique uniq = _PK_ (show (pprUnique uniq))

instance Outputable Unique where
    ppr sty u = pprUnique u

instance Text Unique where
    showsPrec p uniq rest = _UNPK_ (showUnique uniq)
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
#if __GLASGOW_HASKELL__ == 201
# define BYTE_ARRAY GHCbase.ByteArray
# define RUN_ST	    GHCbase.runST
# define AND_THEN   >>=
# define AND_THEN_  >>
# define RETURN	    return
#elif __GLASGOW_HASKELL__ >= 202
# define BYTE_ARRAY GlaExts.ByteArray
# define RUN_ST	    ST.runST
# define AND_THEN   >>=
# define AND_THEN_  >>
# define RETURN	    return
#else
# define BYTE_ARRAY _ByteArray
# define RUN_ST	    _runST
# define AND_THEN   `thenStrictlyST`
# define AND_THEN_  `seqStrictlyST`
# define RETURN	    returnStrictlyST
#endif

iToBase62 :: Int -> Doc

iToBase62 n@(I# n#)
  = ASSERT(n >= 0)
    let
	bytes = case chars62 of { BYTE_ARRAY bounds_who_needs_'em bytes -> bytes }
    in
    if n# <# 62# then
	case (indexCharArray# bytes n#) of { c ->
	char (C# c) }
    else
	case (quotRem n 62)		of { (q, I# r#) ->
	case (indexCharArray# bytes r#) of { c  ->
	(<>) (iToBase62 q) (char (C# c)) }}

-- keep this at top level! (bug on 94/10/24 WDP)
chars62 :: BYTE_ARRAY Int
chars62
  = RUN_ST (
	newCharArray (0, 61)	AND_THEN \ ch_array ->
	fill_in ch_array 0 62 "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
				AND_THEN_
	unsafeFreezeByteArray ch_array
    )
  where
    fill_in ch_array i lim str
      | i == lim
      = RETURN ()
      | otherwise
      = writeCharArray ch_array i (str !! i)	AND_THEN_
	fill_in ch_array (i+1) lim str
\end{code}

%************************************************************************
%*									*
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
%*									*
%************************************************************************

Allocation of unique supply characters:
	v,t,u : for renumbering value-, type- and usage- vars.
	other a-z: lower case chars for unique supplies (see Main.lhs)
	B:   builtin
	C-E: pseudo uniques	(used in native-code generator)
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

initRenumberingUniques = (mkUnique 'v' 1, mkUnique 't' 1, mkUnique 'u' 1)

mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,
 mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUnique1 i = mkUnique 'C' i -- used for uniqueOf on Regs
mkPseudoUnique2 i = mkUnique 'D' i -- ditto
mkPseudoUnique3 i = mkUnique 'E' i -- ditto

getBuiltinUniques :: Int -> [Unique]
getBuiltinUniques n = map (mkUnique 'B') [1 .. n]
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
evalClassKey		= mkPreludeClassUnique 4 
floatingClassKey	= mkPreludeClassUnique 5 
fractionalClassKey	= mkPreludeClassUnique 6 
integralClassKey	= mkPreludeClassUnique 7 
monadClassKey		= mkPreludeClassUnique 8 
monadZeroClassKey	= mkPreludeClassUnique 9 
monadPlusClassKey	= mkPreludeClassUnique 10
functorClassKey		= mkPreludeClassUnique 11
numClassKey		= mkPreludeClassUnique 12
ordClassKey		= mkPreludeClassUnique 13
readClassKey		= mkPreludeClassUnique 14
realClassKey		= mkPreludeClassUnique 15
realFloatClassKey	= mkPreludeClassUnique 16
realFracClassKey	= mkPreludeClassUnique 17
showClassKey		= mkPreludeClassUnique 18
					       
cCallableClassKey	= mkPreludeClassUnique 19
cReturnableClassKey	= mkPreludeClassUnique 20

ixClassKey		= mkPreludeClassUnique 21
allClassKey		= mkPreludeClassUnique 22	-- Pseudo class used for universal quantification
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
foreignObjPrimTyConKey			= mkPreludeTyConUnique 20
foreignObjTyConKey			= mkPreludeTyConUnique 21
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
stateAndForeignObjPrimTyConKey		= mkPreludeTyConUnique 40
stateAndMutableArrayPrimTyConKey	= mkPreludeTyConUnique 41
stateAndMutableByteArrayPrimTyConKey	= mkPreludeTyConUnique 42
stateAndSynchVarPrimTyConKey	    	= mkPreludeTyConUnique 43
stateAndPtrPrimTyConKey			= mkPreludeTyConUnique 44
stateAndStablePtrPrimTyConKey		= mkPreludeTyConUnique 45
stateAndWordPrimTyConKey		= mkPreludeTyConUnique 46
statePrimTyConKey			= mkPreludeTyConUnique 47
stateTyConKey				= mkPreludeTyConUnique 48
mutableByteArrayTyConKey		= mkPreludeTyConUnique 49
stTyConKey				= mkPreludeTyConUnique 50
primIoTyConKey				= mkPreludeTyConUnique 51
byteArrayTyConKey			= mkPreludeTyConUnique 52
wordPrimTyConKey			= mkPreludeTyConUnique 53
wordTyConKey				= mkPreludeTyConUnique 54
voidTyConKey				= mkPreludeTyConUnique 55
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
foreignObjDataConKey			= mkPreludeDataConUnique 15
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
stateAndForeignObjPrimDataConKey	= mkPreludeDataConUnique 32
stateAndMutableArrayPrimDataConKey	= mkPreludeDataConUnique 33
stateAndMutableByteArrayPrimDataConKey	= mkPreludeDataConUnique 34
stateAndSynchVarPrimDataConKey	    	= mkPreludeDataConUnique 35
stateAndPtrPrimDataConKey		= mkPreludeDataConUnique 36
stateAndStablePtrPrimDataConKey		= mkPreludeDataConUnique 37
stateAndWordPrimDataConKey		= mkPreludeDataConUnique 38
stateDataConKey				= mkPreludeDataConUnique 39
trueDataConKey				= mkPreludeDataConUnique 40
wordDataConKey				= mkPreludeDataConUnique 41
stDataConKey				= mkPreludeDataConUnique 42
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
%*									*
%************************************************************************

\begin{code}
absentErrorIdKey	      = mkPreludeMiscIdUnique  1
andandIdKey		      = mkPreludeMiscIdUnique  2
appendIdKey 		      = mkPreludeMiscIdUnique  3
augmentIdKey		      = mkPreludeMiscIdUnique  4
buildIdKey		      = mkPreludeMiscIdUnique  5
composeIdKey		      = mkPreludeMiscIdUnique  6
errorIdKey		      = mkPreludeMiscIdUnique  7
foldlIdKey		      = mkPreludeMiscIdUnique  8
foldrIdKey		      = mkPreludeMiscIdUnique  9
forkIdKey   	    	      = mkPreludeMiscIdUnique 10
int2IntegerIdKey	      = mkPreludeMiscIdUnique 11
integerMinusOneIdKey	      = mkPreludeMiscIdUnique 12
integerPlusOneIdKey	      = mkPreludeMiscIdUnique 13
integerPlusTwoIdKey	      = mkPreludeMiscIdUnique 14
integerZeroIdKey	      = mkPreludeMiscIdUnique 15
irrefutPatErrorIdKey	      = mkPreludeMiscIdUnique 16
lexIdKey		      = mkPreludeMiscIdUnique 17
noDefaultMethodErrorIdKey     = mkPreludeMiscIdUnique 20
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 21
nonExplicitMethodErrorIdKey   = mkPreludeMiscIdUnique 22
notIdKey		      = mkPreludeMiscIdUnique 23
packCStringIdKey	      = mkPreludeMiscIdUnique 24
parErrorIdKey		      = mkPreludeMiscIdUnique 25
parIdKey		      = mkPreludeMiscIdUnique 26
patErrorIdKey		      = mkPreludeMiscIdUnique 27
readParenIdKey		      = mkPreludeMiscIdUnique 28
realWorldPrimIdKey	      = mkPreludeMiscIdUnique 29
recConErrorIdKey	      = mkPreludeMiscIdUnique 30
recUpdErrorIdKey	      = mkPreludeMiscIdUnique 31
runSTIdKey		      = mkPreludeMiscIdUnique 32
seqIdKey		      = mkPreludeMiscIdUnique 33
showParenIdKey		      = mkPreludeMiscIdUnique 34
showSpaceIdKey		      = mkPreludeMiscIdUnique 35
showStringIdKey		      = mkPreludeMiscIdUnique 36
traceIdKey		      = mkPreludeMiscIdUnique 37
unpackCString2IdKey	      = mkPreludeMiscIdUnique 38
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 39
unpackCStringFoldrIdKey	      = mkPreludeMiscIdUnique 40
unpackCStringIdKey	      = mkPreludeMiscIdUnique 41
voidIdKey		      = mkPreludeMiscIdUnique 42
ushowListIdKey		      = mkPreludeMiscIdUnique 43
ureadListIdKey		      = mkPreludeMiscIdUnique 44

copyableIdKey		= mkPreludeMiscIdUnique 45
noFollowIdKey		= mkPreludeMiscIdUnique 46
parAtAbsIdKey		= mkPreludeMiscIdUnique 47
parAtForNowIdKey	= mkPreludeMiscIdUnique 48
parAtIdKey		= mkPreludeMiscIdUnique 49
parAtRelIdKey		= mkPreludeMiscIdUnique 50
parGlobalIdKey		= mkPreludeMiscIdUnique 51
parLocalIdKey		= mkPreludeMiscIdUnique 52
\end{code}

Certain class operations from Prelude classes.  They get
their own uniques so we can look them up easily when we want
to conjure them up during type checking.        
\begin{code}					  
fromIntClassOpKey	= mkPreludeMiscIdUnique 53
fromIntegerClassOpKey	= mkPreludeMiscIdUnique 54
minusClassOpKey		= mkPreludeMiscIdUnique 69
fromRationalClassOpKey	= mkPreludeMiscIdUnique 55
enumFromClassOpKey	= mkPreludeMiscIdUnique 56
enumFromThenClassOpKey	= mkPreludeMiscIdUnique 57
enumFromToClassOpKey	= mkPreludeMiscIdUnique 58
enumFromThenToClassOpKey= mkPreludeMiscIdUnique 59
eqClassOpKey		= mkPreludeMiscIdUnique 60
geClassOpKey		= mkPreludeMiscIdUnique 61
zeroClassOpKey		= mkPreludeMiscIdUnique 62
thenMClassOpKey		= mkPreludeMiscIdUnique 63 -- (>>=)
unboundKey		= mkPreludeMiscIdUnique 64	-- Just a place holder for unbound
							-- variables produced by the renamer
fromEnumClassOpKey	= mkPreludeMiscIdUnique 65

mainKey			= mkPreludeMiscIdUnique 66
mainPrimIoKey		= mkPreludeMiscIdUnique 67
returnMClassOpKey	= mkPreludeMiscIdUnique 68
-- Used for minusClassOp			69
otherwiseIdKey		= mkPreludeMiscIdUnique 70
toEnumClassOpKey	= mkPreludeMiscIdUnique 71
\end{code}
