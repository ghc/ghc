

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
module Unique (
	Unique, Uniquable(..),
	u2i,				-- hack: used in UniqFM

	pprUnique, pprUnique10, showUnique,

	mkUnique,			-- Used in UniqSupply
	mkUniqueGrimily,		-- Used in UniqSupply only!

	incrUnique,			-- Used for renumbering
	initTyVarUnique, mkTyVarUnique,
	initTidyUniques,

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
	assertIdKey,
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
	coerceIdKey,
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
	inlineIdKey,
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
	mainKey,
	minusClassOpKey,
	monadClassKey,
	monadPlusClassKey,
	monadZeroClassKey,
	mutableArrayPrimTyConKey,
	mutableByteArrayPrimTyConKey,
	nilDataConKey,
	noMethodBindingErrorIdKey,
	nonExhaustiveGuardsErrorIdKey,
	notIdKey,
	numClassKey,
	ordClassKey,
	orderingTyConKey,
	otherwiseIdKey,
	packCStringIdKey,
	parErrorIdKey,
	parIdKey,
	patErrorIdKey,
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
	recSelErrIdKey,
	recUpdErrorIdKey,
	return2GMPsDataConKey,
	return2GMPsTyConKey,
	returnIntAndGMPDataConKey,
	returnIntAndGMPTyConKey,
	returnMClassOpKey,
	seqIdKey,
	showClassKey,
	showParenIdKey,
	showSpaceIdKey,
	showStringIdKey,
	stTyConKey,
	stDataConKey,
	ioTyConKey,
	ioDataConKey,
	ioResultTyConKey,
	ioOkDataConKey,
	ioFailDataConKey,
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
	stRetDataConKey,
	statePrimTyConKey,
	stateTyConKey,
	stRetTyConKey,
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

#include "HsVersions.h"

import FastString	( uniqueOfFS )
import GlaExts
import ST
import PrelBase ( Char(..), chr, ord )

import Outputable
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
\subsection[Uniquable-class]{The @Uniquable@ class}
%*									*
%************************************************************************

\begin{code}
class Uniquable a where
    uniqueOf :: a -> Unique

instance Uniquable FastString where
 uniqueOf fs = mkUniqueGrimily (uniqueOfFS fs)

instance Uniquable Int where
 uniqueOf (I# i#) = mkUniqueGrimily i#
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
  = if u1 ==# u2 then EQ else if u1 <# u2 then LT else GT

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
    compare a b = cmpUnique a b

-----------------
instance Uniquable Unique where
    uniqueOf u = u
\end{code}

We do sometimes make strings with @Uniques@ in them:
\begin{code}
pprUnique, pprUnique10 :: Unique -> SDoc

pprUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (iToBase62 u)

pprUnique10 uniq	-- in base-10, dudes
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (int u)

finish_ppr 't' u pp_u | u < 26
  =	-- Special case to make v common tyvars, t1, t2, ...
	-- come out as a, b, ... (shorter, easier to read)
    char (chr (ord 'a' + u))
finish_ppr tag u pp_u = char tag <> pp_u

showUnique :: Unique -> String
showUnique uniq = showSDoc (pprUnique uniq)

instance Outputable Unique where
    ppr u = pprUnique u

instance Text Unique where
    showsPrec p uniq rest = showUnique uniq
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
# define BYTE_ARRAY GlaExts.ByteArray
# define RUN_ST	    ST.runST
# define AND_THEN   >>=
# define AND_THEN_  >>
# define RETURN	    return

iToBase62 :: Int -> SDoc

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

-- The "tyvar uniques" print specially nicely: a, b, c, etc.
-- See pprUnique for details

initTyVarUnique :: Unique
initTyVarUnique = mkUnique 't' 0

mkTyVarUnique :: Int -> Unique
mkTyVarUnique n = mkUnique 't' n

initTidyUniques :: (Unique, Unique)	-- Global and local
initTidyUniques = (mkUnique 'g' 0, mkUnique 'x' 0)

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
intPrimTyConKey				= mkPreludeTyConUnique 14
intTyConKey				= mkPreludeTyConUnique 15
integerTyConKey				= mkPreludeTyConUnique 16
liftTyConKey				= mkPreludeTyConUnique 17
listTyConKey				= mkPreludeTyConUnique 18
foreignObjPrimTyConKey			= mkPreludeTyConUnique 19
foreignObjTyConKey			= mkPreludeTyConUnique 20
mutableArrayPrimTyConKey		= mkPreludeTyConUnique 21
mutableByteArrayPrimTyConKey		= mkPreludeTyConUnique 22
orderingTyConKey			= mkPreludeTyConUnique 23
synchVarPrimTyConKey		    	= mkPreludeTyConUnique 24
ratioTyConKey				= mkPreludeTyConUnique 25
rationalTyConKey			= mkPreludeTyConUnique 26
realWorldTyConKey			= mkPreludeTyConUnique 27
return2GMPsTyConKey			= mkPreludeTyConUnique 28
returnIntAndGMPTyConKey			= mkPreludeTyConUnique 29
stablePtrPrimTyConKey			= mkPreludeTyConUnique 30
stablePtrTyConKey			= mkPreludeTyConUnique 31
stateAndAddrPrimTyConKey		= mkPreludeTyConUnique 32
stateAndArrayPrimTyConKey		= mkPreludeTyConUnique 33
stateAndByteArrayPrimTyConKey		= mkPreludeTyConUnique 34
stateAndCharPrimTyConKey		= mkPreludeTyConUnique 35
stateAndDoublePrimTyConKey		= mkPreludeTyConUnique 36
stateAndFloatPrimTyConKey		= mkPreludeTyConUnique 37
stateAndIntPrimTyConKey			= mkPreludeTyConUnique 38
stateAndForeignObjPrimTyConKey		= mkPreludeTyConUnique 39
stateAndMutableArrayPrimTyConKey	= mkPreludeTyConUnique 40
stateAndMutableByteArrayPrimTyConKey	= mkPreludeTyConUnique 41
stateAndSynchVarPrimTyConKey	    	= mkPreludeTyConUnique 42
stateAndPtrPrimTyConKey			= mkPreludeTyConUnique 43
stateAndStablePtrPrimTyConKey		= mkPreludeTyConUnique 44
stateAndWordPrimTyConKey		= mkPreludeTyConUnique 45
statePrimTyConKey			= mkPreludeTyConUnique 46
stateTyConKey				= mkPreludeTyConUnique 47
mutableByteArrayTyConKey		= mkPreludeTyConUnique 48
stTyConKey				= mkPreludeTyConUnique 49
stRetTyConKey				= mkPreludeTyConUnique 50
ioTyConKey				= mkPreludeTyConUnique 51
ioResultTyConKey			= mkPreludeTyConUnique 52
byteArrayTyConKey			= mkPreludeTyConUnique 53
wordPrimTyConKey			= mkPreludeTyConUnique 54
wordTyConKey				= mkPreludeTyConUnique 55
voidTyConKey				= mkPreludeTyConUnique 56
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
stRetDataConKey				= mkPreludeDataConUnique 43
ioDataConKey				= mkPreludeDataConUnique 44
ioOkDataConKey				= mkPreludeDataConUnique 45
ioFailDataConKey			= mkPreludeDataConUnique 46
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
recSelErrIdKey		      = mkPreludeMiscIdUnique 11
integerMinusOneIdKey	      = mkPreludeMiscIdUnique 12
integerPlusOneIdKey	      = mkPreludeMiscIdUnique 13
integerPlusTwoIdKey	      = mkPreludeMiscIdUnique 14
integerZeroIdKey	      = mkPreludeMiscIdUnique 15
irrefutPatErrorIdKey	      = mkPreludeMiscIdUnique 16
lexIdKey		      = mkPreludeMiscIdUnique 17
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 20
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 21
notIdKey		      = mkPreludeMiscIdUnique 22
packCStringIdKey	      = mkPreludeMiscIdUnique 23
parErrorIdKey		      = mkPreludeMiscIdUnique 24
parIdKey		      = mkPreludeMiscIdUnique 25
patErrorIdKey		      = mkPreludeMiscIdUnique 26
readParenIdKey		      = mkPreludeMiscIdUnique 27
realWorldPrimIdKey	      = mkPreludeMiscIdUnique 28
recConErrorIdKey	      = mkPreludeMiscIdUnique 29
recUpdErrorIdKey	      = mkPreludeMiscIdUnique 30
seqIdKey		      = mkPreludeMiscIdUnique 31
showParenIdKey		      = mkPreludeMiscIdUnique 32
showSpaceIdKey		      = mkPreludeMiscIdUnique 33
showStringIdKey		      = mkPreludeMiscIdUnique 34
traceIdKey		      = mkPreludeMiscIdUnique 35
unpackCString2IdKey	      = mkPreludeMiscIdUnique 36
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 37
unpackCStringFoldrIdKey	      = mkPreludeMiscIdUnique 38
unpackCStringIdKey	      = mkPreludeMiscIdUnique 39
voidIdKey		      = mkPreludeMiscIdUnique 40
ushowListIdKey		      = mkPreludeMiscIdUnique 41
ureadListIdKey		      = mkPreludeMiscIdUnique 42

copyableIdKey		= mkPreludeMiscIdUnique 43
noFollowIdKey		= mkPreludeMiscIdUnique 44
parAtAbsIdKey		= mkPreludeMiscIdUnique 45
parAtForNowIdKey	= mkPreludeMiscIdUnique 46
parAtIdKey		= mkPreludeMiscIdUnique 47
parAtRelIdKey		= mkPreludeMiscIdUnique 48
parGlobalIdKey		= mkPreludeMiscIdUnique 49
parLocalIdKey		= mkPreludeMiscIdUnique 50
\end{code}

Certain class operations from Prelude classes.  They get
their own uniques so we can look them up easily when we want
to conjure them up during type checking.        
\begin{code}					  
fromIntClassOpKey	= mkPreludeMiscIdUnique 51
fromIntegerClassOpKey	= mkPreludeMiscIdUnique 52
minusClassOpKey		= mkPreludeMiscIdUnique 53
fromRationalClassOpKey	= mkPreludeMiscIdUnique 54
enumFromClassOpKey	= mkPreludeMiscIdUnique 55
enumFromThenClassOpKey	= mkPreludeMiscIdUnique 56
enumFromToClassOpKey	= mkPreludeMiscIdUnique 57
enumFromThenToClassOpKey= mkPreludeMiscIdUnique 58
eqClassOpKey		= mkPreludeMiscIdUnique 59
geClassOpKey		= mkPreludeMiscIdUnique 60
zeroClassOpKey		= mkPreludeMiscIdUnique 61
thenMClassOpKey		= mkPreludeMiscIdUnique 62 -- (>>=)
unboundKey		= mkPreludeMiscIdUnique 63	-- Just a place holder for unbound
							-- variables produced by the renamer
fromEnumClassOpKey	= mkPreludeMiscIdUnique 64

mainKey			= mkPreludeMiscIdUnique 65
returnMClassOpKey	= mkPreludeMiscIdUnique 66
otherwiseIdKey		= mkPreludeMiscIdUnique 67
toEnumClassOpKey	= mkPreludeMiscIdUnique 68
\end{code}

\begin{code}
inlineIdKey		= mkPreludeMiscIdUnique 69
coerceIdKey		= mkPreludeMiscIdUnique 70
assertIdKey		= mkPreludeMiscIdUnique 71
\end{code}
