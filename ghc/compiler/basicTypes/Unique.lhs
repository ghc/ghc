%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

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

	pprUnique, pprUnique10,

	mkUnique,			-- Used in UniqSupply
	mkUniqueGrimily,		-- Used in UniqSupply only!
	getKey,				-- Used in Var only!

	incrUnique,			-- Used for renumbering
	initTyVarUnique,
	initTidyUniques,

	-- now all the built-in Uniques (and functions to make them)
	-- [the Oh-So-Wonderful Haskell module system wins again...]
	mkAlphaTyVarUnique,
	mkPrimOpIdUnique,
	mkTupleDataConUnique,
	mkUbxTupleDataConUnique,
	mkTupleTyConUnique,
	mkUbxTupleTyConUnique,

	getBuiltinUniques, mkBuiltinUnique,
	mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,

	absentErrorIdKey,	-- alphabetical...
	addrDataConKey,
	addrPrimTyConKey,
	addrTyConKey,
	appendIdKey,
	arrayPrimTyConKey,
	assertIdKey,
	augmentIdKey,
	boolTyConKey,
	boundedClassKey,
	boxedConKey,
	buildIdKey,
	byteArrayPrimTyConKey,
	cCallableClassKey,
	cReturnableClassKey,
	charDataConKey,
	charPrimTyConKey,
	charTyConKey,
	concatIdKey,
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
	errorIdKey,
	falseDataConKey,
	filterIdKey,
	floatDataConKey,
	floatPrimTyConKey,
	floatTyConKey,
	floatingClassKey,
	foldlIdKey,
	foldrIdKey,
	foreignObjDataConKey,
	foreignObjPrimTyConKey,
	foreignObjTyConKey,
	weakPrimTyConKey,
	fractionalClassKey,
	fromEnumClassOpKey,
	fromIntClassOpKey,
	fromIntegerClassOpKey,
	fromRationalClassOpKey,
	funTyConKey,
	functorClassKey,
	geClassOpKey,
	intDataConKey,
	intPrimTyConKey,
	intTyConKey,
	int8TyConKey,
	int8DataConKey,
	int16TyConKey,
	int16DataConKey,
	int32TyConKey,
	int32DataConKey,
	int64DataConKey,
	int64PrimTyConKey,
	int64TyConKey,
	integerDataConKey,
	integerMinusOneIdKey,
	integerPlusOneIdKey,
	integerPlusTwoIdKey,
	int2IntegerIdKey,
	addr2IntegerIdKey,
	integerTyConKey,
	integerZeroIdKey,
	integralClassKey,
	irrefutPatErrorIdKey,
	ixClassKey,
	listTyConKey,
	mainKey,
	mapIdKey,
	minusClassOpKey,
	monadClassKey,
	monadPlusClassKey,
	monadZeroClassKey,
	mutableArrayPrimTyConKey,
	mutableByteArrayPrimTyConKey,
	mutVarPrimTyConKey,
	nilDataConKey,
	noMethodBindingErrorIdKey,
	nonExhaustiveGuardsErrorIdKey,
	numClassKey,
	anyBoxConKey,
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
	realClassKey,
	realFloatClassKey,
	realFracClassKey,
	realWorldPrimIdKey,
	realWorldTyConKey,
	recConErrorIdKey,
	recSelErrIdKey,
	recUpdErrorIdKey,
	returnMClassOpKey,
	showClassKey,
	ioTyConKey,
	ioDataConKey,
	stablePtrDataConKey,
	stablePtrPrimTyConKey,
	stablePtrTyConKey,
	stateDataConKey,
	stateTyConKey,

	statePrimTyConKey,
	typeConKey,
	kindConKey,
	boxityConKey,
	mVarPrimTyConKey,
	thenMClassOpKey,
	threadIdPrimTyConKey,
	toEnumClassOpKey,
	traceIdKey,
	trueDataConKey,
	unboxedConKey,
	unpackCString2IdKey,
	unpackCStringAppendIdKey,
	unpackCStringFoldrIdKey,
	unpackCStringIdKey,
	unsafeCoerceIdKey,
	ushowListIdKey,
	voidIdKey,
	voidTyConKey,
	wordDataConKey,
	wordPrimTyConKey,
	wordTyConKey,
	word8TyConKey,
	word8DataConKey,
	word16TyConKey,
	word16DataConKey,
	word32TyConKey,
	word32DataConKey,
	word64DataConKey,
	word64PrimTyConKey,
	word64TyConKey,
	zeroClassOpKey,
	zipIdKey,
	bindIOIdKey,
	deRefStablePtrIdKey,
	makeStablePtrIdKey,
	unboundKey,
	byteArrayTyConKey,
	mutableByteArrayTyConKey
    ) where

#include "HsVersions.h"

import FastString	( FastString, uniqueOfFS )
import GlaExts
import ST
import PrelBase ( Char(..), chr, ord )

import Outputable
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

getKey		:: Unique -> Int#		-- for Var

incrUnique	:: Unique -> Unique
\end{code}


\begin{code}
mkUniqueGrimily x = MkUnique x

{-# INLINE getKey #-}
getKey (MkUnique x) = x

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
    shiftr x y = shiftRL# x y
\end{code}



%************************************************************************
%*									*
\subsection[Uniquable-class]{The @Uniquable@ class}
%*									*
%************************************************************************

\begin{code}
class Uniquable a where
    getUnique :: a -> Unique

instance Uniquable FastString where
 getUnique fs = mkUniqueGrimily (uniqueOfFS fs)

instance Uniquable Int where
 getUnique (I# i#) = mkUniqueGrimily i#
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
    getUnique u = u
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

instance Outputable Unique where
    ppr u = pprUnique u

instance Show Unique where
    showsPrec p uniq = showsPrecSDoc p (pprUnique uniq)
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
	0-9: prelude things below

\begin{code}
mkAlphaTyVarUnique i            = mkUnique '1' i

mkPreludeClassUnique i		= mkUnique '2' i
mkPreludeTyConUnique i		= mkUnique '3' i
mkTupleTyConUnique a		= mkUnique '4' a
mkUbxTupleTyConUnique a		= mkUnique '5' a

mkPreludeDataConUnique i	= mkUnique '6' i -- must be alphabetic
mkTupleDataConUnique a		= mkUnique '7' a -- ditto (*may* be used in C labels)
mkUbxTupleDataConUnique a	= mkUnique '8' a

mkPrimOpIdUnique op		= mkUnique '9' op
mkPreludeMiscIdUnique i		= mkUnique '0' i

-- The "tyvar uniques" print specially nicely: a, b, c, etc.
-- See pprUnique for details

initTyVarUnique :: Unique
initTyVarUnique = mkUnique 't' 0

initTidyUniques :: (Unique, Unique)	-- Global and local
initTidyUniques = (mkUnique 'g' 0, mkUnique 'x' 0)

mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3,
 mkBuiltinUnique :: Int -> Unique

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUnique1 i = mkUnique 'C' i -- used for getUnique on Regs
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
stateTyConKey			        = mkPreludeTyConUnique 50
statePrimTyConKey			= mkPreludeTyConUnique 51
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
voidTyConKey				= mkPreludeTyConUnique 64
boxedConKey				= mkPreludeTyConUnique 65
unboxedConKey				= mkPreludeTyConUnique 66
anyBoxConKey				= mkPreludeTyConUnique 67
kindConKey				= mkPreludeTyConUnique 68
boxityConKey				= mkPreludeTyConUnique 69
typeConKey				= mkPreludeTyConUnique 70
threadIdPrimTyConKey			= mkPreludeTyConUnique 71
\end{code}

%************************************************************************
%*									*
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
%*									*
%************************************************************************

\begin{code}
addrDataConKey				= mkPreludeDataConUnique  1
charDataConKey				= mkPreludeDataConUnique  2
consDataConKey				= mkPreludeDataConUnique  3
doubleDataConKey			= mkPreludeDataConUnique  4
falseDataConKey				= mkPreludeDataConUnique  5
floatDataConKey				= mkPreludeDataConUnique  6
intDataConKey				= mkPreludeDataConUnique  7
int8DataConKey				= mkPreludeDataConUnique  8
int16DataConKey				= mkPreludeDataConUnique  9
int32DataConKey				= mkPreludeDataConUnique 10
int64DataConKey				= mkPreludeDataConUnique 11
integerDataConKey			= mkPreludeDataConUnique 12
foreignObjDataConKey			= mkPreludeDataConUnique 13
nilDataConKey				= mkPreludeDataConUnique 14
ratioDataConKey				= mkPreludeDataConUnique 15
stablePtrDataConKey			= mkPreludeDataConUnique 16
stateDataConKey				= mkPreludeDataConUnique 33
trueDataConKey				= mkPreludeDataConUnique 34
wordDataConKey				= mkPreludeDataConUnique 35
word8DataConKey				= mkPreludeDataConUnique 36
word16DataConKey			= mkPreludeDataConUnique 37
word32DataConKey			= mkPreludeDataConUnique 38
word64DataConKey			= mkPreludeDataConUnique 39
stDataConKey				= mkPreludeDataConUnique 40
ioDataConKey				= mkPreludeDataConUnique 42
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
addr2IntegerIdKey	      = mkPreludeMiscIdUnique 14
irrefutPatErrorIdKey	      = mkPreludeMiscIdUnique 15
lexIdKey		      = mkPreludeMiscIdUnique 16
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 17
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 18
packCStringIdKey	      = mkPreludeMiscIdUnique 19
parErrorIdKey		      = mkPreludeMiscIdUnique 20
parIdKey		      = mkPreludeMiscIdUnique 21
patErrorIdKey		      = mkPreludeMiscIdUnique 22
realWorldPrimIdKey	      = mkPreludeMiscIdUnique 23
recConErrorIdKey	      = mkPreludeMiscIdUnique 24
recUpdErrorIdKey	      = mkPreludeMiscIdUnique 25
traceIdKey		      = mkPreludeMiscIdUnique 26
unpackCString2IdKey	      = mkPreludeMiscIdUnique 27
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 28
unpackCStringFoldrIdKey	      = mkPreludeMiscIdUnique 29
unpackCStringIdKey	      = mkPreludeMiscIdUnique 30
voidIdKey		      = mkPreludeMiscIdUnique 31
ushowListIdKey		      = mkPreludeMiscIdUnique 32
unsafeCoerceIdKey	      = mkPreludeMiscIdUnique 33
concatIdKey		      = mkPreludeMiscIdUnique 34
filterIdKey		      = mkPreludeMiscIdUnique 35
zipIdKey		      = mkPreludeMiscIdUnique 36
bindIOIdKey		      = mkPreludeMiscIdUnique 37
deRefStablePtrIdKey	      = mkPreludeMiscIdUnique 38
makeStablePtrIdKey	      = mkPreludeMiscIdUnique 39
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
zeroClassOpKey		      = mkPreludeMiscIdUnique 112
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
\end{code}
