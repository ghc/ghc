%
% (c) The AQUA Project, Glasgow University, 1997-1998
%

\section[Int]{Module @Int@}

This code is largely copied from the Hugs library of the same name,
suitably hammered to use unboxed types.

\begin{code}
#include "MachDeps.h"

module Int
	( Int8
	, Int16
	, Int32
	, Int64
	, int8ToInt  -- :: Int8  -> Int
	, intToInt8  -- :: Int   -> Int8
	, int16ToInt -- :: Int16 -> Int
	, intToInt16 -- :: Int   -> Int16
	, int32ToInt -- :: Int32 -> Int
	, intToInt32 -- :: Int   -> Int32

	, intToInt64 -- :: Int   -> Int64
	, int64ToInt -- :: Int64 -> Int

        , integerToInt64 -- :: Integer -> Int64
        , int64ToInteger -- :: Int64   -> Integer

	-- plus Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	--  Show and Bits instances for each of Int8, Int16, Int32 and Int64

	-- The "official" place to get these from is Addr, importing
	-- them from Int is a non-standard thing to do.
	, indexInt8OffAddr
	, indexInt16OffAddr
	, indexInt32OffAddr
	, indexInt64OffAddr
	
	, readInt8OffAddr
	, readInt16OffAddr
	, readInt32OffAddr
	, readInt64OffAddr
	
	, writeInt8OffAddr
	, writeInt16OffAddr
	, writeInt32OffAddr
	, writeInt64OffAddr
	
	, sizeofInt8
	, sizeofInt16
	, sizeofInt32
	, sizeofInt64
	
	-- The "official" place to get these from is Foreign
	, indexInt8OffForeignObj
	, indexInt16OffForeignObj
	, indexInt32OffForeignObj
	, indexInt64OffForeignObj
	, readInt8OffForeignObj
	, readInt16OffForeignObj
	, readInt32OffForeignObj
	, readInt64OffForeignObj
	, writeInt8OffForeignObj
	, writeInt16OffForeignObj
	, writeInt32OffForeignObj
	, writeInt64OffForeignObj
	
	-- non-standard, GHC specific
	, intToWord

	) where

import GlaExts
import Ix
import Bits
import PrelGHC
import CCall
import Numeric ( readDec )
import Word    ( Word32 )
import PrelForeign

-----------------------------------------------------------------------------
-- The "official" coercion functions
-----------------------------------------------------------------------------

int8ToInt  :: Int8  -> Int
intToInt8  :: Int   -> Int8
int16ToInt :: Int16 -> Int
intToInt16 :: Int   -> Int16
int32ToInt :: Int32 -> Int
intToInt32 :: Int   -> Int32

-- And some non-exported ones

int8ToInt16  :: Int8  -> Int16
int8ToInt32  :: Int8  -> Int32
int16ToInt8  :: Int16 -> Int8
int16ToInt32 :: Int16 -> Int32
int32ToInt8  :: Int32 -> Int8
int32ToInt16 :: Int32 -> Int16

int8ToInt16  (I8#  x) = I16# x
int8ToInt32  (I8#  x) = I32# x
int16ToInt8  (I16# x) = I8#  x
int16ToInt32 (I16# x) = I32# x
int32ToInt8  (I32# x) = I8#  x
int32ToInt16 (I32# x) = I16# x

--GHC specific
intToWord :: Int -> Word
intToWord (I# i#) = W# (int2Word# i#)
\end{code}

\subsection[Int8]{The @Int8@ interface}

\begin{code}
data Int8 = I8# Int#
instance CCallable Int8
instance CReturnable Int8

int8ToInt (I8# x) = I# (int8ToInt# x)
int8ToInt# x = if x' <=# 0x7f# then x' else x' -# 0x100#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xff#)

--
-- This doesn't perform any bounds checking
-- on the value it is passed, nor its sign.
-- i.e., show (intToInt8 511) => "-1"
--
intToInt8 (I# x) = I8# (intToInt8# x)
intToInt8# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xff#)

instance Eq  Int8     where 
  (I8# x#) == (I8# y#) = x# ==# y#
  (I8# x#) /= (I8# y#) = x# /=# y#

instance Ord Int8 where 
  compare (I8# x#) (I8# y#) = compareInt# (int8ToInt# x#) (int8ToInt# y#)

compareInt# :: Int# -> Int# -> Ordering
compareInt# x# y#
 | x# <#  y# = LT
 | x# ==# y# = EQ
 | otherwise = GT

instance Num Int8 where
  (I8# x#) + (I8# y#) = I8# (intToInt8# (x# +# y#))
  (I8# x#) - (I8# y#) = I8# (intToInt8# (x# -# y#))
  (I8# x#) * (I8# y#) = I8# (intToInt8# (x# *# y#))
  negate i@(I8# x#) = 
     if x# ==# 0#
      then i
      else I8# (0x100# -# x#)

  abs           = absReal
  signum        = signumReal
  fromInteger (J# a# s# d#)
                = case (integer2Int# a# s# d#) of { i# -> I8# (intToInt8# i#) }
  fromInt       = intToInt8

instance Bounded Int8 where
    minBound = 0x80
    maxBound = 0x7f 

instance Real Int8 where
    toRational x = toInteger x % 1

instance Integral Int8 where
    div x@(I8# x#) y@(I8# y#) = 
       if x > 0 && y < 0	then quotInt8 (x-y-1) y
       else if x < 0 && y > 0	then quotInt8 (x-y+1) y
       else quotInt8 x y
    quot x@(I8# _) y@(I8# y#)
       | y# /=# 0# = x `quotInt8` y
       | otherwise = error "Integral.Int8.quot: divide by 0\n"
    rem x@(I8# _) y@(I8# y#)
       | y# /=# 0#  = x `remInt8` y
       | otherwise  = error "Integral.Int8.rem: divide by 0\n"
    mod x@(I8# x#) y@(I8# y#) =
       if x > 0 && y < 0 || x < 0 && y > 0 then
	  if r/=0 then r+y else 0
       else
	  r
	where r = remInt8 x y
    a@(I8# _) `quotRem` b@(I8# _) = (a `quotInt8` b, a `remInt8` b)
    toInteger i8  = toInteger (int8ToInt i8)
    toInt     i8  = int8ToInt i8

remInt8  (I8# x) (I8# y) = I8# (intToInt8# ((int8ToInt# x) `remInt#` (int8ToInt# y)))
quotInt8 (I8# x) (I8# y) = I8# (intToInt8# ((int8ToInt# x) `quotInt#` (int8ToInt# y)))

instance Ix Int8 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	      | inRange b i = int8ToInt (i - m)
	      | otherwise   = error (showString "Ix{Int8}.index: Index " .
				     showParen True (showsPrec 0 i) .
                                     showString " out of range " $
				     showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int8 where
    toEnum         = intToInt8
    fromEnum       = int8ToInt
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int8)]
			  where last = if d < c then minBound else maxBound

instance Read Int8 where
    readsPrec p s = [ (intToInt8 x,r) | (x,r) <- readsPrec p s ]

instance Show Int8 where
    showsPrec p i8 = showsPrec p (int8ToInt i8)

binop8 :: (Int32 -> Int32 -> a) -> (Int8 -> Int8 -> a)
binop8 op x y = int8ToInt32 x `op` int8ToInt32 y

instance Bits Int8 where
  (I8# x) .&. (I8# y) = I8# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I8# x) .|. (I8# y) = I8# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I8# x) `xor` (I8# y) = I8# (word2Int# ((int2Word# x) `xor#` (int2Word# y)))
  complement (I8# x)    = I8# (word2Int# ((int2Word# x) `xor#` (int2Word# 0xff#)))
  shift (I8# x) i@(I# i#)
	| i > 0     = I8# (intToInt8# (iShiftL# (int8ToInt# x)  i#))
	| otherwise = I8# (intToInt8# (iShiftRA# (int8ToInt# x) (negateInt# i#)))
  i8@(I8# x)  `rotate` (I# i)
        | i ==# 0#    = i8
	| i ># 0#     = 
	     I8# (intToInt8# ( word2Int#  (
	             (int2Word# (iShiftL# (int8ToInt# x) i'))
		             `or#`
                     (int2Word# (iShiftRA# (word2Int# (
		                                (int2Word# x) `and#` 
			                        (int2Word# (0x100# -# pow2# i2))))
			                  i2)))))
	| otherwise = rotate i8 (I# (8# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 7#)
           i2 = 8# -# i'
  bit i         = shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 8
  isSigned _    = True

pow2# :: Int# -> Int#
pow2# x# = iShiftL# 1# x#

pow2_64# :: Int# -> Int64#
pow2_64# x# = word64ToInt64# (shiftL64# (wordToWord64# (int2Word# 1#)) x#)

sizeofInt8 :: Word32
sizeofInt8 = 1
\end{code}

\subsection[Int16]{The @Int16@ interface}

\begin{code}
data Int16  = I16# Int#
instance CCallable Int16
instance CReturnable Int16

int16ToInt (I16# x) = I# (int16ToInt# x)

int16ToInt# x = if x' <=# 0x7fff# then x' else x' -# 0x10000#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xffff#)

intToInt16 (I# x) = I16# (intToInt16# x)
intToInt16# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xffff#)

instance Eq  Int16     where
  (I16# x#) == (I16# y#) = x# ==# y#
  (I16# x#) /= (I16# y#) = x# /=# y#

instance Ord Int16 where
  compare (I16# x#) (I16# y#) = compareInt# (int16ToInt# x#) (int16ToInt# y#)

instance Num Int16 where
  (I16# x#) + (I16# y#) = I16# (intToInt16# (x# +# y#))
  (I16# x#) - (I16# y#) = I16# (intToInt16# (x# -# y#))
  (I16# x#) * (I16# y#) = I16# (intToInt16# (x# *# y#))
  negate i@(I16# x#) = 
     if x# ==# 0#
      then i
      else I16# (0x10000# -# x#)
  abs           = absReal
  signum        = signumReal
  fromInteger (J# a# s# d#)
                = case (integer2Int# a# s# d#) of { i# -> I16# (intToInt16# i#) }
  fromInt       = intToInt16

instance Bounded Int16 where
    minBound = 0x8000
    maxBound = 0x7fff 

instance Real Int16 where
    toRational x = toInteger x % 1

instance Integral Int16 where
    div x@(I16# x#) y@(I16# y#) = 
       if x > 0 && y < 0	then quotInt16 (x-y-1) y
       else if x < 0 && y > 0	then quotInt16 (x-y+1) y
       else quotInt16 x y
    quot x@(I16# _) y@(I16# y#)
       | y# /=# 0#      = x `quotInt16` y
       | otherwise      = error "Integral.Int16.quot: divide by 0\n"
    rem x@(I16# _) y@(I16# y#)
       | y# /=# 0#      = x `remInt16` y
       | otherwise      = error "Integral.Int16.rem: divide by 0\n"
    mod x@(I16# x#) y@(I16# y#) =
       if x > 0 && y < 0 || x < 0 && y > 0 then
	  if r/=0 then r+y else 0
       else
	  r
	where r = remInt16 x y
    a@(I16# _) `quotRem` b@(I16# _) = (a `quotInt16` b, a `remInt16` b)
    toInteger i16  = toInteger (int16ToInt i16)
    toInt     i16  = int16ToInt i16

remInt16  (I16# x) (I16# y) = I16# (intToInt16# ((int16ToInt# x) `remInt#` (int16ToInt# y)))
quotInt16 (I16# x) (I16# y) = I16# (intToInt16# ((int16ToInt# x) `quotInt#` (int16ToInt# y)))

instance Ix Int16 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	      | inRange b i = int16ToInt (i - m)
	      | otherwise   = error (showString "Ix{Int16}.index: Index " .
				     showParen True (showsPrec 0 i) .
                                     showString " out of range " $
				     showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int16 where
    toEnum           = intToInt16
    fromEnum         = int16ToInt
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int16)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int16)]
			  where last = if d < c then minBound else maxBound

instance Read Int16 where
    readsPrec p s = [ (intToInt16 x,r) | (x,r) <- readsPrec p s ]

instance Show Int16 where
    showsPrec p i16 = showsPrec p (int16ToInt i16)

binop16 :: (Int32 -> Int32 -> a) -> (Int16 -> Int16 -> a)
binop16 op x y = int16ToInt32 x `op` int16ToInt32 y

instance Bits Int16 where
  (I16# x) .&. (I16# y) = I16# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I16# x) .|. (I16# y) = I16# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I16# x) `xor` (I16# y) = I16# (word2Int# ((int2Word# x) `xor#`  (int2Word# y)))
  complement (I16# x)    = I16# (word2Int# ((int2Word# x) `xor#` (int2Word# 0xffff#)))
  shift (I16# x) i@(I# i#)
	| i > 0     = I16# (intToInt16# (iShiftL# (int16ToInt# x)  i#))
	| otherwise = I16# (intToInt16# (iShiftRA# (int16ToInt# x) (negateInt# i#)))
  i16@(I16# x)  `rotate` (I# i)
        | i ==# 0#    = i16
	| i ># 0#     = 
	     I16# (intToInt16# (word2Int# (
	            (int2Word# (iShiftL# (int16ToInt# x) i')) 
		             `or#`
                    (int2Word# (iShiftRA# ( word2Int# (
		                    (int2Word# x) `and#` (int2Word# (0x100# -# pow2# i2))))
			                  i2)))))
	| otherwise = rotate i16 (I# (16# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 15#)
           i2 = 16# -# i'
  bit i             = shift 1 i
  setBit x i        = x .|. bit i
  clearBit x i      = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i       = (x .&. bit i) /= 0
  bitSize  _        = 16
  isSigned _        = True

sizeofInt16 :: Word32
sizeofInt16 = 2
\end{code}

%
%
\subsection[Int32]{The @Int32@ interface}
%
%

\begin{code}
data Int32  = I32# Int#
instance CCallable Int32
instance CReturnable Int32

int32ToInt (I32# x) = I# (int32ToInt# x)

int32ToInt# :: Int# -> Int#
#if WORD_SIZE_IN_BYTES > 4
int32ToInt# x = if x' <=# 0x7fffffff# then x' else x' -# 0x100000000#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xffffffff#)
#else
int32ToInt# x = x
#endif

intToInt32 (I# x) = I32# (intToInt32# x)
intToInt32# :: Int# -> Int#
#if WORD_SIZE_IN_BYTES > 4
intToInt32# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xffffffff#)
#else
intToInt32# i# = i#
#endif

instance Eq  Int32     where
  (I32# x#) == (I32# y#) = x# ==# y#
  (I32# x#) /= (I32# y#) = x# /=# y#

instance Ord Int32    where
  compare (I32# x#) (I32# y#) = compareInt# (int32ToInt# x#) (int32ToInt# y#)

instance Num Int32 where
  (I32# x#) + (I32# y#) = I32# (intToInt32# (x# +# y#))
  (I32# x#) - (I32# y#) = I32# (intToInt32# (x# -# y#))
  (I32# x#) * (I32# y#) = I32# (intToInt32# (x# *# y#))
#if WORD_SIZE_IN_BYTES > 4
  negate i@(I32# x)  = 
      if x ==# 0#
       then i
       else I32# (intToInt32# (0x100000000# -# x'))
#else
  negate (I32# x)  = I32# (negateInt# x)
#endif
  abs           = absReal
  signum        = signumReal
  fromInteger (J# a# s# d#)
                = case (integer2Int# a# s# d#) of { i# -> I32# (intToInt32# i#) }
  fromInt       = intToInt32

-- ToDo: remove LitLit when minBound::Int is fixed (currently it's one
-- too high, and doesn't allow the correct minBound to be defined here).
instance Bounded Int32 where 
    minBound = case ``0x80000000'' of { I# x -> I32# x }
    maxBound = I32# 0x7fffffff#

instance Real Int32 where
    toRational x = toInteger x % 1

instance Integral Int32 where
    div x@(I32# x#) y@(I32# y#) = 
       if x > 0 && y < 0	then quotInt32 (x-y-1) y
       else if x < 0 && y > 0	then quotInt32 (x-y+1) y
       else quotInt32 x y
    quot x@(I32# _) y@(I32# y#)
       | y# /=# 0#  = x `quotInt32` y
       | otherwise  = error "Integral.Int32.quot: divide by 0\n"
    rem x@(I32# _) y@(I32# y#)
       | y# /=# 0#  = x `remInt32` y
       | otherwise  = error "Integral.Int32.rem: divide by 0\n"
    mod x@(I32# x#) y@(I32# y#) =
       if x > 0 && y < 0 || x < 0 && y > 0 then
	  if r/=0 then r+y else 0
       else
	  r
	where r = remInt32 x y
    a@(I32# _) `quotRem` b@(I32# _) = (a `quotInt32` b, a `remInt32` b)
    toInteger i32  = toInteger (int32ToInt i32)
    toInt     i32  = int32ToInt i32

remInt32  (I32# x) (I32# y) = I32# (intToInt32# ((int32ToInt# x) `remInt#` (int32ToInt# y)))
quotInt32 (I32# x) (I32# y) = I32# (intToInt32# ((int32ToInt# x) `quotInt#` (int32ToInt# y)))

instance Ix Int32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	      | inRange b i = int32ToInt (i - m)
	      | otherwise   = error (showString "Ix{Int32}.index: Index " .
				     showParen True (showsPrec 0 i) .
                                     showString " out of range " $
				     showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int32 where
    toEnum         = intToInt32
    fromEnum       = int32ToInt
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int32)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int32)]
			  where last = if d < c then minBound else maxBound

instance Read Int32 where
    readsPrec p s = [ (intToInt32 x,r) | (x,r) <- readsPrec p s ]

instance Show Int32 where
    showsPrec p i32 = showsPrec p (int32ToInt i32)

instance Bits Int32 where
  (I32# x) .&. (I32# y)   = I32# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I32# x) .|. (I32# y)   = I32# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I32# x) `xor` (I32# y) = I32# (word2Int# ((int2Word# x) `xor#` (int2Word# y)))
#if WORD_SIZE_IN_BYTES > 4
  complement (I32# x)     = I32# (word2Int# ((int2Word# x) `xor#` (int2Word# 0xffffffff#)))
#else
  complement (I32# x)     = I32# (word2Int# ((int2Word# x) `xor#` (int2Word# (negateInt# 1#))))
#endif
  shift (I32# x) i@(I# i#)
	| i > 0     = I32# (intToInt32# (iShiftL# (int32ToInt# x)  i#))
	| otherwise = I32# (intToInt32# (iShiftRA# (int32ToInt# x) (negateInt# i#)))
  i32@(I32# x)  `rotate` (I# i)
        | i ==# 0#    = i32
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x100000000-2^i2))>>i2)
	     I32# (intToInt32# ( word2Int# (
	            (int2Word# (iShiftL# (int32ToInt# x) i')) 
		          `or#`
                    (int2Word# (iShiftRA# (word2Int# (
		                              (int2Word# x) 
					          `and#` 
			                       (int2Word# (maxBound# -# pow2# i2 +# 1#))))
			                  i2)))))
	| otherwise = rotate i32 (I# (32# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 31#)
           i2 = 32# -# i'
           (I32# maxBound#) = maxBound
  bit i        	= shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 32
  isSigned _    = True

sizeofInt32 :: Word32
sizeofInt32 = 4
\end{code}

\subsection[Int64]{The @Int64@ interface}


\begin{code}
#if WORD_SIZE_IN_BYTES == 8
data Int64 = I64# Int#

int32ToInt64 :: Int32 -> Int64
int32ToInt64 (I32# i#) = I64# i#

intToInt32# :: Int# -> Int#
intToInt32# i# = word2Int# ((int2Word# i#) `and#` (case (maxBound::Word32) of W# x# -> x#))

int64ToInt32 :: Int64 -> Int32
int64ToInt32 (I64# i#) = I32# (intToInt32# w#)

instance Eq  Int64     where 
  (I64# x) == (I64# y) = x `eqInt#` y
  (I64# x) /= (I64# y) = x `neInt#` y

instance Ord Int32    where
  compare (I64# x#) (I64# y#) = compareInt# x# y#

instance Num Int64 where
  (I64# x) + (I64# y) = I64# (x +# y)
  (I64# x) - (I64# y) = I64# (x -# y)
  (I64# x) * (I64# y) = I64# (x *# y)
  negate w@(I64# x)   = I64# (negateInt# x)
  abs x               = absReal
  signum              = signumReal
  fromInteger (J# a# s# d#) = case (integer2Int# a# s# d#) of { i# -> I64# i# }
  fromInt       = intToInt64

instance Bounded Int64 where
  minBound = integerToInt64 (-0x8000000000000000)
  maxBound = integerToInt64 0x7fffffffffffffff

instance Real Int64 where
  toRational x = toInteger x % 1

instance Integral Int64 where
    div x@(I64# x#) y@(I64# y#)
      | x > 0 && y < 0	= quotInt64 (x-y-1) y
      | x < 0 && y > 0	= quotInt64 (x-y+1) y
      | otherwise       = quotInt64 x y

    quot x@(I64# _) y@(I64# y#)
       | y# /=# 0# = x `quotInt64` y
       | otherwise = error "Integral.Int64.quot: divide by 0\n"

    rem x@(I64# _) y@(I64# y#)
       | y# /=# 0# = x `remInt64` y
       | otherwise = error "Integral.Int32.rem: divide by 0\n"

    mod x@(I64# x#) y@(I64# y#)
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt64 x y

    a@(I64# _) `quotRem` b@(I64# _) = (a `quotInt64` b, a `remInt64` b)
    toInteger (I64# i#) = toInteger (I# i#)
    toInt     (I64# i#) = I# i#

instance Enum Int64 where
    toEnum    (I# i)   = I64# i#
    fromEnum  (I64# i) = I64# i#
    enumFrom c         = map toEnum [fromEnum c .. fromEnum (maxBound::Int64)] -- a long list!
    enumFromThen c d   = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int64)]
		       where last = if d < c then minBound else maxBound


instance Read Int64 where
    readsPrec p s = [ (intToInt64 x,r) | (x,r) <- readsPrec p s ]

instance Show Int64 where
    showsPrec p i64 = showsPrec p (int64ToInt i64)

instance Bits Int64 where
  (I64# x) .&. (I64# y)   = I64# (word2Int# ((int2Word# x) `and#` (int2Word# y)))
  (I64# x) .|. (I64# y)   = I64# (word2Int# ((int2Word# x) `or#`  (int2Word# y)))
  (I64# x) `xor` (I64# y) = I64# (word2Int# ((int2Word# x) `xor#` (int2Word# y)))
  complement (I64# x)     = I64# (negateInt# x)
  shift (I64# x) i@(I# i#)
	| i > 0     = I64# (iShiftL# x  i#)
	| otherwise = I64# (iShiftRA# x (negateInt# i#))
  i64@(I64# x)  `rotate` (I# i)
        | i ==# 0#    = i64
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x10000000000000000-2^i2))>>i2) )
	     I64# (word2Int# (
	            (int2Word# (iShiftL# x i')) 
		          `or#`
                    (int2Word# (iShiftRA# (word2Int# (
		                              (int2Word# x) 
					          `and#` 
			                       (int2Word# (maxBound# -# pow2# i2 +# 1#))))
			                  i2))))
	| otherwise = rotate i64 (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (I64# maxBound#) = maxBound
  bit i        	= shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 64
  isSigned _    = True



remInt64  (I64# x) (I64# y) = I64# (x `remInt#` y)
quotInt64 (I64# x) (I64# y) = I64# (x `quotInt#` y)

int64ToInteger :: Int64 -> Integer
int64ToInteger (I64# i#) = toInteger (I# i#)

integerToInt64 :: Integer -> Int64
integerToInt64 i = case fromInteger i of { I# i# -> I64# i# }

intToInt64 :: Int -> Int64
intToInt64 (I# i#) = I64# i#

int64ToInt :: Int64 -> Int
int64ToInt (I64# i#) = I# i#

#else
--assume: support for long-longs
--data Int64 = I64 Int64# deriving (Eq, Ord, Bounded)

int32ToInt64 :: Int32 -> Int64
int32ToInt64 (I32# i#) = I64# (intToInt64# i#)

int64ToInt32 :: Int64 -> Int32
int64ToInt32 (I64# i#) = I32# (int64ToInt# i#)

int64ToInteger :: Int64 -> Integer
int64ToInteger (I64# x#) = int64ToInteger# x#

integerToInt64 :: Integer -> Int64
integerToInt64 (J# a# s# d#) = I64# (integerToInt64# a# s# d#)

instance Show Int64 where
  showsPrec p x = showsPrec p (int64ToInteger x)

instance Read Int64 where
  readsPrec p s = [ (integerToInt64 x,r) | (x,r) <- readDec s ]

instance Eq  Int64     where 
  (I64# x) == (I64# y) = x `eqInt64#` y
  (I64# x) /= (I64# y) = x `neInt64#` y

instance Ord Int64     where 
  compare (I64# x) (I64# y)   = compareInt64# x y
  (<)  (I64# x) (I64# y)      = x `ltInt64#` y
  (<=) (I64# x) (I64# y)      = x `leInt64#` y
  (>=) (I64# x) (I64# y)      = x `geInt64#` y
  (>)  (I64# x) (I64# y)      = x `gtInt64#` y
  max x@(I64# x#) y@(I64# y#) = 
     case (compareInt64# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(I64# x#) y@(I64# y#) =
     case (compareInt64# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Int64 where
  (I64# x) + (I64# y) = I64# (x `plusInt64#`  y)
  (I64# x) - (I64# y) = I64# (x `minusInt64#` y)
  (I64# x) * (I64# y) = I64# (x `timesInt64#` y)
  negate (I64# x)     = I64# (negateInt64# x)
  abs x               = absReal x
  signum              = signumReal
  fromInteger i       = integerToInt64 i
  fromInt     i       = intToInt64 i

compareInt64# i# j# 
 | i# `ltInt64#` j# = LT
 | i# `eqInt64#` j# = EQ
 | otherwise	    = GT

instance Bounded Int64 where
  minBound = integerToInt64 (-0x8000000000000000)
  maxBound = integerToInt64 0x7fffffffffffffff

instance Real Int64 where
  toRational x = toInteger x % 1

instance Integral Int64 where
    div x@(I64# x#) y@(I64# y#)
      | x > 0 && y < 0	= quotInt64 (x-y-1) y
      | x < 0 && y > 0	= quotInt64 (x-y+1) y
      | otherwise       = quotInt64 x y

    quot x@(I64# _) y@(I64# y#)
       | y# `neInt64#` (intToInt64# 0#) = x `quotInt64` y
       | otherwise = error "Integral.Int64.quot: divide by 0\n"

    rem x@(I64# _) y@(I64# y#)
       | y# `neInt64#` (intToInt64# 0#) = x `remInt64` y
       | otherwise = error "Integral.Int32.rem: divide by 0\n"

    mod x@(I64# x#) y@(I64# y#)
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt64 x y

    a@(I64# _) `quotRem` b@(I64# _) = (a `quotInt64` b, a `remInt64` b)
    toInteger i         = int64ToInteger i
    toInt     i         = int64ToInt i

instance Enum Int64 where
    toEnum    (I# i)   = I64# (intToInt64# i)
    fromEnum  (I64# w) = I# (int64ToInt# w)
    enumFrom i              = eft64 i 1
    enumFromTo   i1 i2      = eftt64 i1 1 (> i2)
    enumFromThen i1 i2      = eftt64 i1 (i2 - i1) (>last)
        where 
	 last
	  | i1 < i2   = maxBound::Int64
	  | otherwise = minBound


instance Bits Int64 where
  (I64# x) .&. (I64# y)   = I64# (word64ToInt64# ((int64ToWord64# x) `and64#` (int64ToWord64# y)))
  (I64# x) .|. (I64# y)   = I64# (word64ToInt64# ((int64ToWord64# x) `or64#`  (int64ToWord64# y)))
  (I64# x) `xor` (I64# y) = I64# (word64ToInt64# ((int64ToWord64# x) `xor64#` (int64ToWord64# y)))
  complement (I64# x)     = I64# (negateInt64# x)
  shift (I64# x) i@(I# i#)
	| i > 0     = I64# (iShiftL64# x  i#)
	| otherwise = I64# (iShiftRA64# x (negateInt# i#))
  i64@(I64# x)  `rotate` (I# i)
        | i ==# 0#    = i64
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x10000000000000000-2^i2))>>i2) )
	     I64# (word64ToInt64# (
	            (int64ToWord64# (iShiftL64# x i'))		          `or64#`
                    (int64ToWord64# (iShiftRA64# (word64ToInt64# ((int64ToWord64# x)     `and64#` 
			                         (int64ToWord64# (maxBound# `minusInt64#` (pow2_64# i2 `plusInt64#` (intToInt64# 1#))))))
			                        i2))))
	| otherwise = rotate i64 (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (I64# maxBound#) = maxBound
  bit i        	= shift 1 i
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i   = (x .&. bit i) /= 0
  bitSize  _    = 64
  isSigned _    = True

remInt64  (I64# x) (I64# y) = I64# (x `remInt64#` y)
quotInt64 (I64# x) (I64# y) = I64# (x `quotInt64#` y)

intToInt64 :: Int -> Int64
intToInt64 (I# i#) = I64# (intToInt64# i#)

int64ToInt :: Int64 -> Int
int64ToInt (I64# i#) = I# (int64ToInt# i#)

-- Enum Int64 helper funs:

eftt64 :: Int64 -> Int64 -> (Int64->Bool) -> [Int64]
eftt64 now step done = go now
  where
   go now
     | done now  = []
     | otherwise = now : go (now+step)

eft64 :: Int64 -> Int64 -> [Int64]
eft64 now step = go now
  where 
   go x
    | x == maxBound = [x]
    | otherwise     = x:go (x+step)


-- Word64# primop wrappers:

ltInt64# :: Int64# -> Int64# -> Bool
ltInt64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_ltInt64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
leInt64# :: Int64# -> Int64# -> Bool
leInt64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_leInt64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
eqInt64# :: Int64# -> Int64# -> Bool
eqInt64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_eqInt64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
neInt64# :: Int64# -> Int64# -> Bool
neInt64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_neInt64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
geInt64# :: Int64# -> Int64# -> Bool
geInt64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_geInt64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
gtInt64# :: Int64# -> Int64# -> Bool
gtInt64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_gtInt64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True

plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# a# b# = 
  case (unsafePerformIO (_ccall_ stg_plusInt64 a# b#)) of
    I64# i# -> i#

minusInt64# :: Int64# -> Int64# -> Int64#
minusInt64# a# b# =
  case (unsafePerformIO (_ccall_ stg_minusInt64 a# b#)) of
    I64# i# -> i#

timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# a# b# =
  case (unsafePerformIO (_ccall_ stg_timesInt64 a# b#)) of
    I64# i# -> i#

quotInt64# :: Int64# -> Int64# -> Int64#
quotInt64# a# b# =
  case (unsafePerformIO (_ccall_ stg_quotInt64 a# b#)) of
    I64# i# -> i#

remInt64# :: Int64# -> Int64# -> Int64#
remInt64# a# b# =
  case (unsafePerformIO (_ccall_ stg_remInt64 a# b#)) of
    I64# i# -> i#

negateInt64# :: Int64# -> Int64#
negateInt64# a# =
  case (unsafePerformIO (_ccall_ stg_negateInt64 a#)) of
    I64# i# -> i#

and64# :: Word64# -> Word64# -> Word64#
and64# a# b# =
  case (unsafePerformIO (_ccall_ stg_and64 a# b#)) of
    W64# w# -> w#

or64# :: Word64# -> Word64# -> Word64#
or64# a# b# =
  case (unsafePerformIO (_ccall_ stg_or64 a# b#)) of
    W64# w# -> w#

xor64# :: Word64# -> Word64# -> Word64#
xor64# a# b# = 
  case (unsafePerformIO (_ccall_ stg_xor64 a# b#)) of
    W64# w# -> w#

not64# :: Word64# -> Word64#
not64# a# = 
  case (unsafePerformIO (_ccall_ stg_not64 a#)) of
    W64# w# -> w#

shiftL64# :: Word64# -> Int# -> Word64#
shiftL64# a# b# =
  case (unsafePerformIO (_ccall_ stg_shiftL64 a# b#)) of
    W64# w# -> w#

iShiftL64# :: Int64# -> Int# -> Int64#
iShiftL64# a# b# =
  case (unsafePerformIO (_ccall_ stg_iShiftL64 a# b#)) of
    I64# i# -> i#

iShiftRL64# :: Int64# -> Int# -> Int64#
iShiftRL64# a# b# =
  case (unsafePerformIO (_ccall_ stg_iShiftRL64 a# b#)) of
    I64# i# -> i#

iShiftRA64# :: Int64# -> Int# -> Int64#
iShiftRA64# a# b# =
  case (unsafePerformIO (_ccall_ stg_iShiftRA64 a# b#)) of
    I64# i# -> i#

shiftRL64# :: Word64# -> Int# -> Word64#
shiftRL64# a# b# =
  case (unsafePerformIO (_ccall_ stg_shifRtL64 a# b#)) of
    W64# w# -> w#

int64ToInt# :: Int64# -> Int#
int64ToInt# i# =
  case (unsafePerformIO (_ccall_ stg_int64ToInt i#)) of
    I# i# -> i#

wordToWord64# :: Word# -> Word64#
wordToWord64# w# =
  case (unsafePerformIO (_ccall_ stg_wordToWord64 w#)) of
    W64# w# -> w#

word64ToInt64# :: Word64# -> Int64#
word64ToInt64# w# =
  case (unsafePerformIO (_ccall_ stg_word64ToInt64 w#)) of
    I64# i# -> i#

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# w# =
  case (unsafePerformIO (_ccall_ stg_int64ToWord64 w#)) of
    W64# w# -> w#

intToInt64# :: Int# -> Int64#
intToInt64# i# =
  case (unsafePerformIO (_ccall_ stg_intToInt64 i#)) of
    I64# i# -> i#
      

#endif

sizeofInt64 :: Word32
sizeofInt64 = 8
\end{code}

%
%
\subsection[Int Utils]{Miscellaneous utilities}
%
%

Code copied from the Prelude

\begin{code}
absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}

\begin{code}
indexInt8OffAddr  :: Addr -> Int -> Int8
indexInt8OffAddr (A# a#) (I# i#) = intToInt8 (I# (ord# (indexCharOffAddr# a# i#)))

indexInt8OffForeignObj  :: ForeignObj -> Int -> Int8
indexInt8OffForeignObj (ForeignObj fo#) (I# i#) = intToInt8 (I# (ord# (indexCharOffForeignObj# fo# i#)))

indexInt16OffAddr :: Addr -> Int -> Int16
indexInt16OffAddr a i =
#ifdef WORDS_BIGENDIAN
  intToInt16 ( int8ToInt l + (int8ToInt maxBound) * int8ToInt h)
#else
  intToInt16 ( int8ToInt h + (int8ToInt maxBound) * int8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexInt8OffAddr a byte_idx
   h = indexInt8OffAddr a (byte_idx+1)

indexInt16OffForeignObj :: ForeignObj -> Int -> Int16
indexInt16OffForeignObj fo i =
#ifdef WORDS_BIGENDIAN
  intToInt16 ( int8ToInt l + (int8ToInt maxBound) * int8ToInt h)
#else
  intToInt16 ( int8ToInt h + (int8ToInt maxBound) * int8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexInt8OffForeignObj fo byte_idx
   h = indexInt8OffForeignObj fo (byte_idx+1)

indexInt32OffAddr :: Addr -> Int -> Int32
indexInt32OffAddr (A# a#) i = intToInt32 (I# (indexIntOffAddr# a# i'#))
 where
   -- adjust index to be in Int units, not Int32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexInt32OffForeignObj :: ForeignObj -> Int -> Int32
indexInt32OffForeignObj (ForeignObj fo#) i = intToInt32 (I# (indexIntOffForeignObj# fo# i'#))
 where
   -- adjust index to be in Int units, not Int32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexInt64OffAddr :: Addr -> Int -> Int64
indexInt64OffAddr (A# a#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = I64# (indexIntOffAddr# a# i#)
#else
 = I64# (indexInt64OffAddr# a# i#)
#endif

indexInt64OffForeignObj :: ForeignObj -> Int -> Int64
indexInt64OffForeignObj (ForeignObj fo#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = I64# (indexIntOffForeignObj# fo# i#)
#else
 = I64# (indexInt64OffForeignObj# fo# i#)
#endif

\end{code}

Read words out of mutable memory:

\begin{code}
readInt8OffAddr :: Addr -> Int -> IO Int8
readInt8OffAddr a i = _casm_ `` %r=(StgInt8)(((StgInt8*)%0)[(StgInt)%1]); '' a i

readInt8OffForeignObj :: ForeignObj -> Int -> IO Int8
readInt8OffForeignObj fo i = _casm_ `` %r=(StgInt8)(((StgInt8*)%0)[(StgInt)%1]); '' fo i

readInt16OffAddr  :: Addr -> Int -> IO Int16
readInt16OffAddr a i = _casm_ `` %r=(StgInt16)(((StgInt16*)%0)[(StgInt)%1]); '' a i

readInt16OffForeignObj  :: ForeignObj -> Int -> IO Int16
readInt16OffForeignObj fo i = _casm_ `` %r=(StgInt16)(((StgInt16*)%0)[(StgInt)%1]); '' fo i

readInt32OffAddr  :: Addr -> Int -> IO Int32
readInt32OffAddr a i = _casm_ `` %r=(StgInt32)(((StgInt32*)%0)[(StgInt)%1]); '' a i

readInt32OffForeignObj  :: ForeignObj -> Int -> IO Int32
readInt32OffForeignObj fo i = _casm_ `` %r=(StgInt32)(((StgInt32*)%0)[(StgInt)%1]); '' fo i

readInt64OffAddr  :: Addr -> Int -> IO Int64
#if WORD_SIZE_IN_BYTES==8
readInt64OffAddr a i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' a i
#else
readInt64OffAddr a i = _casm_ `` %r=(StgInt64)(((StgInt64*)%0)[(StgInt)%1]); '' a i
#endif

readInt64OffForeignObj  :: ForeignObj -> Int -> IO Int64
#if WORD_SIZE_IN_BYTES==8
readInt64OffForeignObj fo i = _casm_ `` %r=(StgInt)(((StgInt*)%0)[(StgInt)%1]); '' fo i
#else
readInt64OffForeignObj fo i = _casm_ `` %r=(StgInt64)(((StgInt64*)%0)[(StgInt)%1]); '' fo i
#endif
\end{code}

\begin{code}
writeInt8OffAddr  :: Addr -> Int -> Int8  -> IO ()
writeInt8OffAddr a i e = _casm_ `` (((StgInt8*)%0)[(StgInt)%1])=(StgInt8)%2; '' a i e

writeInt8OffForeignObj  :: ForeignObj -> Int -> Int8  -> IO ()
writeInt8OffForeignObj fo i e = _casm_ `` (((StgInt8*)%0)[(StgInt)%1])=(StgInt8)%2; '' fo i e

writeInt16OffAddr :: Addr -> Int -> Int16 -> IO ()
writeInt16OffAddr a i e = _casm_ `` (((StgInt16*)%0)[(StgInt)%1])=(StgInt16)%2; '' a i e

writeInt16OffForeignObj :: ForeignObj -> Int -> Int16 -> IO ()
writeInt16OffForeignObj fo i e = _casm_ `` (((StgInt16*)%0)[(StgInt)%1])=(StgInt16)%2; '' fo i e

writeInt32OffAddr :: Addr -> Int -> Int32 -> IO ()
writeInt32OffAddr a i e = _casm_ `` (((StgInt32*)%0)[(StgInt)%1])=(StgInt32)%2; '' a i e

writeInt32OffForeignObj :: ForeignObj -> Int -> Int32 -> IO ()
writeInt32OffForeignObj fo i e = _casm_ `` (((StgInt32*)%0)[(StgInt)%1])=(StgInt32)%2; '' fo i e

writeInt64OffAddr :: Addr -> Int -> Int64 -> IO ()
#if WORD_SIZE_IN_BYTES==8
writeInt64OffAddr a i e = _casm_ `` (((StgInt*)%0)[(StgInt)%1])=(StgInt)%2; '' a i e
#else
writeInt64OffAddr a i e = _casm_ `` (((StgInt64*)%0)[(StgInt)%1])=(StgInt64)%2; '' a i e
#endif

writeInt64OffForeignObj :: ForeignObj -> Int -> Int64 -> IO ()
#if WORD_SIZE_IN_BYTES==8
writeInt64OffForeignObj fo i e = _casm_ `` (((StgInt*)%0)[(StgInt)%1])=(StgInt)%2; '' fo i e
#else
writeInt64OffForeignObj fo i e = _casm_ `` (((StgInt64*)%0)[(StgInt)%1])=(StgInt64)%2; '' fo i e
#endif

\end{code}

