%
% (c) The AQUA Project, Glasgow University, 1997-1998
%

\section[Int]{Module @Int@}

This code is largely copied from the Hugs library of the same name.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

-----------------------------------------------------------------------------
-- Signed Integers
-- Suitable for use with Hugs 1.4 on 32 bit systems.
-----------------------------------------------------------------------------

module Int
	( Int8
	, Int16
	, Int32
	--, Int64
	, int8ToInt  -- :: Int8  -> Int
	, intToInt8  -- :: Int   -> Int8
	, int16ToInt -- :: Int16 -> Int
	, intToInt16 -- :: Int   -> Int16
	, int32ToInt -- :: Int32 -> Int
	, intToInt32 -- :: Int   -> Int32
	-- plus Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	--  Show and Bits instances for each of Int8, Int16 and Int32
	) where

import PrelBase
import PrelNum
import PrelRead
import Ix
import GHCerr  ( error )
import Bits
import GHC
import CCall

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
    quot x@(I8# _) y@(I8# y#) =
       if y# /=# 0#
       then x `quotInt8` y
       else error "Integral.Int8.quot: divide by 0\n"
    rem x@(I8# _) y@(I8# y#) =
       if y# /=# 0#
       then x `remInt8` y
       else error "Integral.Int8.rem: divide by 0\n"
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
	| otherwise = I8# (intToInt8# (iShiftRA# (int8ToInt# x) i#))
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
    quot x@(I16# _) y@(I16# y#) =
       if y# /=# 0#
       then x `quotInt16` y
       else error "Integral.Int16.quot: divide by 0\n"
    rem x@(I16# _) y@(I16# y#) =
       if y# /=# 0#
       then x `remInt16` y
       else error "Integral.Int16.rem: divide by 0\n"
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
    toEnum         = intToInt16
    fromEnum       = int16ToInt
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
	| otherwise = I16# (intToInt16# (iShiftRA# (int16ToInt# x) i#))
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
\end{code}

\subsection[Int32]{The @Int32@ interface}

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
    quot x@(I32# _) y@(I32# y#) =
       if y# /=# 0#
       then x `quotInt32` y
       else error "Integral.Int32.quot: divide by 0\n"
    rem x@(I32# _) y@(I32# y#) =
       if y# /=# 0#
       then x `remInt32` y
       else error "Integral.Int32.rem: divide by 0\n"
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
	| otherwise = I32# (intToInt32# (iShiftRA# (int32ToInt# x) i#))
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

{-# INLINE wordop #-}
wordop op (I# x) (I# y) = I# (word2Int# (int2Word# x `op` int2Word# y))

-----------------------------------------------------------------------------
-- End of exported definitions
--
-- The remainder of this file consists of definitions which are only
-- used in the implementation.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}
