%
% (c) The University of Glasgow, 2000
%
\section[PrelInt]{Module @PrelInt@}

\begin{code}
{-# OPTIONS -monly-3-regs #-}

module PrelInt 
   ( 
	Int8(..), Int16(..), Int32(..), Int64(..)

	, intToInt8      -- :: Int     -> Int8
	, intToInt16     -- :: Int     -> Int16
	, intToInt32     -- :: Int     -> Int32
	, intToInt64     -- :: Int     -> Int64

        , integerToInt8  -- :: Integer -> Int8
        , integerToInt16 -- :: Integer -> Int16
        , integerToInt32 -- :: Integer -> Int32
        , integerToInt64 -- :: Integer -> Int64

	, int8ToInt      -- :: Int8    -> Int
        , int8ToInteger  -- :: Int8    -> Integer
        , int8ToInt16    -- :: Int8    -> Int16
        , int8ToInt32    -- :: Int8    -> Int32
        , int8ToInt64    -- :: Int8    -> Int64

	, int16ToInt     -- :: Int16   -> Int
        , int16ToInteger -- :: Int16   -> Integer
        , int16ToInt8    -- :: Int16   -> Int8
        , int16ToInt32   -- :: Int16   -> Int32
        , int16ToInt64   -- :: Int16   -> Int64

	, int32ToInt     -- :: Int32   -> Int
        , int32ToInteger -- :: Int32   -> Integer
        , int32ToInt8    -- :: Int32   -> Int8
        , int32ToInt16   -- :: Int32   -> Int16
        , int32ToInt64   -- :: Int32   -> Int64

	, int64ToInt     -- :: Int64   -> Int
        , int64ToInteger -- :: Int64   -> Integer
        , int64ToInt8    -- :: Int64   -> Int8
        , int64ToInt16   -- :: Int64   -> Int16
        , int64ToInt32   -- :: Int64   -> Int32

	-- internal stuff
	, intToInt8#, i8ToInt#, intToInt16#, i16ToInt#, intToInt32#, i32ToInt#,
	, intToInt64#, plusInt64#, minusInt64#, negateInt64#

 ) where

import PrelWord
import PrelBits
import PrelArr
import PrelRead
import PrelReal
import PrelNum
import PrelBase

-- ---------------------------------------------------------------------------
-- Coercion functions (DEPRECATED)
-- ---------------------------------------------------------------------------

intToInt8      :: Int     -> Int8
intToInt16     :: Int     -> Int16
intToInt32     :: Int     -> Int32
intToInt64     :: Int     -> Int64

integerToInt8  :: Integer -> Int8
integerToInt16 :: Integer -> Int16
integerToInt32 :: Integer -> Int32
integerToInt64 :: Integer -> Int64

int8ToInt      :: Int8    -> Int
int8ToInteger  :: Int8    -> Integer
int8ToInt16    :: Int8    -> Int16
int8ToInt32    :: Int8    -> Int32
int8ToInt64    :: Int8    -> Int64

int16ToInt     :: Int16   -> Int
int16ToInteger :: Int16   -> Integer
int16ToInt8    :: Int16   -> Int8
int16ToInt32   :: Int16   -> Int32
int16ToInt64   :: Int16   -> Int64

int32ToInt     :: Int32   -> Int
int32ToInteger :: Int32   -> Integer
int32ToInt8    :: Int32   -> Int8
int32ToInt16   :: Int32   -> Int16
int32ToInt64   :: Int32   -> Int64

int64ToInt     :: Int64   -> Int
int64ToInteger :: Int64   -> Integer
int64ToInt8    :: Int64   -> Int8
int64ToInt16   :: Int64   -> Int16
int64ToInt32   :: Int64   -> Int32

integerToInt8  = fromInteger
integerToInt16 = fromInteger
integerToInt32 = fromInteger

int8ToInt16    = intToInt16 . int8ToInt
int8ToInt32    = intToInt32 . int8ToInt
int16ToInt32   = intToInt32 . int16ToInt

int16ToInt8  (I16# x) = I8#  (intToInt8# x)
int32ToInt8  (I32# x) = I8#  (intToInt8# x)
int32ToInt16 (I32# x) = I16# (intToInt16# x)

int8ToInteger  = toInteger
int8ToInt64    = int32ToInt64 . int8ToInt32

int16ToInteger = toInteger
int16ToInt64   = int32ToInt64 . int16ToInt32

int32ToInteger = toInteger

int64ToInt8    = int32ToInt8  . int64ToInt32
int64ToInt16   = int32ToInt16 . int64ToInt32

-----------------------------------------------------------------------------
-- The following rules for fromIntegral remove the need to export specialized
-- conversion functions.
-----------------------------------------------------------------------------

{-# RULES
   "fromIntegral/Int->Int8"         fromIntegral = intToInt8;
   "fromIntegral/Int->Int16"        fromIntegral = intToInt16;
   "fromIntegral/Int->Int32"        fromIntegral = intToInt32;
   "fromIntegral/Int->Int64"        fromIntegral = intToInt64;

   "fromIntegral/Integer->Int8"     fromIntegral = integerToInt8;
   "fromIntegral/Integer->Int16"    fromIntegral = integerToInt16;
   "fromIntegral/Integer->Int32"    fromIntegral = integerToInt32;
   "fromIntegral/Integer->Int64"    fromIntegral = integerToInt64;

   "fromIntegral/Int8->Int"         fromIntegral = int8ToInt;
   "fromIntegral/Int8->Integer"     fromIntegral = int8ToInteger;
   "fromIntegral/Int8->Int16"       fromIntegral = int8ToInt16;
   "fromIntegral/Int8->Int32"       fromIntegral = int8ToInt32;
   "fromIntegral/Int8->Int64"       fromIntegral = int8ToInt64;

   "fromIntegral/Int16->Int"        fromIntegral = int16ToInt;
   "fromIntegral/Int16->Integer"    fromIntegral = int16ToInteger;
   "fromIntegral/Int16->Int8"       fromIntegral = int16ToInt8;
   "fromIntegral/Int16->Int32"      fromIntegral = int16ToInt32;
   "fromIntegral/Int16->Int64"      fromIntegral = int16ToInt64;

   "fromIntegral/Int32->Int"        fromIntegral = int32ToInt;
   "fromIntegral/Int32->Integer"    fromIntegral = int32ToInteger;
   "fromIntegral/Int32->Int8"       fromIntegral = int32ToInt8;
   "fromIntegral/Int32->Int16"      fromIntegral = int32ToInt16;
   "fromIntegral/Int32->Int64"      fromIntegral = int32ToInt64;

   "fromIntegral/Int64->Int"        fromIntegral = int64ToInt;
   "fromIntegral/Int64->Integer"    fromIntegral = int64ToInteger;
   "fromIntegral/Int64->Int8"       fromIntegral = int64ToInt8;
   "fromIntegral/Int64->Int16"      fromIntegral = int64ToInt16;
   "fromIntegral/Int64->Int32"      fromIntegral = int64ToInt32
 #-}

-- -----------------------------------------------------------------------------
-- Int8
-- -----------------------------------------------------------------------------

data Int8 = I8# Int#

instance CCallable Int8
instance CReturnable Int8

int8ToInt (I8# x)  = I# (i8ToInt# x)

i8ToInt# :: Int# -> Int#
i8ToInt# x = if x' <=# 0x7f# then x' else x' -# 0x100#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xff#)

-- This doesn't perform any bounds checking on the value it is passed,
-- nor its sign, i.e., show (intToInt8 511) => "-1"
intToInt8 (I# x) = I8# (intToInt8# x)

intToInt8# :: Int# -> Int#
intToInt8# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xff#)

instance Eq  Int8     where 
  (I8# x#) == (I8# y#) = x# ==# y#
  (I8# x#) /= (I8# y#) = x# /=# y#

instance Ord Int8 where 
  compare (I8# x#) (I8# y#) = compareInt# (i8ToInt# x#) (i8ToInt# y#)

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
  fromInteger (S# i#)    = I8# (intToInt8# i#)
  fromInteger (J# s# d#) = I8# (intToInt8# (integer2Int# s# d#))

instance Bounded Int8 where
    minBound = 0x80
    maxBound = 0x7f 

instance Real Int8 where
    toRational x = toInteger x % 1

instance Integral Int8 where
    div x y
       | x > 0 && y < 0 = quotInt8 (x-y-1) y
       | x < 0 && y > 0	= quotInt8 (x-y+1) y
       | otherwise      = quotInt8 x y
    quot x@(I8# _) y@(I8# y#)
       | y# /=# 0# = x `quotInt8` y
       | otherwise = divZeroError "quot{Int8}" x
    rem x@(I8# _) y@(I8# y#)
       | y# /=# 0#  = x `remInt8` y
       | otherwise  = divZeroError "rem{Int8}" x
    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt8 x y

    a@(I8# _) `quotRem` b@(I8# _) = (a `quotInt8` b, a `remInt8` b)
    toInteger i8  = toInteger (int8ToInt i8)
    toInt     i8  = int8ToInt i8


remInt8, quotInt8 :: Int8 -> Int8 -> Int8
remInt8  (I8# x) (I8# y) = I8# (intToInt8# ((i8ToInt# x) `remInt#`  (i8ToInt# y)))
quotInt8 (I8# x) (I8# y) = I8# (intToInt8# ((i8ToInt# x) `quotInt#` (i8ToInt# y)))

instance Ix Int8 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	      | inRange b i = int8ToInt (i - m)
	      | otherwise   = indexError b i "Int8"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int8 where
    succ i
      | i == maxBound = succError "Int8"
      | otherwise     = i+1
    pred i
      | i == minBound = predError "Int8"
      | otherwise     = i-1

    toEnum x
      | x >= toInt (minBound::Int8) && x <= toInt (maxBound::Int8) 
      = intToInt8 x
      | otherwise
      = toEnumError "Int8" x (minBound::Int8,maxBound::Int8)

    fromEnum           = int8ToInt
    enumFrom e1        = map toEnum [fromEnum e1 .. fromEnum (maxBound::Int8)]
    enumFromThen e1 e2 = 
             map toEnum [fromEnum e1, fromEnum e2 .. fromEnum (last::Int8)]
		where 
		   last 
		     | e2 < e1   = minBound
		     | otherwise = maxBound

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
	| i > 0     = I8# (intToInt8# (iShiftL# (i8ToInt# x)  i#))
	| otherwise = I8# (intToInt8# (iShiftRA# (i8ToInt# x) (negateInt# i#)))
  i8@(I8# x)  `rotate` (I# i)
        | i ==# 0#    = i8
	| i ># 0#     = 
	     I8# (intToInt8# ( word2Int#  (
	             (int2Word# (iShiftL# (i8ToInt# x) i'))
		             `or#`
                     (int2Word# (iShiftRA# (word2Int# (
		                                (int2Word# x) `and#` 
			                        (int2Word# (0x100# -# pow2# i2))))
			                  i2)))))
	| otherwise = rotate i8 (I# (8# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 7#)
           i2 = 8# -# i'
  bitSize  _    = 8
  isSigned _    = True

pow2# :: Int# -> Int#
pow2# x# = iShiftL# 1# x#

pow2_64# :: Int# -> Int64#
pow2_64# x# = word64ToInt64# (shiftL64# (wordToWord64# (int2Word# 1#)) x#)

-- -----------------------------------------------------------------------------
-- Int16
-- -----------------------------------------------------------------------------

data Int16  = I16# Int#

instance CCallable Int16
instance CReturnable Int16

int16ToInt  (I16# x) = I# (i16ToInt# x)

i16ToInt# :: Int# -> Int#
i16ToInt# x = if x' <=# 0x7fff# then x' else x' -# 0x10000#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xffff#)

-- This doesn't perform any bounds checking on the value it is passed,
-- nor its sign, i.e., show (intToInt8 131071) => "-1"
intToInt16 (I# x) = I16# (intToInt16# x)

intToInt16# :: Int# -> Int#
intToInt16# i# = word2Int# ((int2Word# i#) `and#` int2Word# 0xffff#)

instance Eq  Int16     where
  (I16# x#) == (I16# y#) = x# ==# y#
  (I16# x#) /= (I16# y#) = x# /=# y#

instance Ord Int16 where
  compare (I16# x#) (I16# y#) = compareInt# (i16ToInt# x#) (i16ToInt# y#)

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
  fromInteger (S# i#)    = I16# (intToInt16# i#)
  fromInteger (J# s# d#) = I16# (intToInt16# (integer2Int# s# d#))

instance Bounded Int16 where
    minBound = 0x8000
    maxBound = 0x7fff 

instance Real Int16 where
    toRational x = toInteger x % 1

instance Integral Int16 where
    div x y
       | x > 0 && y < 0	= quotInt16 (x-y-1) y
       | x < 0 && y > 0	= quotInt16 (x-y+1) y
       | otherwise	= quotInt16 x y
    quot x@(I16# _) y@(I16# y#)
       | y# /=# 0#      = x `quotInt16` y
       | otherwise      = divZeroError "quot{Int16}" x
    rem x@(I16# _) y@(I16# y#)
       | y# /=# 0#      = x `remInt16` y
       | otherwise      = divZeroError "rem{Int16}" x
    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise			  = r
	where r = remInt16 x y

    a@(I16# _) `quotRem` b@(I16# _) = (a `quotInt16` b, a `remInt16` b)
    toInteger i16  = toInteger (int16ToInt i16)
    toInt     i16  = int16ToInt i16

remInt16, quotInt16 :: Int16 -> Int16 -> Int16
remInt16  (I16# x) (I16# y) = I16# (intToInt16# ((i16ToInt# x) `remInt#` (i16ToInt# y)))
quotInt16 (I16# x) (I16# y) = I16# (intToInt16# ((i16ToInt# x) `quotInt#` (i16ToInt# y)))

instance Ix Int16 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	      | inRange b i = int16ToInt (i - m)
	      | otherwise   = indexError b i "Int16"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int16 where
    succ i
      | i == maxBound = succError "Int16"
      | otherwise     = i+1

    pred i
      | i == minBound = predError "Int16"
      | otherwise     = i-1

    toEnum x
      | x >= toInt (minBound::Int16) && x <= toInt (maxBound::Int16) 
      = intToInt16 x
      | otherwise
      = toEnumError "Int16" x (minBound::Int16, maxBound::Int16)

    fromEnum         = int16ToInt

    enumFrom e1        = map toEnum [fromEnum e1 .. fromEnum (maxBound::Int16)]
    enumFromThen e1 e2 = map toEnum [fromEnum e1, fromEnum e2 .. fromEnum (last::Int16)]
			  where last 
			          | e2 < e1   = minBound
				  | otherwise = maxBound

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
	| i > 0     = I16# (intToInt16# (iShiftL# (i16ToInt# x)  i#))
	| otherwise = I16# (intToInt16# (iShiftRA# (i16ToInt# x) (negateInt# i#)))
  i16@(I16# x)  `rotate` (I# i)
        | i ==# 0#    = i16
	| i ># 0#     = 
	     I16# (intToInt16# (word2Int# (
	            (int2Word# (iShiftL# (i16ToInt# x) i')) 
		             `or#`
                    (int2Word# (iShiftRA# ( word2Int# (
		                    (int2Word# x) `and#` (int2Word# (0x100# -# pow2# i2))))
			                  i2)))))
	| otherwise = rotate i16 (I# (16# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 15#)
           i2 = 16# -# i'
  bitSize  _        = 16
  isSigned _        = True

-- -----------------------------------------------------------------------------
-- Int32
-- -----------------------------------------------------------------------------

data Int32  = I32# Int#

instance CCallable Int32
instance CReturnable Int32

int32ToInt  (I32# x) = I# (i32ToInt# x)

i32ToInt# :: Int# -> Int#
#if WORD_SIZE_IN_BYTES > 4
i32ToInt# x = if x' <=# 0x7fffffff# then x' else x' -# 0x100000000#
   where x' = word2Int# (int2Word# x `and#` int2Word# 0xffffffff#)
#else
i32ToInt# x = x
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
  compare (I32# x#) (I32# y#) = compareInt# (i32ToInt# x#) (i32ToInt# y#)

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
  fromInteger (S# i#)    = I32# (intToInt32# i#)
  fromInteger (J# s# d#) = I32# (intToInt32# (integer2Int# s# d#))


instance Bounded Int32 where 
    minBound = int2Int32 minBound
    maxBound = int2Int32 maxBound

int2Int32 :: Int -> Int32
int2Int32 (I# i#) = I32# (intToInt32# i#)

instance Real Int32 where
    toRational x = toInteger x % 1

instance Integral Int32 where
    div x y
       | x > 0 && y < 0	= quotInt32 (x-y-1) y
       | x < 0 && y > 0	= quotInt32 (x-y+1) y
       | otherwise      = quotInt32 x y
    quot x@(I32# _) y@(I32# y#)
       | y# /=# 0#  = x `quotInt32` y
       | otherwise  = divZeroError "quot{Int32}" x
    rem x@(I32# _) y@(I32# y#)
       | y# /=# 0#  = x `remInt32` y
       | otherwise  = divZeroError "rem{Int32}" x
    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise			  = r
	where r = remInt32 x y

    a@(I32# _) `quotRem` b@(I32# _) = (a `quotInt32` b, a `remInt32` b)
    toInteger i32  = toInteger (int32ToInt i32)
    toInt     i32  = int32ToInt i32

remInt32, quotInt32 :: Int32 -> Int32 -> Int32
remInt32  (I32# x) (I32# y) = I32# (intToInt32# ((i32ToInt# x) `remInt#`  (i32ToInt# y)))
quotInt32 (I32# x) (I32# y) = I32# (intToInt32# ((i32ToInt# x) `quotInt#` (i32ToInt# y)))

instance Ix Int32 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	      | inRange b i = int32ToInt (i - m)
	      | otherwise   = indexError b i "Int32"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int32 where
    succ i
      | i == maxBound = succError "Int32"
      | otherwise     = i+1

    pred i
      | i == minBound = predError "Int32"
      | otherwise     = i-1

    toEnum x
        -- with Int having the same range as Int32, the following test
	-- shouldn't fail. However, having it here 
      | x >= toInt (minBound::Int32) && x <= toInt (maxBound::Int32) 
      = intToInt32 x
      | otherwise
      = toEnumError "Int32" x (minBound::Int32, maxBound::Int32)

    fromEnum           = int32ToInt

    enumFrom e1        = map toEnum [fromEnum e1 .. fromEnum (maxBound::Int32)]
    enumFromThen e1 e2 = map toEnum [fromEnum e1, fromEnum e2 .. fromEnum (last::Int32)]
			  where 
			    last
			     | e2 < e1   = minBound
			     | otherwise = maxBound


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
	| i > 0     = I32# (intToInt32# (iShiftL# (i32ToInt# x)  i#))
	| otherwise = I32# (intToInt32# (iShiftRA# (i32ToInt# x) (negateInt# i#)))
  i32@(I32# x)  `rotate` (I# i)
        | i ==# 0#    = i32
	| i ># 0#     = 
             -- ( (x<<i') | ((x&(0x100000000-2^i2))>>i2)
	     I32# (intToInt32# ( word2Int# (
	            (int2Word# (iShiftL# (i32ToInt# x) i')) 
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
  bitSize  _    = 32
  isSigned _    = True

-- -----------------------------------------------------------------------------
-- Int64
-- -----------------------------------------------------------------------------

#if WORD_SIZE_IN_BYTES == 8
data Int64  = I64# Int#

int32ToInt64 (I32# i#) = I64# i#

intToInt32# :: Int# -> Int#
intToInt32# i# = word2Int# ((int2Word# i#) `and#` (case (maxBound::Word32) of W# x# -> x#))

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
  fromInteger (S# i#)    = I64# i#
  fromInteger (J# s# d#) = I64# (integer2Int# s# d#)

instance Bounded Int64 where
  minBound = integerToInt64 (-0x8000000000000000)
  maxBound = integerToInt64 0x7fffffffffffffff

instance Integral Int64 where
    div x y
      | x > 0 && y < 0	= quotInt64 (x-y-1) y
      | x < 0 && y > 0	= quotInt64 (x-y+1) y
      | otherwise       = quotInt64 x y

    quot x@(I64# _) y@(I64# y#)
       | y# /=# 0# = x `quotInt64` y
       | otherwise = divZeroError "quot{Int64}" x

    rem x@(I64# _) y@(I64# y#)
       | y# /=# 0# = x `remInt64` y
       | otherwise = divZeroError "rem{Int64}" x

    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt64 x y

    a@(I64# _) `quotRem` b@(I64# _) = (a `quotInt64` b, a `remInt64` b)
    toInteger (I64# i#) = toInteger (I# i#)
    toInt     (I64# i#) = I# i#

remInt64  (I64# x) (I64# y) = I64# (x `remInt#` y)
quotInt64 (I64# x) (I64# y) = I64# (x `quotInt#` y)

int64ToInteger (I64# i#) = toInteger (I# i#)
integerToInt64 i = case fromInteger i of { I# i# -> I64# i# }

intToInt64 (I# i#) = I64# i#
int64ToInt (I64# i#) = I# i#

#else
--assume: support for long-longs
data Int64 = I64# Int64#

int32ToInt64 (I32# i#) = I64# (intToInt64# i#)
int64ToInt32 (I64# i#) = I32# (int64ToInt# i#)

int64ToInteger (I64# x#) = 
   case int64ToInteger# x# of
     (# s#, p# #) -> J# s# p#

integerToInt64 (S# i#) = I64# (intToInt64# i#)
integerToInt64 (J# s# d#) = I64# (integerToInt64# s# d#)

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

compareInt64# :: Int64# -> Int64# -> Ordering
compareInt64# i# j# 
 | i# `ltInt64#` j# = LT
 | i# `eqInt64#` j# = EQ
 | otherwise	    = GT

instance Bounded Int64 where
  minBound = integerToInt64 (-0x8000000000000000)
  maxBound = integerToInt64 0x7fffffffffffffff

instance Integral Int64 where
    div x y
      | x > 0 && y < 0	= quotInt64 (x-y-1) y
      | x < 0 && y > 0	= quotInt64 (x-y+1) y
      | otherwise       = quotInt64 x y

    quot x@(I64# _) y@(I64# y#)
       | y# `neInt64#` (intToInt64# 0#) = x `quotInt64` y
       | otherwise = divZeroError "quot{Int64}" x

    rem x@(I64# _) y@(I64# y#)
       | y# `neInt64#` (intToInt64# 0#) = x `remInt64` y
       | otherwise = divZeroError "rem{Int64}" x

    mod x y
       | x > 0 && y < 0 || x < 0 && y > 0 = if r/=0 then r+y else 0
       | otherwise = r
	where r = remInt64 x y

    a@(I64# _) `quotRem` b@(I64# _) = (a `quotInt64` b, a `remInt64` b)
    toInteger i         = int64ToInteger i
    toInt     i         = int64ToInt i

remInt64, quotInt64 :: Int64 -> Int64 -> Int64
remInt64  (I64# x) (I64# y) = I64# (x `remInt64#` y)
quotInt64 (I64# x) (I64# y) = I64# (x `quotInt64#` y)

intToInt64 (I# i#) = I64# (intToInt64# i#)
int64ToInt (I64# i#) = I# (int64ToInt# i#)

-- Int64# primop wrappers:

ltInt64# :: Int64# -> Int64# -> Bool
ltInt64# x# y# = stg_ltInt64 x# y# /=# 0#
      
leInt64# :: Int64# -> Int64# -> Bool
leInt64# x# y# = stg_leInt64 x# y# /=# 0#

eqInt64# :: Int64# -> Int64# -> Bool
eqInt64# x# y# = stg_eqInt64 x# y# /=# 0#

neInt64# :: Int64# -> Int64# -> Bool
neInt64# x# y# = stg_neInt64 x# y# /=# 0#

geInt64# :: Int64# -> Int64# -> Bool
geInt64# x# y# = stg_geInt64 x# y# /=# 0#

gtInt64# :: Int64# -> Int64# -> Bool
gtInt64# x# y# = stg_gtInt64 x# y# /=# 0#

foreign import "stg_intToInt64" unsafe intToInt64# :: Int# -> Int64#
foreign import "stg_int64ToInt" unsafe int64ToInt# :: Int64# -> Int#
foreign import "stg_negateInt64" unsafe negateInt64# :: Int64# -> Int64#
foreign import "stg_remInt64" unsafe remInt64# :: Int64# -> Int64# -> Int64#
foreign import "stg_quotInt64" unsafe quotInt64# :: Int64# -> Int64# -> Int64#
foreign import "stg_timesInt64" unsafe timesInt64# :: Int64# -> Int64# -> Int64#
foreign import "stg_minusInt64" unsafe minusInt64# :: Int64# -> Int64# -> Int64#
foreign import "stg_plusInt64" unsafe plusInt64# :: Int64# -> Int64# -> Int64#
foreign import "stg_gtInt64" unsafe stg_gtInt64 :: Int64# -> Int64# -> Int#
foreign import "stg_geInt64" unsafe stg_geInt64 :: Int64# -> Int64# -> Int#
foreign import "stg_neInt64" unsafe stg_neInt64 :: Int64# -> Int64# -> Int#
foreign import "stg_eqInt64" unsafe stg_eqInt64 :: Int64# -> Int64# -> Int#
foreign import "stg_leInt64" unsafe stg_leInt64 :: Int64# -> Int64# -> Int#
foreign import "stg_ltInt64" unsafe stg_ltInt64 :: Int64# -> Int64# -> Int#

#endif

--
-- Code that's independent of Int64 rep.
-- 
instance CCallable   Int64
instance CReturnable Int64

instance Enum Int64 where
    succ i
      | i == maxBound = succError "Int64"
      | otherwise     = i+1

    pred i
      | i == minBound = predError "Int64"
      | otherwise     = i-1

    toEnum    i = intToInt64 i
    fromEnum  x
      | x >= intToInt64 (minBound::Int) && x <= intToInt64 (maxBound::Int)
      = int64ToInt x
      | otherwise
      = fromEnumError "Int64" x

    enumFrom e1        = map integerToInt64 [int64ToInteger e1 .. int64ToInteger (maxBound::Int64)]
    enumFromTo e1 e2   = map integerToInt64 [int64ToInteger e1 .. int64ToInteger e2]
    enumFromThen e1 e2 = map integerToInt64 [int64ToInteger e1, int64ToInteger e2 .. int64ToInteger last]
		       where 
			  last :: Int64
		          last 
			   | e2 < e1   = minBound
			   | otherwise = maxBound

    enumFromThenTo e1 e2 e3 = map integerToInt64 [int64ToInteger e1, int64ToInteger e2 .. int64ToInteger e3]

instance Show Int64 where
    showsPrec p i64 = showsPrec p (int64ToInteger i64)

instance Read Int64 where
  readsPrec _ s = [ (integerToInt64 x,r) | (x,r) <- readDec s ]

instance Ix Int64 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = int64ToInt (i-m)
	   | otherwise   = indexError b i "Int64"
    inRange (m,n) i      = m <= i && i <= n

instance Real Int64 where
  toRational x = toInteger x % 1

#if WORD_SIZE_IN_BYTES == 8
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
  bitSize  _    = 64
  isSigned _    = True

#else /* WORD_SIZE_IN_BYTES != 8 */

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
  bitSize  _    = 64
  isSigned _    = True

foreign import "stg_not64" unsafe not64# :: Word64# -> Word64#
foreign import "stg_xor64" unsafe xor64# :: Word64# -> Word64# -> Word64#
foreign import "stg_or64" unsafe or64# :: Word64# -> Word64# -> Word64#
foreign import "stg_and64" unsafe and64# :: Word64# -> Word64# -> Word64#
foreign import "stg_shiftRL64" unsafe shiftRL64# :: Word64# -> Int# -> Word64#
foreign import "stg_iShiftRA64" unsafe iShiftRA64# :: Int64# -> Int# -> Int64#
foreign import "stg_iShiftRL64" unsafe iShiftRL64# :: Int64# -> Int# -> Int64#
foreign import "stg_iShiftL64" unsafe iShiftL64# :: Int64# -> Int# -> Int64#
foreign import "stg_shiftL64" unsafe shiftL64# :: Word64# -> Int# -> Word64#

#endif

-- ---------------------------------------------------------------------------
-- Miscellaneous Utilities
-- ---------------------------------------------------------------------------

absReal :: (Ord a, Num a) => a -> a
absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal :: (Ord a, Num a) => a -> a
signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}
