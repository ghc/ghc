%
% (c) The AQUA Project, Glasgow University, 1994-1996
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
import Error
import Bits
import GHC

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

int8ToInt16  = I16 . int8ToInt
int8ToInt32  = I32 . int8ToInt
int16ToInt8  = I8  . int16ToInt
int16ToInt32 = I32 . int16ToInt
int32ToInt8  = I8  . int32ToInt
int32ToInt16 = I16 . int32ToInt

-----------------------------------------------------------------------------
-- Int8
-----------------------------------------------------------------------------

newtype Int8  = I8 Int

int8ToInt (I8 x) = if x' <= 0x7f then x' else x' - 0x100
   where x' = case x of { I# x ->
		I# (word2Int# (int2Word# x `and#` int2Word# 0xff#))
	      }
intToInt8 = I8

instance Eq  Int8     where (==)    = binop (==)
instance Ord Int8     where compare = binop compare

instance Num Int8 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    fromInt       = to

instance Bounded Int8 where
    minBound = 0x80
    maxBound = 0x7f 

instance Real Int8 where
    toRational x = toInteger x % 1

instance Integral Int8 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    toInteger     = toInteger . from
    toInt         = toInt     . from

instance Ix Int8 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	      | inRange b i = from (i - m)
	      | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int8 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int8)]
			  where last = if d < c then minBound else maxBound

instance Read Int8 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int8 where
    showsPrec p = showsPrec p . from

binop8 :: (Int32 -> Int32 -> a) -> (Int8 -> Int8 -> a)
binop8 op x y = int8ToInt32 x `op` int8ToInt32 y

instance Bits Int8 where
  x .&. y       = int32ToInt8 (binop8 (.&.) x y)
  x .|. y       = int32ToInt8 (binop8 (.|.) x y)
  x `xor` y     = int32ToInt8 (binop8 xor x y)
  complement    = int32ToInt8 . complement . int8ToInt32
  x `shift` i   = int32ToInt8 (int8ToInt32 x `shift` i)
--  rotate      
  bit           = int32ToInt8 . bit
  setBit x i    = int32ToInt8 (setBit (int8ToInt32 x) i)
  clearBit x i  = int32ToInt8 (clearBit (int8ToInt32 x) i)
  complementBit x i = int32ToInt8 (complementBit (int8ToInt32 x) i)
  testBit x i   = testBit (int8ToInt32 x) i
  bitSize  _    = 8
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int16
-----------------------------------------------------------------------------

newtype Int16  = I16 Int

int16ToInt (I16 x) = if x' <= 0x7fff then x' else x' - 0x10000
   where x' = case x of { I# x ->
		I# (word2Int# (int2Word# x `and#` int2Word# 0xffff#))
	      }
intToInt16 = I16

instance Eq  Int16     where (==)    = binop (==)
instance Ord Int16     where compare = binop compare

instance Num Int16 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    fromInt       = to

instance Bounded Int16 where
    minBound = 0x8000
    maxBound = 0x7fff 

instance Real Int16 where
    toRational x = toInteger x % 1

instance Integral Int16 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    toInteger     = toInteger . from
    toInt         = toInt     . from

instance Ix Int16 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	      | inRange b i = from (i - m)
	      | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int16 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int16)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int16)]
			  where last = if d < c then minBound else maxBound

instance Read Int16 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int16 where
    showsPrec p = showsPrec p . from

binop16 :: (Int32 -> Int32 -> a) -> (Int16 -> Int16 -> a)
binop16 op x y = int16ToInt32 x `op` int16ToInt32 y

instance Bits Int16 where
  x .&. y       = int32ToInt16 (binop16 (.&.) x y)
  x .|. y       = int32ToInt16 (binop16 (.|.) x y)
  x `xor` y     = int32ToInt16 (binop16 xor x y)
  complement    = int32ToInt16 . complement . int16ToInt32
  x `shift` i   = int32ToInt16 (int16ToInt32 x `shift` i)
--  rotate      
  bit           = int32ToInt16 . bit
  setBit x i    = int32ToInt16 (setBit (int16ToInt32 x) i)
  clearBit x i  = int32ToInt16 (clearBit (int16ToInt32 x) i)
  complementBit x i = int32ToInt16 (complementBit (int16ToInt32 x) i)
  testBit x i   = testBit (int16ToInt32 x) i
  bitSize  _    = 16
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int32
-----------------------------------------------------------------------------

newtype Int32  = I32 Int

int32ToInt (I32 x) = x
intToInt32 = I32

instance Eq  Int32     where (==)    = binop (==)
instance Ord Int32     where compare = binop compare

instance Num Int32 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . fromInteger
    fromInt       = to

instance Bounded Int32 where
    minBound = to minBound
    maxBound = to maxBound

instance Real Int32 where
    toRational x = toInteger x % 1

instance Integral Int32 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    toInteger     = toInteger . from
    toInt         = toInt     . from

instance Ix Int32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	      | inRange b i = from (i - m)
	      | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int32 where
    toEnum         = to 
    fromEnum       = from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Int32)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Int32)]
			  where last = if d < c then minBound else maxBound

instance Read Int32 where
    readsPrec p s = [ (to x,r) | (x,r) <- readsPrec p s ]

instance Show Int32 where
    showsPrec p = showsPrec p . from

instance Bits Int32 where
  x .&. y      	= to (binop (wordop and#) x y)
  x .|. y      	= to (binop (wordop or# ) x y)
  x `xor` y    	= to (binop (wordop xor#) x y)
  complement x 	= (x `xor` maxBound) + 1
  shift (I32 (I# x)) i@(I# i#)
	| i > 0     = I32 (I# (iShiftRL# x i#))
	| otherwise = I32 (I# (iShiftL# x i#))
--  rotate    	   
  bit i        	= 1 `shift` -i
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
-- Coercions - used to make the instance declarations more uniform
-----------------------------------------------------------------------------

class Coerce a where
  to   :: Int -> a
  from :: a -> Int

instance Coerce Int32 where
  from = int32ToInt
  to   = intToInt32

instance Coerce Int8 where
  from = int8ToInt
  to   = intToInt8

instance Coerce Int16 where
  from = int16ToInt
  to   = intToInt16

binop :: Coerce int => (Int -> Int -> a) -> (int -> int -> a)
binop op x y = from x `op` from y

to2 :: Coerce int => (Int, Int) -> (int, int)
to2 (x,y) = (to x, to y)

-----------------------------------------------------------------------------
-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}
