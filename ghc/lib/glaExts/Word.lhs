%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Word]{Module @Word@}

This code is largely copied from the Hugs library of the same name.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Word
	( Word8
	, Word16
	, Word32
	, Word64
	, word8ToWord32  -- :: Word8  -> Word32
	, word32ToWord8  -- :: Word32 -> Word8
	, word16ToWord32 -- :: Word16 -> Word32
	, word32ToWord16 -- :: Word32 -> Word16
	, word8ToInt     -- :: Word8  -> Int
	, intToWord8     -- :: Int    -> Word8
	, word16ToInt    -- :: Word16 -> Int
	, intToWord16    -- :: Int    -> Word16
	, word32ToInt    -- :: Word32 -> Int
	, intToWord32    -- :: Int    -> Word32
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

word8ToWord32  :: Word8  -> Word32
word32ToWord8  :: Word32 -> Word8
word16ToWord32 :: Word16 -> Word32
word32ToWord16 :: Word32 -> Word16

word8ToInt   :: Word8  -> Int
intToWord8   :: Int    -> Word8
word16ToInt  :: Word16 -> Int
intToWord16  :: Int    -> Word16

word8ToInt  = word32ToInt    . word8ToWord32
intToWord8  = word32ToWord8  . intToWord32
word16ToInt = word32ToInt    . word16ToWord32
intToWord16 = word32ToWord16 . intToWord32

intToWord32 (I# x)   = W32# (int2Word# x)
word32ToInt (W32# x) = I#   (word2Int# x)

-----------------------------------------------------------------------------
-- Word8
-----------------------------------------------------------------------------

newtype Word8  = W8 Word32

word8ToWord32 (W8 x) = x .&. 0xff
word32ToWord8 = W8

instance Eq  Word8     where (==)    = binop (==)
instance Ord Word8     where compare = binop compare

instance Num Word8 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . integer2Word
    fromInt       = intToWord8

instance Bounded Word8 where
    minBound = 0
    maxBound = 0xff

instance Real Word8 where
    toRational x = toInteger x % 1

instance Integral Word8 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    toInteger     = toInteger . from
    toInt         = word8ToInt

instance Ix Word8 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (from (i - m))
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    toEnum         = to . intToWord32
    fromEnum       = word32ToInt . from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word8)]
		       where last = if d < c then minBound else maxBound

instance Read Word8 where
    readsPrec p = readDec

instance Show Word8 where
    showsPrec p = showsPrec p . from

instance Bits Word8 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
--  rotate      
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 8
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word16
-----------------------------------------------------------------------------

newtype Word16 = W16 Word32

word16ToWord32 (W16 x) = x .&. 0xffff
word32ToWord16 = W16

instance Eq  Word16     where (==)    = binop (==)
instance Ord Word16     where compare = binop compare

instance Num Word16 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . integer2Word
    fromInt       = intToWord16

instance Bounded Word16 where
    minBound = 0
    maxBound = 0xffff

instance Real Word16 where
  toRational x = toInteger x % 1

instance Integral Word16 where
  x `div` y     = to  (binop div x y)
  x `quot` y    = to  (binop quot x y)
  x `rem` y     = to  (binop rem x y)
  x `mod` y     = to  (binop mod x y)
  x `quotRem` y = to2 (binop quotRem x y)
  divMod        = quotRem
  toInteger     = toInteger . from
  toInt         = word16ToInt

instance Ix Word16 where
  range (m,n)          = [m..n]
  index b@(m,n) i
         | inRange b i = word32ToInt (from (i - m))
         | otherwise   = error "index: Index out of range"
  inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
  toEnum         = to . intToWord32
  fromEnum       = word32ToInt . from
  enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word16)]
  enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word16)]
		       where last = if d < c then minBound else maxBound

instance Read Word16 where
  readsPrec p = readDec

instance Show Word16 where
  showsPrec p x = showsPrec p (from x)

instance Bits Word16 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
--  rotate      
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 16
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word32
-----------------------------------------------------------------------------

data Word32 = W32# Word# deriving (Eq, Ord)

instance Num Word32 where
    (+) = intop (+)
    (-) = intop (-)
    (*) = intop (*)
    negate (W32# x) = W32# (int2Word# (negateInt# (word2Int# x)))
    abs             = absReal
    signum          = signumReal
    fromInteger     = integer2Word
    fromInt (I# x)  = W32# (int2Word# x)

{-# INLINE intop #-}
intop op x y = intToWord32 (word32ToInt x `op` word32ToInt y)

instance Bounded Word32 where
    minBound = 0
    maxBound = 0xffffffff

instance Real Word32 where
    toRational x = toInteger x % 1

instance Integral Word32 where
    x `div` y   = if x > 0 && y < 0	then quotWord (x-y-1) y
		  else if x < 0 && y > 0	then quotWord (x-y+1) y
		  else quotWord x y
    quot	=  quotWord
    rem		=  remWord
    x `mod` y = if x > 0 && y < 0 || x < 0 && y > 0 then
		    if r/=0 then r+y else 0
	    	else
		    r
	      where r = remWord x y
    a `quotRem` b    = (a `quot` b, a `rem` b)
    divMod x y       = (x `div` y,  x `mod` y)
    toInteger (W32# x) = int2Integer# (word2Int# x)
    toInt     (W32# x) = I# (word2Int# x)

{-# INLINE quotWord #-}
{-# INLINE remWord  #-}
(W32# x) `quotWord` (W32# y) = 
	W32# (int2Word# (word2Int# x `quotInt#` word2Int# y))
(W32# x) `remWord`  (W32# y) = 
	W32# (int2Word# (word2Int# x `remInt#`  word2Int# y))

instance Ix Word32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (i - m)
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    toEnum        = intToWord32
    fromEnum      = word32ToInt
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word32)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word32)]
		       where last = if d < c then minBound else maxBound

instance Read Word32 where
    readsPrec p = readDec

instance Show Word32 where
    showsPrec p x = showsPrec p (word32ToInt x)

instance Bits Word32 where
  (.&.)         = wordop and#
  (.|.)         = wordop or#
  xor           = wordop xor#
  complement x  = (x `xor` maxBound) + 1
  shift (W32# x) i@(I# i#)
	| i > 0     = W32# (shiftL# x i#)
	| otherwise = W32# (shiftRL# x (negateInt# i#))
  --rotate
  bit i             = 1 `shift` -i
  setBit x i        = x .|. bit i
  clearBit x i      = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i       = (x .&. bit i) /= 0
  bitSize  _        = 32
  isSigned _        = False

{-# INLINE wordop #-}
wordop op (W32# x) (W32# y) = W32# (x `op` y)

-----------------------------------------------------------------------------
-- Word64
-----------------------------------------------------------------------------

data Word64 = W64 {lo,hi::Word32} deriving (Eq, Ord, Bounded)

w64ToInteger W64{lo,hi} = toInteger lo + 0x100000000 * toInteger hi 
integerToW64 x = case x `quotRem` 0x100000000 of 
                 (h,l) -> W64{lo=fromInteger l, hi=fromInteger h}

instance Show Word64 where
  showsPrec p x = showsPrec p (w64ToInteger x)

instance Read Word64 where
  readsPrec p s = [ (integerToW64 x,r) | (x,r) <- readDec s ]

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
  to   :: Word32 -> a
  from :: a -> Word32

instance Coerce Word8 where
  from = word8ToWord32
  to   = word32ToWord8

instance Coerce Word16 where
  from = word16ToWord32
  to   = word32ToWord16

binop :: Coerce word => (Word32 -> Word32 -> a) -> (word -> word -> a)
binop op x y = from x `op` from y

to2 :: Coerce word => (Word32, Word32) -> (word, word)
to2 (x,y) = (to x, to y)

integer2Word (J# a# s# d#) = W32# (int2Word# (integer2Int# a# s# d#))

-----------------------------------------------------------------------------
-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

\end{code}
