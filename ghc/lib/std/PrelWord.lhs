%
% (c) The University of Glasgow, 1997-2000
%
\section[PrelWord]{Module @PrelWord@}

\begin{code}
#include "MachDeps.h"

module PrelWord (
	Word8(..), Word16(..), Word32(..), Word64(..),

        -- SUP: deprecated in the new FFI, subsumed by fromIntegral
	, intToWord8      -- :: Int     -> Word8
	, intToWord16     -- :: Int     -> Word16
	, intToWord32     -- :: Int     -> Word32
	, intToWord64     -- :: Int     -> Word64

	, integerToWord8  -- :: Integer -> Word8
	, integerToWord16 -- :: Integer -> Word16
	, integerToWord32 -- :: Integer -> Word32
	, integerToWord64 -- :: Integer -> Word64

	, word8ToInt      -- :: Word8   -> Int
        , word8ToInteger  -- :: Word8   -> Integer
	, word8ToWord16   -- :: Word8   -> Word16
	, word8ToWord32   -- :: Word8   -> Word32
	, word8ToWord64   -- :: Word8   -> Word64

	, word16ToInt     -- :: Word16  -> Int
        , word16ToInteger -- :: Word16  -> Integer
	, word16ToWord8   -- :: Word16  -> Word8
	, word16ToWord32  -- :: Word16  -> Word32
	, word16ToWord64  -- :: Word16  -> Word64

	, word32ToInt     -- :: Word32  -> Int
        , word32ToInteger -- :: Word32  -> Integer
	, word32ToWord8   -- :: Word32  -> Word8
	, word32ToWord16  -- :: Word32  -> Word16
	, word32ToWord64  -- :: Word32  -> Word64

	, word64ToInt     -- :: Word64  -> Int
        , word64ToInteger -- :: Word64  -> Integer
	, word64ToWord8   -- :: Word64  -> Word8
	, word64ToWord16  -- :: Word64  -> Word16
	, word64ToWord32  -- :: Word64  -> Word32

	-- NB! GHC SPECIFIC:
 	, wordToWord8     -- :: Word    -> Word8
	, wordToWord16    -- :: Word    -> Word16
	, wordToWord32    -- :: Word    -> Word32
	, wordToWord64    -- :: Word    -> Word64

	, word8ToWord     -- :: Word8   -> Word
	, word16ToWord    -- :: Word16  -> Word
	, word32ToWord    -- :: Word32  -> Word
	, word64ToWord    -- :: Word64  -> Word

	-- The "official" place to get these from is Addr.
        -- SUP: deprecated in the new FFI, subsumed by the Storable class
	, indexWord8OffAddr
	, indexWord16OffAddr
	, indexWord32OffAddr
	, indexWord64OffAddr
	
	, readWord8OffAddr
	, readWord16OffAddr
	, readWord32OffAddr
	, readWord64OffAddr
	
	, writeWord8OffAddr
	, writeWord16OffAddr
	, writeWord32OffAddr
	, writeWord64OffAddr

	-- internal stuff
	, wordToInt
	, wordToWord8#, wordToWord16#, wordToWord32#, wordToWord64#

	, word64ToInt64#, int64ToWord64#
	, wordToWord64#, word64ToWord#

	, toEnumError, fromEnumError, succError, predError, divZeroError
  ) where

import Numeric	( showInt )

import PrelArr
import PrelRead
import PrelIOBase
import PrelEnum
import PrelAddr
import PrelReal
import PrelNum
import PrelBase

-- ---------------------------------------------------------------------------
-- Coercion functions (DEPRECATED)
-- ---------------------------------------------------------------------------

intToWord8      :: Int     -> Word8
intToWord16     :: Int     -> Word16
intToWord32     :: Int     -> Word32
intToWord64     :: Int     -> Word64

integerToWord8  :: Integer -> Word8
integerToWord16 :: Integer -> Word16
integerToWord32 :: Integer -> Word32
integerToWord64 :: Integer -> Word64

word8ToInt      :: Word8   -> Int
word8ToInteger  :: Word8   -> Integer
word8ToWord16   :: Word8   -> Word16
word8ToWord32   :: Word8   -> Word32
word8ToWord64   :: Word8   -> Word64

word16ToInt     :: Word16  -> Int
word16ToInteger :: Word16  -> Integer
word16ToWord8   :: Word16  -> Word8
word16ToWord32  :: Word16  -> Word32
word16ToWord64  :: Word16  -> Word64

word32ToInt     :: Word32  -> Int
word32ToInteger :: Word32  -> Integer
word32ToWord8   :: Word32  -> Word8
word32ToWord16  :: Word32  -> Word16
word32ToWord64  :: Word32  -> Word64

word64ToInt     :: Word64  -> Int
word64ToInteger :: Word64  -> Integer
word64ToWord8   :: Word64  -> Word8
word64ToWord16  :: Word64  -> Word16
word64ToWord32  :: Word64  -> Word32

wordToWord8     :: Word    -> Word8
wordToWord16    :: Word    -> Word16
wordToWord32    :: Word    -> Word32
wordToWord64    :: Word    -> Word64

word8ToWord     :: Word8   -> Word
word16ToWord    :: Word16  -> Word
word32ToWord    :: Word32  -> Word
word64ToWord    :: Word64  -> Word

intToWord8      = word32ToWord8   . intToWord32
intToWord16     = word32ToWord16  . intToWord32

integerToWord8  = fromInteger
integerToWord16 = fromInteger

word8ToInt      = word32ToInt     . word8ToWord32
word8ToInteger  = word32ToInteger . word8ToWord32

word16ToInt     = word32ToInt     . word16ToWord32
word16ToInteger = word32ToInteger . word16ToWord32

#if WORD_SIZE_IN_BYTES > 4
intToWord32 (I# x)   = W32# ((int2Word# x) `and#` (case (maxBound::Word32) of W32# x# -> x#))
#else
intToWord32 (I# x)   = W32# (int2Word# x)
#endif

word32ToInt (W32# x) = I#   (word2Int# x)

word32ToInteger (W32# x) = word2Integer x
integerToWord32 = fromInteger

-----------------------------------------------------------------------------
-- The following rules for fromIntegral remove the need to export specialized
-- conversion functions.
-----------------------------------------------------------------------------

{-# RULES
   "fromIntegral/Int->Word8"        fromIntegral = intToWord8;
   "fromIntegral/Int->Word16"       fromIntegral = intToWord16;
   "fromIntegral/Int->Word32"       fromIntegral = intToWord32;
   "fromIntegral/Int->Word64"       fromIntegral = intToWord64;

   "fromIntegral/Integer->Word8"    fromIntegral = integerToWord8;
   "fromIntegral/Integer->Word16"   fromIntegral = integerToWord16;
   "fromIntegral/Integer->Word32"   fromIntegral = integerToWord32;
   "fromIntegral/Integer->Word64"   fromIntegral = integerToWord64;

   "fromIntegral/Word8->Int"        fromIntegral = word8ToInt;
   "fromIntegral/Word8->Integer"    fromIntegral = word8ToInteger;
   "fromIntegral/Word8->Word16"     fromIntegral = word8ToWord16;
   "fromIntegral/Word8->Word32"     fromIntegral = word8ToWord32;
   "fromIntegral/Word8->Word64"     fromIntegral = word8ToWord64;

   "fromIntegral/Word16->Int"       fromIntegral = word16ToInt;
   "fromIntegral/Word16->Integer"   fromIntegral = word16ToInteger;
   "fromIntegral/Word16->Word8"     fromIntegral = word16ToWord8;
   "fromIntegral/Word16->Word32"    fromIntegral = word16ToWord32;
   "fromIntegral/Word16->Word64"    fromIntegral = word16ToWord64;

   "fromIntegral/Word32->Int"       fromIntegral = word32ToInt;
   "fromIntegral/Word32->Integer"   fromIntegral = word32ToInteger;
   "fromIntegral/Word32->Word8"     fromIntegral = word32ToWord8;
   "fromIntegral/Word32->Word16"    fromIntegral = word32ToWord16;
   "fromIntegral/Word32->Word64"    fromIntegral = word32ToWord64;

   "fromIntegral/Word64->Int"       fromIntegral = word64ToInt;
   "fromIntegral/Word64->Integer"   fromIntegral = word64ToInteger;
   "fromIntegral/Word64->Word8"     fromIntegral = word64ToWord8;
   "fromIntegral/Word64->Word16"    fromIntegral = word64ToWord16;
   "fromIntegral/Word64->Word32"    fromIntegral = word64ToWord32
 #-}

\end{code}

\subsection[Word8]{The @Word8@ interface}


The byte type @Word8@ is represented in the Haskell
heap by boxing up a 32-bit quantity, @Word#@. An invariant
for this representation is that the higher 24 bits are
*always* zeroed out. A consequence of this is that
operations that could possibly overflow have to mask
out the top three bytes before building the resulting @Word8@.

\begin{code}
data Word8  = W8# Word#

instance CCallable Word8
instance CReturnable Word8

word8ToWord32 (W8#  x) = W32# x
word8ToWord16 (W8#  x) = W16# x
word32ToWord8 (W32# x) = W8# (wordToWord8# x)

-- mask out upper three bytes.
intToWord8# :: Int# -> Word#
intToWord8# i# = (int2Word# i#) `and#` (int2Word# 0xff#)

wordToWord8# :: Word# -> Word#
wordToWord8# w# = w# `and#` (int2Word# 0xff#)

instance Eq  Word8     where 
  (W8# x) == (W8# y) = x `eqWord#` y
  (W8# x) /= (W8# y) = x `neWord#` y

instance Ord Word8     where 
  compare (W8# x#) (W8# y#) = compareWord# x# y#
  (<)  (W8# x) (W8# y)      = x `ltWord#` y
  (<=) (W8# x) (W8# y)      = x `leWord#` y
  (>=) (W8# x) (W8# y)      = x `geWord#` y
  (>)  (W8# x) (W8# y)      = x `gtWord#` y
  max x@(W8# x#) y@(W8# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W8# x#) y@(W8# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

-- Helper function, used by Ord Word* instances.
compareWord# :: Word# -> Word# -> Ordering
compareWord# x# y# 
 | x# `ltWord#` y# = LT
 | x# `eqWord#` y# = EQ
 | otherwise       = GT

instance Num Word8 where
  (W8# x) + (W8# y) = 
      W8# (intToWord8# (word2Int# x +# word2Int# y))
  (W8# x) - (W8# y) = 
      W8# (intToWord8# (word2Int# x -# word2Int# y))
  (W8# x) * (W8# y) = 
      W8# (intToWord8# (word2Int# x *# word2Int# y))
  negate w@(W8# x)  = 
     if x' ==# 0# 
      then w
      else W8# (int2Word# (0x100# -# x'))
     where
      x' = word2Int# x
  abs x         = x
  signum        = signumReal
  fromInteger (S# i#)    = W8# (wordToWord8# (int2Word# i#))
  fromInteger (J# s# d#) = W8# (wordToWord8# (integer2Word# s# d#))
  fromInt       = intToWord8

instance Bounded Word8 where
  minBound = 0
  maxBound = 0xff

instance Real Word8 where
  toRational x = toInteger x % 1

-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word8 where
  div  x@(W8# x#)  (W8# y#) 
    | y# `neWord#` (int2Word# 0#) = W8# (x# `quotWord#` y#)
    | otherwise                   = divZeroError "div{Word8}" x

  quot x@(W8# x#)  (W8# y#)   
    | y# `neWord#` (int2Word# 0#) = W8# (x# `quotWord#` y#)
    | otherwise                   = divZeroError "quot{Word8}" x

  rem  x@(W8# x#)  (W8# y#)
    | y# `neWord#` (int2Word# 0#) = W8# (x# `remWord#` y#)
    | otherwise                   = divZeroError "rem{Word8}" x

  mod  x@(W8# x#)  (W8# y#)
    | y# `neWord#` (int2Word# 0#) = W8# (x# `remWord#` y#)
    | otherwise                   = divZeroError "mod{Word8}" x

  quotRem (W8# x) (W8# y) = (W8# (x `quotWord#` y), W8# (x `remWord#` y))
  divMod  (W8# x) (W8# y) = (W8# (x `quotWord#` y), W8# (x `remWord#` y))

  toInteger = toInteger . toInt
  toInt     = word8ToInt

instance Ix Word8 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = word8ToInt (i-m)
	   | otherwise   = indexError b i "Word8"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    succ w	    
      | w == maxBound = succError "Word8"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word8"
      | otherwise     = w-1

    toEnum   i@(I# i#)  
      | i >= toInt (minBound::Word8) && i <= toInt (maxBound::Word8) 
      = W8# (intToWord8# i#)
      | otherwise
      = toEnumError "Word8" i (minBound::Word8,maxBound::Word8)

    fromEnum  (W8# w) = I# (word2Int# w)

    enumFrom          = boundedEnumFrom
    enumFromThen      = boundedEnumFromThen

instance Read Word8 where
    readsPrec _ = readDec

instance Show Word8 where
    showsPrec _ = showInt
\end{code}

\subsection[Word16]{The @Word16@ interface}

The double byte type @Word16@ is represented in the Haskell
heap by boxing up a machine word, @Word#@. An invariant
for this representation is that only the lower 16 bits are
`active', any bits above are {\em always} zeroed out.
A consequence of this is that operations that could possibly
overflow have to mask out anything above the lower two bytes
before putting together the resulting @Word16@.

\begin{code}
data Word16 = W16# Word#

instance CCallable Word16
instance CReturnable Word16

word16ToWord8  (W16# x) = W8#  (wordToWord8#  x)
word16ToWord32 (W16# x) = W32# x

word32ToWord16 (W32# x) = W16# (wordToWord16# x)

-- mask out upper 16 bits.
intToWord16# :: Int# -> Word#
intToWord16# i# = ((int2Word# i#) `and#` (int2Word# 0xffff#))

wordToWord16# :: Word# -> Word#
wordToWord16# w# = w# `and#` (int2Word# 0xffff#)

instance Eq  Word16    where 
  (W16# x) == (W16# y) = x `eqWord#` y
  (W16# x) /= (W16# y) = x `neWord#` y

instance Ord Word16     where
  compare (W16# x#) (W16# y#) = compareWord# x# y#
  (<)  (W16# x) (W16# y)      = x `ltWord#` y
  (<=) (W16# x) (W16# y)      = x `leWord#` y
  (>=) (W16# x) (W16# y)      = x `geWord#` y
  (>)  (W16# x) (W16# y)      = x `gtWord#` y
  max x@(W16# x#) y@(W16# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W16# x#) y@(W16# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }



instance Num Word16 where
  (W16# x) + (W16# y) = 
       W16# (intToWord16# (word2Int# x +# word2Int# y))
  (W16# x) - (W16# y) = 
       W16# (intToWord16# (word2Int# x -# word2Int# y))
  (W16# x) * (W16# y) = 
       W16# (intToWord16# (word2Int# x *# word2Int# y))
  negate w@(W16# x)  = 
       if x' ==# 0# 
        then w
        else W16# (int2Word# (0x10000# -# x'))
       where
        x' = word2Int# x
  abs x         = x
  signum        = signumReal
  fromInteger (S# i#)    = W16# (wordToWord16# (int2Word# i#))
  fromInteger (J# s# d#) = W16# (wordToWord16# (integer2Word# s# d#))
  fromInt       = intToWord16

instance Bounded Word16 where
  minBound = 0
  maxBound = 0xffff

instance Real Word16 where
  toRational x = toInteger x % 1

instance Integral Word16 where
  div  x@(W16# x#)  (W16# y#)
   | y# `neWord#` (int2Word# 0#) = W16# (x# `quotWord#` y#)
   | otherwise                   = divZeroError "div{Word16}" x

  quot x@(W16# x#) (W16# y#)
   | y# `neWord#`(int2Word# 0#)  = W16# (x# `quotWord#` y#)
   | otherwise                   = divZeroError "quot{Word16}" x

  rem  x@(W16# x#) (W16# y#)
   | y# `neWord#` (int2Word# 0#) = W16# (x# `remWord#` y#)
   | otherwise                   = divZeroError "rem{Word16}" x

  mod  x@(W16# x#)  (W16# y#)
   | y# `neWord#` (int2Word# 0#) = W16# (x# `remWord#` y#)
   | otherwise		         = divZeroError "mod{Word16}" x

  quotRem (W16# x) (W16# y) = (W16# (x `quotWord#` y), W16# (x `remWord#` y))
  divMod  (W16# x) (W16# y) = (W16# (x `quotWord#` y), W16# (x `remWord#` y))

  toInteger = toInteger . toInt
  toInt     = word16ToInt

instance Ix Word16 where
  range (m,n)          = [m..n]
  index b@(m,_) i
         | inRange b i = word16ToInt (i - m)
         | otherwise   = indexError b i "Word16"
  inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
    succ w	    
      | w == maxBound = succError "Word16"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word16"
      | otherwise     = w-1

    toEnum   i@(I# i#)  
      | i >= toInt (minBound::Word16) && i <= toInt (maxBound::Word16)
      = W16# (intToWord16# i#)
      | otherwise
      = toEnumError "Word16" i (minBound::Word16,maxBound::Word16)

    fromEnum  (W16# w) = I# (word2Int# w)
    enumFrom     = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance Read Word16 where
  readsPrec _ = readDec

instance Show Word16 where
  showsPrec _ = showInt
\end{code}

\subsection[Word32]{The @Word32@ interface}

The quad byte type @Word32@ is represented in the Haskell
heap by boxing up a machine word, @Word#@. An invariant
for this representation is that any bits above the lower
32 are {\em always} zeroed out. A consequence of this is that
operations that could possibly overflow have to mask
the result before building the resulting @Word16@.

\begin{code}
data Word32 = W32# Word#

instance CCallable Word32
instance CReturnable Word32

instance Eq  Word32    where 
  (W32# x) == (W32# y) = x `eqWord#` y
  (W32# x) /= (W32# y) = x `neWord#` y

instance Ord Word32    where
  compare (W32# x#) (W32# y#) = compareWord# x# y#
  (<)  (W32# x) (W32# y)      = x `ltWord#` y
  (<=) (W32# x) (W32# y)      = x `leWord#` y
  (>=) (W32# x) (W32# y)      = x `geWord#` y
  (>)  (W32# x) (W32# y)      = x `gtWord#` y
  max x@(W32# x#) y@(W32# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W32# x#) y@(W32# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Word32 where
  (W32# x) + (W32# y) = 
       W32# (intToWord32# (word2Int# x +# word2Int# y))
  (W32# x) - (W32# y) =
       W32# (intToWord32# (word2Int# x -# word2Int# y))
  (W32# x) * (W32# y) = 
       W32# (intToWord32# (word2Int# x *# word2Int# y))
#if WORD_SIZE_IN_BYTES == 8
  negate w@(W32# x)  = 
      if x' ==# 0#
       then w
       else W32# (intToWord32# (0x100000000# -# x'))
       where
        x' = word2Int# x
#else
  negate (W32# x)  = W32# (intToWord32# (negateInt# (word2Int# x)))
#endif
  abs x           = x
  signum          = signumReal
  fromInteger (S# i#)    = W32# (intToWord32# i#)
  fromInteger (J# s# d#) = W32# (wordToWord32# (integer2Word# s# d#))
  fromInt (I# x)  = W32# (intToWord32# x)
    -- ToDo: restrict fromInt{eger} range.

intToWord32#  :: Int#  -> Word#
wordToWord32# :: Word# -> Word#

#if WORD_SIZE_IN_BYTES == 8
intToWord32#  i#  = (int2Word# i#) `and#` (int2Word# 0xffffffff#)
wordToWord32# w#  = w# `and#` (int2Word# 0xffffffff#)
wordToWord64# w#  = w#
#else
intToWord32#  i# = int2Word# i#
wordToWord32# w# = w#
#endif

instance Bounded Word32 where
    minBound = 0
#if WORD_SIZE_IN_BYTES == 8
    maxBound = 0xffffffff
#else
    maxBound = minBound - 1
#endif

instance Real Word32 where
    toRational x = toInteger x % 1

instance Integral Word32 where
    div  x y 
      | y /= 0         = quotWord32 x y
      | otherwise      = divZeroError "div{Word32}" x

    quot x y
      | y /= 0         = quotWord32 x y
      | otherwise      = divZeroError "quot{Word32}" x

    rem	 x y
      | y /= 0         = remWord32 x y
      | otherwise      = divZeroError "rem{Word32}" x

    mod  x y
      | y /= 0         = remWord32 x y
      | otherwise      = divZeroError "mod{Word32}" x

    quotRem a b        = (a `quot` b, a `rem` b)
    divMod x y         = quotRem x y

    toInteger          = word32ToInteger 
    toInt              = word32ToInt


{-# INLINE quotWord32 #-}
{-# INLINE remWord32  #-}
remWord32, quotWord32 :: Word32 -> Word32 -> Word32
(W32# x) `quotWord32` (W32# y) = W32# (x `quotWord#` y)
(W32# x) `remWord32`  (W32# y) = W32# (x `remWord#`  y)


instance Ix Word32 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = word32ToInt (i - m)
	   | otherwise   = indexError b i "Word32"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    succ w	    
      | w == maxBound = succError "Word32"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word32"
      | otherwise     = w-1

     -- the toEnum/fromEnum will fail if the mapping isn't legal,
     -- use the intTo* & *ToInt coercion functions to 'bypass' these range checks.
    toEnum   x
      | x >= 0    = intToWord32 x
      | otherwise
      = toEnumError "Word32" x (minBound::Word32,maxBound::Word32)

    fromEnum   x
      | x <= intToWord32 (maxBound::Int)
      = word32ToInt x
      | otherwise
      = fromEnumError "Word32" x 

    enumFrom w           = [w .. maxBound]
    enumFromTo   w1 w2
       | w1 <= w2        = eftt32 True{-increasing-} w1 diff_f last
       | otherwise	 = []
	where
         last = (> w2)
	 diff_f x = x + 1 
	  
    enumFromThen w1 w2   = [w1,w2 .. last]
       where
	 last :: Word32
	 last
	  | w1 <=w2   = maxBound
	  | otherwise = minBound

    enumFromThenTo w1 w2 wend  = eftt32 increasing w1 step_f last
     where
       increasing = w1 <= w2
       diff1 = w2 - w1
       diff2 = w1 - w2
       
       last
        | increasing = (> wend)
	| otherwise  = (< wend)

       step_f 
        | increasing = \ x -> x + diff1
        | otherwise  = \ x -> x - diff2

eftt32 :: Bool -> Word32 -> (Word32 -> Word32) -> (Word32-> Bool) -> [Word32]
eftt32 increasing init stepper done = go init
  where
    go now
     | done now                    = []
     | increasing     && now > nxt = [now] -- oflow
     | not increasing && now < nxt = [now] -- uflow
     | otherwise                   = now : go nxt
     where
      nxt = stepper now 

instance Read Word32 where
    readsPrec _ = readDec

instance Show Word32 where
    showsPrec _ = showInt

-- -----------------------------------------------------------------------------
-- Word64
-- -----------------------------------------------------------------------------

#if WORD_SIZE_IN_BYTES == 8
--data Word64 = W64# Word#

word32ToWord64 (W32 w#) = W64# w#

word8ToWord64 (W8# w#) = W64# w#
word64ToWord8 (W64# w#) = W8# (w# `and#` (int2Word# 0xff#))

word16ToWord64 (W16# w#) = W64# w#
word64ToWord16 (W64# w#) = W16# (w# `and#` (int2Word# 0xffff#))

wordToWord32# :: Word# -> Word#
wordToWord32# w# = w# `and#` (case (maxBound::Word32) of W# x# -> x#)

word64ToWord32 :: Word64 -> Word32
word64ToWord32 (W64# w#) = W32# (wordToWord32# w#)

wordToWord64# w# = w#
word64ToWord# w# = w#

instance Eq  Word64     where 
  (W64# x) == (W64# y) = x `eqWord#` y
  (W64# x) /= (W64# y) = x `neWord#` y

instance Ord Word64     where 
  compare (W64# x#) (W64# y#) = compareWord# x# y#
  (<)  (W64# x) (W64# y)      = x `ltWord#` y
  (<=) (W64# x) (W64# y)      = x `leWord#` y
  (>=) (W64# x) (W64# y)      = x `geWord#` y
  (>)  (W64# x) (W64# y)      = x `gtWord#` y
  max x@(W64# x#) y@(W64# y#) = 
     case (compareWord# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W64# x#) y@(W64# y#) =
     case (compareWord# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Word64 where
  (W64# x) + (W64# y) = 
      W64# (intToWord64# (word2Int# x +# word2Int# y))
  (W64# x) - (W64# y) = 
      W64# (intToWord64# (word2Int# x -# word2Int# y))
  (W64# x) * (W64# y) = 
      W64# (intToWord64# (word2Int# x *# word2Int# y))
  negate w@(W64# x)  = 
     if x' ==# 0# 
      then w
      else W64# (int2Word# (0x100# -# x'))
     where
      x' = word2Int# x
  abs x         = x
  signum        = signumReal
  fromInteger (S# i#)    = W64# (int2Word# i#)
  fromInteger (J# s# d#) = W64# (integer2Word# s# d#)
  fromInt       = intToWord64

-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word64 where
  div  x@(W64# x#)  (W64# y#)
    | y# `neWord#` (int2Word# 0#)  = W64# (x# `quotWord#` y#)
    | otherwise                    = divZeroError "div{Word64}" x

  quot x@(W64# x#)  (W64# y#)
    | y# `neWord#` (int2Word# 0#)  = W64# (x# `quotWord#` y#)
    | otherwise                    = divZeroError "quot{Word64}" x

  rem  x@(W64# x#)  (W64# y#)
    | y# `neWord#` (int2Word# 0#)  = W64# (x# `remWord#` y#)
    | otherwise                    = divZeroError "rem{Word64}" x

  mod  (W64# x)  (W64# y)   
    | y# `neWord#` (int2Word# 0#)  = W64# (x `remWord#` y)
    | otherwise                    = divZeroError "mod{Word64}" x

  quotRem (W64# x) (W64# y) = (W64# (x `quotWord#` y), W64# (x `remWord#` y))
  divMod  (W64# x) (W64# y) = (W64# (x `quotWord#` y), W64# (x `remWord#` y))

  toInteger (W64# x)        = word2Integer# x
  toInt x                   = word64ToInt x

#else /* WORD_SIZE_IN_BYTES < 8 */

--defined in PrelCCall: data Word64 = W64 Word64# deriving (Eq, Ord, Bounded)

-- for completeness sake
word32ToWord64 (W32# w#) = W64# (wordToWord64# w#)
word64ToWord32 (W64# w#) = W32# (word64ToWord# w#)

word8ToWord64 (W8# w#) = W64# (wordToWord64# w#)
word64ToWord8 (W64# w#) = W8# ((word64ToWord# w#) `and#` (int2Word# 0xff#))

word16ToWord64 (W16# w#) = W64# (wordToWord64# w#)
word64ToWord16 (W64# w#) = W16# ((word64ToWord# w#) `and#` (int2Word# 0xffff#))

word64ToInteger (W64# w#) = 
  case word64ToInteger# w# of
    (# s#, p# #) -> J# s# p#
word64ToInt w = 
   case w `quotRem` 0x100000000 of 
     (_,l) -> toInt (word64ToWord32 l)

intToWord64# :: Int# -> Word64#
intToWord64# i# = wordToWord64# (int2Word# i#)

intToWord64 (I# i#) = W64# (intToWord64# i#)

integerToWord64 (S# i#)    = W64# (intToWord64# i#)
integerToWord64 (J# s# d#) = W64# (integerToWord64# s# d#)

instance Eq  Word64     where 
  (W64# x) == (W64# y) = x `eqWord64#` y
  (W64# x) /= (W64# y) = not (x `eqWord64#` y)

instance Ord Word64     where 
  compare (W64# x#) (W64# y#) = compareWord64# x# y#
  (<)  (W64# x) (W64# y)      = x `ltWord64#` y
  (<=) (W64# x) (W64# y)      = x `leWord64#` y
  (>=) (W64# x) (W64# y)      = x `geWord64#` y
  (>)  (W64# x) (W64# y)      = x `gtWord64#` y
  max x@(W64# x#) y@(W64# y#) = 
     case (compareWord64# x# y#) of { LT -> y ; EQ -> x ; GT -> x }
  min x@(W64# x#) y@(W64# y#) =
     case (compareWord64# x# y#) of { LT -> x ; EQ -> x ; GT -> y }

instance Num Word64 where
  (W64# x) + (W64# y) = 
      W64# (int64ToWord64# (word64ToInt64# x `plusInt64#` word64ToInt64# y))
  (W64# x) - (W64# y) = 
      W64# (int64ToWord64# (word64ToInt64# x `minusInt64#` word64ToInt64# y))
  (W64# x) * (W64# y) = 
      W64# (int64ToWord64# (word64ToInt64# x `timesInt64#` word64ToInt64# y))
  negate w
     | w == 0     = w
     | otherwise  = maxBound - w

  abs x         = x
  signum        = signumReal
  fromInteger i = integerToWord64 i
  fromInt       = intToWord64

-- Note: no need to mask results here  as they cannot overflow.
-- ToDo: protect against div by zero.
instance Integral Word64 where
  div  (W64# x)  (W64# y)   = W64# (x `quotWord64#` y)
  quot (W64# x)  (W64# y)   = W64# (x `quotWord64#` y)
  rem  (W64# x)  (W64# y)   = W64# (x `remWord64#` y)
  mod  (W64# x)  (W64# y)   = W64# (x `remWord64#` y)
  quotRem (W64# x) (W64# y) = (W64# (x `quotWord64#` y), W64# (x `remWord64#` y))
  divMod  (W64# x) (W64# y) = (W64# (x `quotWord64#` y), W64# (x `remWord64#` y))
  toInteger w64             = word64ToInteger w64
  toInt x                   = word64ToInt x

compareWord64# :: Word64# -> Word64# -> Ordering
compareWord64# i# j# 
 | i# `ltWord64#` j# = LT
 | i# `eqWord64#` j# = EQ
 | otherwise	     = GT

-- Word64# primop wrappers:

ltWord64# :: Word64# -> Word64# -> Bool
ltWord64# x# y# = stg_ltWord64 x# y# /= 0

leWord64# :: Word64# -> Word64# -> Bool
leWord64# x# y# = stg_leWord64 x# y# /= 0

eqWord64# :: Word64# -> Word64# -> Bool
eqWord64# x# y# = stg_eqWord64 x# y# /= 0
      
neWord64# :: Word64# -> Word64# -> Bool
neWord64# x# y# = stg_neWord64 x# y# /= 0
      
geWord64# :: Word64# -> Word64# -> Bool
geWord64# x# y# = stg_geWord64 x# y# /= 0
      
gtWord64# :: Word64# -> Word64# -> Bool
gtWord64# x# y# = stg_gtWord64 x# y# /= 0

plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# a# b# = case stg_plusInt64 a# b# of { I64# i# -> i# }

minusInt64# :: Int64# -> Int64# -> Int64#
minusInt64# a# b# = case stg_minusInt64 a# b# of { I64# i# -> i# }

timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# a# b# = case stg_timesInt64 a# b# of { I64# i# -> i# }

quotWord64# :: Word64# -> Word64# -> Word64#
quotWord64# a# b# = case stg_quotWord64 a# b# of { W64# w# -> w# }

remWord64# :: Word64# -> Word64# -> Word64#
remWord64# a# b# = case stg_remWord64 a# b# of { W64# w# -> w# }

negateInt64# :: Int64# -> Int64#
negateInt64# a# = case stg_negateInt64 a# of { I64# i# -> i# }

word64ToWord# :: Word64# -> Word#
word64ToWord# w64# = case stg_word64ToWord w64# of { W# w# -> w# }
      
wordToWord64# :: Word# -> Word64#
wordToWord64# w# = case stg_wordToWord64 w# of { W64# w64# -> w64# }

word64ToInt64# :: Word64# -> Int64#
word64ToInt64# w64# = case stg_word64ToInt64 w64# of { I64# i# -> i# }

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# i64# = case stg_int64ToWord64 i64# of { W64# w# -> w# }

intToInt64# :: Int# -> Int64#
intToInt64# i# = case stg_intToInt64 i# of { I64# i64# -> i64# }
      
foreign import "stg_intToInt64" unsafe stg_intToInt64 :: Int# -> Int64
foreign import "stg_int64ToWord64" unsafe stg_int64ToWord64 :: Int64# -> Word64
foreign import "stg_word64ToInt64" unsafe stg_word64ToInt64 :: Word64# -> Int64
foreign import "stg_wordToWord64" unsafe stg_wordToWord64 :: Word# -> Word64
foreign import "stg_word64ToWord" unsafe stg_word64ToWord :: Word64# -> Word
foreign import "stg_negateInt64" unsafe stg_negateInt64 :: Int64# -> Int64
foreign import "stg_remWord64" unsafe stg_remWord64 :: Word64# -> Word64# -> Word64
foreign import "stg_quotWord64" unsafe stg_quotWord64 :: Word64# -> Word64# -> Word64
foreign import "stg_timesInt64" unsafe stg_timesInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_minusInt64" unsafe stg_minusInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_plusInt64" unsafe stg_plusInt64 :: Int64# -> Int64# -> Int64
foreign import "stg_gtWord64" unsafe stg_gtWord64 :: Word64# -> Word64# -> Int
foreign import "stg_geWord64" unsafe stg_geWord64 :: Word64# -> Word64# -> Int
foreign import "stg_neWord64" unsafe stg_neWord64 :: Word64# -> Word64# -> Int
foreign import "stg_eqWord64" unsafe stg_eqWord64 :: Word64# -> Word64# -> Int
foreign import "stg_leWord64" unsafe stg_leWord64 :: Word64# -> Word64# -> Int
foreign import "stg_ltWord64" unsafe stg_ltWord64 :: Word64# -> Word64# -> Int

#endif

instance Enum Word64 where
    succ w	    
      | w == maxBound = succError "Word64"
      | otherwise     = w+1
    pred w	    
      | w == minBound = predError "Word64"
      | otherwise     = w-1

    toEnum i
      | i >= 0    = intToWord64 i
      | otherwise 
      = toEnumError "Word64" i (minBound::Word64,maxBound::Word64)

    fromEnum w
      | w <= intToWord64 (maxBound::Int)
      = word64ToInt w
      | otherwise
      = fromEnumError "Word64" w

    enumFrom e1        = map integerToWord64 [word64ToInteger e1 .. word64ToInteger maxBound]
    enumFromTo e1 e2   = map integerToWord64 [word64ToInteger e1 .. word64ToInteger e2]
    enumFromThen e1 e2 = map integerToWord64 [word64ToInteger e1, word64ToInteger e2 .. word64ToInteger last]
		       where 
			  last :: Word64
		          last 
			   | e2 < e1   = minBound
			   | otherwise = maxBound

    enumFromThenTo e1 e2 e3 = map integerToWord64 [word64ToInteger e1, word64ToInteger e2 .. word64ToInteger e3]

instance Show Word64 where
  showsPrec p x = showsPrec p (word64ToInteger x)

instance Read Word64 where
  readsPrec _ s = [ (integerToWord64 x,r) | (x,r) <- readDec s ]

instance Ix Word64 where
    range (m,n)          = [m..n]
    index b@(m,_) i
	   | inRange b i = word64ToInt (i-m)
	   | otherwise   = indexError b i "Word64"
    inRange (m,n) i      = m <= i && i <= n

instance Bounded Word64 where
  minBound = 0
  maxBound = minBound - 1

instance Real Word64 where
  toRational x = toInteger x % 1

-- -----------------------------------------------------------------------------
-- Reading/writing words to/from memory
-- -----------------------------------------------------------------------------

indexWord8OffAddr  :: Addr -> Int -> Word8
indexWord8OffAddr (A# a#) (I# i#) = W8# (indexWord8OffAddr# a# i#)

indexWord16OffAddr  :: Addr -> Int -> Word16
indexWord16OffAddr (A# a#) (I# i#) = W16# (indexWord16OffAddr# a# i#)

indexWord32OffAddr  :: Addr -> Int -> Word32
indexWord32OffAddr (A# a#) (I# i#) = W32# (indexWord32OffAddr# a# i#)

indexWord64OffAddr  :: Addr -> Int -> Word64
#if WORD_SIZE_IN_BYTES == 8
indexWord64OffAddr (A# a#) (I# i#) = W64# (indexWordOffAddr# a# i#)
#else
indexWord64OffAddr (A# a#) (I# i#) = W64# (indexWord64OffAddr# a# i#)
#endif


readWord8OffAddr :: Addr -> Int -> IO Word8
readWord8OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord8OffAddr# a i s of (# s, w #) -> (# s, W8# w #)

readWord16OffAddr :: Addr -> Int -> IO Word16
readWord16OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord16OffAddr# a i s of (# s, w #) -> (# s, W16# w #)

readWord32OffAddr :: Addr -> Int -> IO Word32
readWord32OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord32OffAddr# a i s of (# s, w #) -> (# s, W32# w #)

readWord64OffAddr  :: Addr -> Int -> IO Word64
#if WORD_SIZE_IN_BYTES == 8
readWord64OffAddr (A# a) (I# i)
  = IO $ \s -> case readWordOffAddr# a i s of (# s, w #) -> (# s, W64# w #)
#else
readWord64OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord64OffAddr# a i s of (# s, w #) -> (# s, W64# w #)
#endif


writeWord8OffAddr  :: Addr -> Int -> Word8  -> IO ()
writeWord8OffAddr (A# a#) (I# i#) (W8# w#) = IO $ \ s# ->
      case (writeWord8OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord16OffAddr  :: Addr -> Int -> Word16  -> IO ()
writeWord16OffAddr (A# a#) (I# i#) (W16# w#) = IO $ \ s# ->
      case (writeWord16OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord32OffAddr  :: Addr -> Int -> Word32  -> IO ()
writeWord32OffAddr (A# a#) (I# i#) (W32# w#) = IO $ \ s# ->
      case (writeWord32OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord64OffAddr :: Addr -> Int -> Word64 -> IO ()
#if WORD_SIZE_IN_BYTES == 8
writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#else
writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWord64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#endif
\end{code}

The Hugs-GHC extension libraries provide functions for going between
Int and the various (un)signed ints. Here we provide the same for
the GHC specific Word type:

\begin{code}
word8ToWord  (W8#  w#) = W# w#
wordToWord8  (W#   w#)  = W8# (w# `and#` (case (maxBound::Word8) of W8#   x# -> x#))

word16ToWord (W16# w#) = W# w#
wordToWord16 (W#   w#) = W16# (w# `and#` (case (maxBound::Word16) of W16# x# -> x#))

word32ToWord (W32# w#) = W# w#
wordToWord32 (W#   w#) = W32# (w# `and#` (case (maxBound::Word32) of W32# x# -> x#))

wordToWord64 (W#   w#) = W64# (wordToWord64# w#)
-- lossy on 32-bit platforms, but provided nontheless.
word64ToWord (W64# w#) = W#   (word64ToWord# w#)

word2Integer :: Word# -> Integer
word2Integer w | i >=# 0#   = S# i
               | otherwise = case word2Integer# w of
                                (# s, d #) -> J# s d
   where i = word2Int# w
\end{code}

Misc utils.

\begin{code}
signumReal :: (Ord a, Num a) => a -> a
signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1
\end{code}

Utils for generating friendly error messages.

\begin{code}
toEnumError :: (Show a,Show b) => String -> a -> (b,b) -> c
toEnumError inst_ty tag bnds
  = error ("Enum.toEnum{" ++ inst_ty ++ "}: tag " ++
           (showParen True (showsPrec 0 tag) $
	     " is outside of bounds " ++
	     show bnds))

fromEnumError :: (Show a,Show b) => String -> a -> b
fromEnumError inst_ty tag
  = error ("Enum.fromEnum{" ++ inst_ty ++ "}: value " ++
           (showParen True (showsPrec 0 tag) $
	     " is outside of Int's bounds " ++
	     show (minBound::Int,maxBound::Int)))

succError :: String -> a
succError inst_ty
  = error ("Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound")

predError :: String -> a
predError inst_ty
  = error ("Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound")

divZeroError :: (Show a) => String -> a -> b
divZeroError meth v 
  = error ("Integral." ++ meth ++ ": divide by 0 (" ++ show v ++ " / 0)")
\end{code}
