%
% (c) The AQUA Project, Glasgow University, 1997
%
\section[Word]{Module @Word@}

GHC implementation of the standard Hugs/GHC @Word@
interface, types and operations over unsigned, sized
quantities.

\begin{code}
#include "MachDeps.h"

module Word
	( Word8		 -- all abstract.
	, Word16         -- instances: Eq, Ord
	, Word32         --  Num, Bounded, Real,
	, Word64         --  Integral, Ix, Enum,
	                 --  Read, Show, Bits,
			 --  CCallable, CReturnable
			 --  (last two 

	, word8ToWord32   -- :: Word8  -> Word32
	, word32ToWord8   -- :: Word32 -> Word8
	, word16ToWord32  -- :: Word16 -> Word32
	, word32ToWord16  -- :: Word32 -> Word16

	, word8ToInt      -- :: Word8  -> Int
	, intToWord8      -- :: Int    -> Word8
	, word16ToInt     -- :: Word16 -> Int
	, intToWord16     -- :: Int    -> Word16
	, word32ToInt     -- :: Word32 -> Int
	, intToWord32     -- :: Int    -> Word32

	, word32ToWord64  -- :: Word32 -> Word64
	, word64ToWord32  -- :: Word64 -> Word32
        
        , word64ToInteger -- :: Word64  -> Integer
	, integerToWord64 -- :: Integer -> Word64

	-- NB! GHC SPECIFIC:
 	, wordToWord8     -- :: Word   -> Word8
	, word8ToWord     -- :: Word8  -> Word
	, wordToWord16    -- :: Word   -> Word16
	, word16ToWord    -- :: Word16 -> Word
	, wordToWord32    -- :: Word   -> Word32
	, word32ToWord    -- :: Word32 -> Word
	, wordToWord64    -- :: Word   -> Word64
	, word64ToWord    -- :: Word64 -> Word

	-- The "official" place to get these from is Addr.
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
	
	, sizeofWord8
	, sizeofWord16
	, sizeofWord32
	, sizeofWord64

	-- The "official" place to get these from is Foreign
#ifndef __PARALLEL_HASKELL__
	, indexWord8OffForeignObj
	, indexWord16OffForeignObj
	, indexWord32OffForeignObj
	, indexWord64OffForeignObj
	
	, readWord8OffForeignObj
	, readWord16OffForeignObj
	, readWord32OffForeignObj
	, readWord64OffForeignObj
	
	, writeWord8OffForeignObj
	, writeWord16OffForeignObj
	, writeWord32OffForeignObj
	, writeWord64OffForeignObj
#endif
	
	-- non-standard, GHC specific
	, wordToInt

	) where

#ifdef __HUGS__
import PreludeBuiltin
#else
import GlaExts
import CCall
import PrelForeign
import PrelIOBase
import PrelAddr
#endif
import Ix
import Bits
import Numeric (readDec, showInt)

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

intToWord32 (I# x)   = W32# ((int2Word# x) `and#` (case (maxBound::Word32) of W32# x# -> x#))
--intToWord32 (I# x)   = W32# (int2Word# x)
word32ToInt (W32# x) = I#   (word2Int# x)

wordToInt :: Word -> Int
wordToInt (W# w#) = I# (word2Int# w#)

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
  fromInteger (J# a# s# d#) = W8# (wordToWord8# (integer2Word# a# s# d#))
  fromInt       = intToWord8

instance Bounded Word8 where
  minBound = 0
  maxBound = 0xff

instance Real Word8 where
  toRational x = toInteger x % 1

-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word8 where
  div  (W8# x)  (W8# y)   = W8# (x `quotWord#` y)
  quot (W8# x)  (W8# y)   = W8# (x `quotWord#` y)
  rem  (W8# x)  (W8# y)   = W8# (x `remWord#` y)
  mod  (W8# x)  (W8# y)   = W8# (x `remWord#` y)
  quotRem (W8# x) (W8# y) = (W8# (x `quotWord#` y), W8# (x `remWord#` y))
  divMod  (W8# x) (W8# y) = (W8# (x `quotWord#` y), W8# (x `remWord#` y))
  toInteger (W8# x)       = word2Integer x
  toInt x                 = word8ToInt x

instance Ix Word8 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word8ToInt (i-m)
	   | otherwise   = error (showString "Ix{Word8}.index: Index " .
				  showParen True (showsPrec 0 i) .
                                  showString " out of range " $
				  showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    toEnum    (I# i)  = W8# (intToWord8# i)
    fromEnum  (W8# w) = I# (word2Int# w)
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word8)]
		       where last = if d < c then minBound else maxBound

instance Read Word8 where
    readsPrec p = readDec

instance Show Word8 where
    showsPrec p = showInt

--
-- Word8s are represented by an (unboxed) 32-bit Word.
-- The invariant is that the upper 24 bits are always zeroed out.
--
instance Bits Word8 where
  (W8# x)  .&.  (W8# y)    = W8# (x `and#` y)
  (W8# x)  .|.  (W8# y)    = W8# (x `or#` y)
  (W8# x) `xor` (W8# y)    = W8# (x `xor#` y)
  complement (W8# x)       = W8# (x `xor#` int2Word# 0xff#)
  shift (W8# x#) i@(I# i#)
	| i > 0     = W8# (wordToWord8# (shiftL# x# i#))
	| otherwise = W8# (wordToWord8# (shiftRL# x# (negateInt# i#)))
  w@(W8# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W8# ((wordToWord8# (shiftL# x i')) `or#`
	                     (shiftRL# (x `and#` 
			                (int2Word# (0x100# -# pow2# i2)))
			               i2))
	| otherwise = rotate w (I# (8# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 7#)
           i2 = 8# -# i'

  bit (I# i#)
	| i# >=# 0# && i# <=# 7# = W8# (wordToWord8# (shiftL# (int2Word# 1#) i#))
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W8# x#) (I# i#)
    | i# <# 8# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise             = False -- for now, this is really an error.

  bitSize  _    = 8
  isSigned _    = False

pow2# :: Int# -> Int#
pow2# x# = word2Int# (shiftL# (int2Word# 1#) x#)

word2Integer w = case word2Integer# w of
			(# a, s, d #) -> J# a s d

pow2_64# :: Int# -> Int64#
pow2_64# x# = word64ToInt64# (shiftL64# (wordToWord64# (int2Word# 1#)) x#)

sizeofWord8 :: Word32
sizeofWord8 = 1

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
  fromInteger (J# a# s# d#) = W16# (wordToWord16# (integer2Word# a# s# d#))
  fromInt       = intToWord16

instance Bounded Word16 where
  minBound = 0
  maxBound = 0xffff

instance Real Word16 where
  toRational x = toInteger x % 1

instance Integral Word16 where
  div  (W16# x)  (W16# y)   = W16# (x `quotWord#` y)
  quot (W16# x)  (W16# y)   = W16# (x `quotWord#` y)
  rem  (W16# x)  (W16# y)   = W16# (x `remWord#` y)
  mod  (W16# x)  (W16# y)   = W16# (x `remWord#` y)
  quotRem (W16# x) (W16# y) = (W16# (x `quotWord#` y), W16# (x `remWord#` y))
  divMod  (W16# x) (W16# y) = (W16# (x `quotWord#` y), W16# (x `remWord#` y))
  toInteger (W16# x)        = word2Integer x
  toInt x                   = word16ToInt x

instance Ix Word16 where
  range (m,n)          = [m..n]
  index b@(m,n) i
         | inRange b i = word16ToInt (i - m)
         | otherwise   = error (showString "Ix{Word16}.index: Index " .
				showParen True (showsPrec 0 i) .
                                showString " out of range " $
				showParen True (showsPrec 0 b) "")
  inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
  toEnum    (I# i)   = W16# (intToWord16# i)
  fromEnum  (W16# w) = I# (word2Int# w)
  enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word16)]
  enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word16)]
		       where last = if d < c then minBound else maxBound

instance Read Word16 where
  readsPrec p = readDec

instance Show Word16 where
  showsPrec p = showInt

instance Bits Word16 where
  (W16# x)  .&.  (W16# y)  = W16# (x `and#` y)
  (W16# x)  .|.  (W16# y)  = W16# (x `or#` y)
  (W16# x) `xor` (W16# y)  = W16# (x `xor#` y)
  complement (W16# x)      = W16# (x `xor#` int2Word# 0xffff#)
  shift (W16# x#) i@(I# i#)
	| i > 0     = W16# (wordToWord16# (shiftL# x# i#))
	| otherwise = W16# (shiftRL# x# (negateInt# i#))
  w@(W16# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W16# ((wordToWord16# (shiftL# x i')) `or#`
	                      (shiftRL# (x `and#` 
			                 (int2Word# (0x10000# -# pow2# i2)))
			                i2))
	| otherwise = rotate w (I# (16# +# i'))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 15#)
           i2 = 16# -# i'
  bit (I# i#)
	| i# >=# 0# && i# <=# 15# = W16# (shiftL# (int2Word# 1#) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W16# x#) (I# i#)
    | i# <# 16# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise             = False -- for now, this is really an error.

  bitSize  _    = 16
  isSigned _    = False


sizeofWord16 :: Word32
sizeofWord16 = 2

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
  fromInteger (J# a# s# d#) = W32# (integer2Word# a# s# d#)
  fromInt (I# x)  = W32# (intToWord32# x)
    -- ToDo: restrict fromInt{eger} range.

intToWord32#  :: Int#  -> Word#
wordToWord32# :: Word# -> Word#

#if WORD_SIZE_IN_BYTES == 8
intToWord32#  i# = (int2Word# i#) `and#` (int2Word# 0xffffffff)
wordToWord32# w# = w# `and#` (int2Word# 0xffffffff)
wordToWord64# w# = w#
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
    div  x y           =  quotWord32 x y
    quot x y           =  quotWord32 x y
    rem	 x y           =  remWord32 x y
    mod  x y           =  remWord32 x y
    quotRem a b        = (a `quotWord32` b, a `remWord32` b)
    divMod x y         = quotRem x y
    toInteger (W32# x) = word2Integer x
    toInt     (W32# x) = I# (word2Int# x)

{-# INLINE quotWord32 #-}
{-# INLINE remWord32  #-}
(W32# x) `quotWord32` (W32# y) = W32# (x `quotWord#` y)
(W32# x) `remWord32`  (W32# y) = W32# (x `remWord#`  y)

instance Ix Word32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (i - m)
	   | otherwise   = error (showString "Ix{Word32}.index: Index " .
				  showParen True (showsPrec 0 i) .
                                  showString " out of range " $
				  showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    toEnum                  = intToWord32
    fromEnum                = word32ToInt   -- lossy, don't use.
    enumFrom w              = [w .. maxBound]
    enumFromTo   w1 w2      
       | w1 > w2   = []
       | otherwise = eft32 w1 w2

    enumFromThen w1 w2   = [w1,w2 .. last]
       where
	 last
	  | w1 < w2   = maxBound::Word32
	  | otherwise = minBound

    enumFromThenTo w1 w2 wend  = eftt32 w1 stepWith
     where
       diff1 = w2 - w1
       diff2 = w1 - w2

       increasing = w2 > w1

       stepWith :: Word32 -> Maybe Word32
       stepWith x
         | increasing && x > nxt = Nothing --oflow.
         | wend <= x  = Nothing
	 | otherwise  = Just nxt
        where
	 nxt
	  | increasing = x + diff1
	  | otherwise  = x - diff2

eftt32 :: Word32 -> (Word32 -> Maybe Word32) -> [Word32]
eftt32 now stepper = go now
  where
    go now =
     case stepper now of
       Nothing -> [now]
       Just v  -> now : go v

eft32 :: Word32 -> Word32 -> [Word32]
eft32 now last = go now
  where 
   go x
    | x == last = [x]
    | otherwise = x:go (x+1)

instance Read Word32 where
    readsPrec p = readDec

instance Show Word32 where
    showsPrec p = showInt

instance Bits Word32 where
  (W32# x)  .&.  (W32# y)  = W32# (x `and#` y)
  (W32# x)  .|.  (W32# y)  = W32# (x `or#` y)
  (W32# x) `xor` (W32# y)  = W32# (x `xor#` y)
  complement (W32# x)      = W32# (x `xor#` mb#) where (W32# mb#) = maxBound
  shift (W32# x) i@(I# i#)
	| i > 0     = W32# (wordToWord32# (shiftL# x i#))
	| otherwise = W32# (shiftRL# x (negateInt# i#))
  w@(W32# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W32# ((wordToWord32# (shiftL# x i')) `or#`
	                      (shiftRL# (x `and#` 
			                (int2Word# (word2Int# maxBound# -# pow2# i2 +# 1#)))
			             i2))
	| otherwise = rotate w (I# (32# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 31#)
           i2 = 32# -# i'
           (W32# maxBound#) = maxBound

  bit (I# i#)
	| i# >=# 0# && i# <=# 31# = W32# (shiftL# (int2Word# 1#) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i        = x .|. bit i
  clearBit x i      = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W32# x#) (I# i#)
    | i# <# 32# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise             = False -- for now, this is really an error.
  bitSize  _        = 32
  isSigned _        = False

sizeofWord32 :: Word32
sizeofWord32 = 4
\end{code}

\subsection[Word64]{The @Word64@ interface}

\begin{code}
#if WORD_SIZE_IN_BYTES == 8
--data Word64 = W64# Word#

word32ToWord64 :: Word32 -> Word64
word32ToWord64 (W32 w#) = W64# w#

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
  fromInteger (J# a# s# d#) = W64# (integer2Word# a# s# d#)
  fromInt       = intToWord64

instance Bounded Word64 where
  minBound = 0
  maxBound = minBound - 1

instance Real Word64 where
  toRational x = toInteger x % 1

-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word64 where
  div  (W64# x)  (W64# y)   = W64# (x `quotWord#` y)
  quot (W64# x)  (W64# y)   = W64# (x `quotWord#` y)
  rem  (W64# x)  (W64# y)   = W64# (x `remWord#` y)
  mod  (W64# x)  (W64# y)   = W64# (x `remWord#` y)
  quotRem (W64# x) (W64# y) = (W64# (x `quotWord#` y), W64# (x `remWord#` y))
  divMod  (W64# x) (W64# y) = (W64# (x `quotWord#` y), W64# (x `remWord#` y))
  toInteger (W64# x)        = word2Integer# x
  toInt x                   = word64ToInt x

instance Ix Word64 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word64ToInt (i-m)
	   | otherwise   = error (showString "Ix{Word64}.index: Index " .
				  showParen True (showsPrec 0 i) .
                                  showString " out of range " $
				  showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word64 where
    toEnum    (I# i)        = W64# (intToWord# i)
    fromEnum  (W64# w)      = I# (word2Int# w)    -- lossy, don't use.
    enumFrom w              = eft64 w 1
    enumFromTo   w1 w2      = eftt64 w1 1 (> w2)
    enumFromThen w1 w2      = eftt64 w1 (w2 - w1) (>last)
        where 
	 last
	  | w1 < w2   = maxBound::Word64
	  | otherwise = minBound

instance Read Word64 where
    readsPrec p = readDec

instance Show Word64 where
    showsPrec p = showInt


instance Bits Word64 where
  (W64# x)  .&.  (W64# y)    = W64# (x `and#` y)
  (W64# x)  .|.  (W64# y)    = W64# (x `or#` y)
  (W64# x) `xor` (W64# y)    = W64# (x `xor#` y)
  complement (W64# x)        = W64# (x `xor#` (case (maxBound::Word64) of W64# x# -> x#))
  shift (W64# x#) i@(I# i#)
	| i > 0     = W64# (shiftL# x# i#)
	| otherwise = W64# (shiftRL# x# (negateInt# i#))

  w@(W64# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W64# (shiftL# x i') `or#`
	                      (shiftRL# (x `and#` 
			                (int2Word# (word2Int# maxBound# -# pow2# i2 +# 1#)))
			             i2))
	| otherwise = rotate w (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (W64# maxBound#) = maxBound

  bit (I# i#)
	| i# >=# 0# && i# <=# 63# = W64# (shiftL# (int2Word# 1#) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W64# x#) (I# i#)
    | i# <# 64# && i# >=# 0# = (word2Int# (x# `and#` (shiftL# (int2Word# 1#) i#))) /=# 0#
    | otherwise              = False -- for now, this is really an error.

  bitSize  _    = 64
  isSigned _    = False

#else
--defined in PrelCCall: data Word64 = W64 Word64# deriving (Eq, Ord, Bounded)

-- for completeness sake
word32ToWord64 :: Word32 -> Word64
word32ToWord64 (W32# w#) = W64# (wordToWord64# w#)

word64ToWord32 :: Word64 -> Word32
word64ToWord32 (W64# w#) = W32# (word64ToWord# w#)

word64ToInteger :: Word64 -> Integer
word64ToInteger (W64# w#) = 
  case word64ToInteger# w# of
    (# a#, s#, p# #) -> J# a# s# p#

word64ToInt :: Word64 -> Int
word64ToInt w = 
   case w `quotRem` 0x100000000 of 
     (h,l) -> toInt (word64ToWord32 l)

intToWord64# :: Int# -> Word64#
intToWord64# i# = wordToWord64# (int2Word# i#)

intToWord64 :: Int -> Word64
intToWord64 (I# i#) = W64# (intToWord64# i#)

integerToWord64 :: Integer -> Word64
integerToWord64 (J# a# s# d#) = W64# (integerToWord64# a# s# d#)

instance Show Word64 where
  showsPrec p x = showsPrec p (word64ToInteger x)

instance Read Word64 where
  readsPrec p s = [ (integerToWord64 x,r) | (x,r) <- readDec s ]

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

instance Bounded Word64 where
  minBound = 0
  maxBound = minBound - 1

instance Real Word64 where
  toRational x = toInteger x % 1

-- Note: no need to mask results here 
-- as they cannot overflow.
instance Integral Word64 where
  div  (W64# x)  (W64# y)   = W64# (x `quotWord64#` y)
  quot (W64# x)  (W64# y)   = W64# (x `quotWord64#` y)
  rem  (W64# x)  (W64# y)   = W64# (x `remWord64#` y)
  mod  (W64# x)  (W64# y)   = W64# (x `remWord64#` y)
  quotRem (W64# x) (W64# y) = (W64# (x `quotWord64#` y), W64# (x `remWord64#` y))
  divMod  (W64# x) (W64# y) = (W64# (x `quotWord64#` y), W64# (x `remWord64#` y))
  toInteger w64             = word64ToInteger w64
  toInt x                   = word64ToInt x


instance Ix Word64 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word64ToInt (i-m)
	   | otherwise   = error (showString "Ix{Word64}.index: Index " .
				  showParen True (showsPrec 0 i) .
                                  showString " out of range " $
				  showParen True (showsPrec 0 b) "")
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word64 where
    toEnum    (I# i)        = W64# (intToWord64# i)
    fromEnum  (W64# w)      = I# (word2Int# (word64ToWord# w))  -- lossy, don't use.
    enumFrom w              = eft64 w 1
    enumFromTo   w1 w2      = eftt64 w1 1 (> w2)
    enumFromThen w1 w2      = eftt64 w1 (w2 - w1) (>last)
        where 
	 last
	  | w1 < w2   = maxBound::Word64
	  | otherwise = minBound

instance Bits Word64 where
  (W64# x)  .&.  (W64# y)    = W64# (x `and64#` y)
  (W64# x)  .|.  (W64# y)    = W64# (x `or64#` y)
  (W64# x) `xor` (W64# y)    = W64# (x `xor64#` y)
  complement (W64# x)        = W64# (x `xor64#` (case (maxBound::Word64) of W64# x# -> x#))
  shift (W64# x#) i@(I# i#)
	| i > 0     = W64# (shiftL64# x# i#)
	| otherwise = W64# (shiftRL64# x# (negateInt# i#))

  w@(W64# x)  `rotate` (I# i)
        | i ==# 0#    = w
	| i ># 0#     = W64# ((shiftL64# x i') `or64#`
	                      (shiftRL64# (x `and64#` 
			                   (int64ToWord64# ((word64ToInt64# maxBound#) `minusInt64#` 
						           (pow2_64# i2 `plusInt64#` (intToInt64# 1#))))))
			             i2)
	| otherwise = rotate w (I# (64# +# i))
          where
           i' = word2Int# (int2Word# i `and#` int2Word# 63#)
           i2 = 64# -# i'
           (W64# maxBound#) = maxBound

  bit (I# i#)
	| i# >=# 0# && i# <=# 63# = W64# (shiftL64# (wordToWord64# (int2Word# 1#)) i#)
	| otherwise = 0 -- We'll be overbearing, for now..

  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i

  testBit (W64# x#) (I# i#)
    | i# <# 64# && i# >=# 0# = (word2Int# (word64ToWord# (x# `and64#` (shiftL64# (wordToWord64# (int2Word# 1#)) i#)))) /=# 0#
    | otherwise              = False -- for now, this is really an error.

  bitSize  _    = 64
  isSigned _    = False

compareWord64# i# j# 
 | i# `ltWord64#` j# = LT
 | i# `eqWord64#` j# = EQ
 | otherwise	     = GT

-- Word64# primop wrappers:

ltWord64# :: Word64# -> Word64# -> Bool
ltWord64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_ltWord64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
leWord64# :: Word64# -> Word64# -> Bool
leWord64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_leWord64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
eqWord64# :: Word64# -> Word64# -> Bool
eqWord64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_eqWord64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
neWord64# :: Word64# -> Word64# -> Bool
neWord64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_neWord64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
geWord64# :: Word64# -> Word64# -> Bool
geWord64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_geWord64 x# y# 
	case (v::Int) of
	  0 -> return False
	  _ -> return True
      
gtWord64# :: Word64# -> Word64# -> Bool
gtWord64# x# y# =  unsafePerformIO $ do
	v <- _ccall_ stg_gtWord64 x# y# 
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

quotWord64# :: Word64# -> Word64# -> Word64#
quotWord64# a# b# =
  case (unsafePerformIO (_ccall_ stg_quotWord64 a# b#)) of
    W64# w# -> w#

remWord64# :: Word64# -> Word64# -> Word64#
remWord64# a# b# =
  case (unsafePerformIO (_ccall_ stg_remWord64 a# b#)) of
    W64# w# -> w#

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

shiftRL64# :: Word64# -> Int# -> Word64#
shiftRL64# a# b# =
  case (unsafePerformIO (_ccall_ stg_shiftRL64 a# b#)) of
    W64# w# -> w#

word64ToWord# :: Word64# -> Word#
word64ToWord# w# =
  case (unsafePerformIO (_ccall_ stg_word64ToWord w#)) of
    W# w# -> w#
      
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

sizeofWord64 :: Word32
sizeofWord64 = 8

-- Enum Word64 helper funs:

eftt64 :: Word64 -> Word64 -> (Word64->Bool) -> [Word64]
eftt64 now step done = go now
  where
   go now
     | done now  = []
     | otherwise = now : go (now+step)

eft64 :: Word64 -> Word64 -> [Word64]
eft64 now step = go now
  where 
   go x
    | x == maxBound = [x]
    | otherwise     = x:go (x+step)
\end{code}



The Hugs-GHC extension libraries provide functions for going between
Int and the various (un)signed ints. Here we provide the same for
the GHC specific Word type:

\begin{code}
wordToWord8  :: Word -> Word8
word8ToWord  :: Word8 -> Word
wordToWord16 :: Word -> Word16
word16ToWord :: Word16 -> Word
wordToWord32 :: Word -> Word32
word32ToWord :: Word32 -> Word

word8ToWord (W8# w#)   = W# w#
wordToWord8 (W# w#)    = W8# (w# `and#` (case (maxBound::Word8) of W8# x# -> x#))
word16ToWord (W16# w#) = W# w#
wordToWord16 (W# w#)   = W16# (w# `and#` (case (maxBound::Word16) of W16# x# -> x#))
word32ToWord (W32# w#) = W# w#
wordToWord32 (W# w#)   = W32# (w# `and#` (case (maxBound::Word32) of W32# x# -> x#))

wordToWord64  :: Word -> Word64
wordToWord64 (W# w#) = W64# (wordToWord64# w#)

-- lossy on 32-bit platforms, but provided nontheless.
word64ToWord :: Word64 -> Word
word64ToWord (W64# w#) = W# (word64ToWord# w#)

\end{code}


--End of exported definitions

The remainder of this file consists of definitions which are only
used in the implementation.

\begin{code}
signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

\end{code}

NOTE: the index is in units of the size of the type, *not* bytes.

\begin{code}
indexWord8OffAddr  :: Addr -> Int -> Word8
indexWord8OffAddr (A# a#) (I# i#) = intToWord8 (I# (ord# (indexCharOffAddr# a# i#)))

indexWord16OffAddr :: Addr -> Int -> Word16
indexWord16OffAddr a i =
#ifdef WORDS_BIGENDIAN
  intToWord16 ( word8ToInt l + (word8ToInt maxBound) * word8ToInt h)
#else
  intToWord16 ( word8ToInt h + (word8ToInt maxBound) * word8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexWord8OffAddr a byte_idx
   h = indexWord8OffAddr a (byte_idx+1)

indexWord32OffAddr :: Addr -> Int -> Word32
indexWord32OffAddr (A# a#) i = wordToWord32 (W# (indexWordOffAddr# a# i'#))
 where
   -- adjust index to be in Word units, not Word32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexWord64OffAddr :: Addr -> Int -> Word64
indexWord64OffAddr (A# a#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = W64# (indexWordOffAddr# a# i#)
#else
 = W64# (indexWord64OffAddr# a# i#)
#endif

#ifndef __PARALLEL_HASKELL__

indexWord8OffForeignObj  :: ForeignObj -> Int -> Word8
indexWord8OffForeignObj (ForeignObj fo#) (I# i#) = intToWord8 (I# (ord# (indexCharOffForeignObj# fo# i#)))

indexWord16OffForeignObj :: ForeignObj -> Int -> Word16
indexWord16OffForeignObj fo i =
#ifdef WORDS_BIGENDIAN
  intToWord16 ( word8ToInt l + (word8ToInt maxBound) * word8ToInt h)
#else
  intToWord16 ( word8ToInt h + (word8ToInt maxBound) * word8ToInt l)
#endif
 where
   byte_idx = i * 2
   l = indexWord8OffForeignObj fo byte_idx
   h = indexWord8OffForeignObj fo (byte_idx+1)

indexWord32OffForeignObj :: ForeignObj -> Int -> Word32
indexWord32OffForeignObj (ForeignObj fo#) i = wordToWord32 (W# (indexWordOffForeignObj# fo# i'#))
 where
   -- adjust index to be in Word units, not Word32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

indexWord64OffForeignObj :: ForeignObj -> Int -> Word64
indexWord64OffForeignObj (ForeignObj fo#) (I# i#)
#if WORD_SIZE_IN_BYTES==8
 = W64# (indexWordOffForeignObj# fo# i#)
#else
 = W64# (indexWord64OffForeignObj# fo# i#)
#endif
#endif

\end{code}

Read words out of mutable memory:

\begin{code}
readWord8OffAddr :: Addr -> Int -> IO Word8
readWord8OffAddr a i = _casm_ `` %r=(StgNat8)(((StgNat8*)%0)[(StgInt)%1]); '' a i

readWord16OffAddr  :: Addr -> Int -> IO Word16
readWord16OffAddr a i = _casm_ `` %r=(StgNat16)(((StgNat16*)%0)[(StgInt)%1]); '' a i

readWord32OffAddr  :: Addr -> Int -> IO Word32
readWord32OffAddr a i = _casm_ `` %r=(StgNat32)(((StgNat32*)%0)[(StgInt)%1]); '' a i

readWord64OffAddr  :: Addr -> Int -> IO Word64
#if WORD_SIZE_IN_BYTES==8
readWord64OffAddr a i = _casm_ `` %r=(StgWord)(((StgWord*)%0)[(StgInt)%1]); '' a i
#else
readWord64OffAddr a i = _casm_ `` %r=(StgNat64)(((StgNat64*)%0)[(StgInt)%1]); '' a i
#endif

#ifndef __PARALLEL_HASKELL__
readWord8OffForeignObj :: ForeignObj -> Int -> IO Word8
readWord8OffForeignObj fo i = _casm_ `` %r=(StgNat8)(((StgNat8*)%0)[(StgInt)%1]); '' fo i

readWord16OffForeignObj  :: ForeignObj -> Int -> IO Word16
readWord16OffForeignObj fo i = _casm_ `` %r=(StgNat16)(((StgNat16*)%0)[(StgInt)%1]); '' fo i

readWord32OffForeignObj  :: ForeignObj -> Int -> IO Word32
readWord32OffForeignObj fo i = _casm_ `` %r=(StgNat32)(((StgNat32*)%0)[(StgInt)%1]); '' fo i

readWord64OffForeignObj  :: ForeignObj -> Int -> IO Word64
#if WORD_SIZE_IN_BYTES==8
readWord64OffForeignObj fo i = _casm_ `` %r=(StgWord)(((StgWord*)%0)[(StgInt)%1]); '' fo i
#else
readWord64OffForeignObj fo i = _casm_ `` %r=(StgNat64)(((StgNat64*)%0)[(StgInt)%1]); '' fo i
#endif

#endif 

\end{code}

Note: we provide primops for the writing via Addrs since that's used
in the IO implementation (a place where we *really* do care about cycles.)

\begin{code}
writeWord8OffAddr  :: Addr -> Int -> Word8  -> IO ()
writeWord8OffAddr (A# a#) (I# i#) (W8# w#) = IO $ \ s# ->
      case (writeCharOffAddr# a# i# (chr# (word2Int# w#)) s#) of s2# -> (# s2#, () #)

writeWord16OffAddr :: Addr -> Int -> Word16 -> IO ()
writeWord16OffAddr a i e = _casm_ `` (((StgNat16*)%0)[(StgInt)%1])=(StgNat16)%2; '' a i e

writeWord32OffAddr :: Addr -> Int -> Word32 -> IO ()
writeWord32OffAddr (A# a#) i@(I# i#) (W32# w#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i'# w# s#) of s2# -> (# s2#, () #)
 where
   -- adjust index to be in Word units, not Word32 ones.
  (I# i'#) 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

writeWord64OffAddr :: Addr -> Int -> Word64 -> IO ()
#if WORD_SIZE_IN_BYTES==8
writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#else
writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWord64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
#endif

#ifndef __PARALLEL_HASKELL__

writeWord8OffForeignObj  :: ForeignObj -> Int -> Word8  -> IO ()
writeWord8OffForeignObj fo i w = _casm_ `` (((StgNat16*)%0)[(StgInt)%1])=(StgNat16)%2; '' fo i w

writeWord16OffForeignObj :: ForeignObj -> Int -> Word16 -> IO ()
writeWord16OffForeignObj fo i w = _casm_ `` (((StgNat16*)%0)[(StgInt)%1])=(StgNat16)%2; '' fo i w

writeWord32OffForeignObj :: ForeignObj -> Int -> Word32 -> IO ()
writeWord32OffForeignObj fo i w = _casm_ `` (((StgNat16*)%0)[(StgInt)%1])=(StgNat16)%2; '' fo i' w
 where
   -- adjust index to be in Word units, not Word32 ones.
  i' 
#if WORD_SIZE_IN_BYTES==8
   = i `div` 2
#else
   = i
#endif

writeWord64OffForeignObj :: ForeignObj -> Int -> Word64 -> IO ()
# if WORD_SIZE_IN_BYTES==8
writeWord64OffForeignObj fo i e = _casm_ `` (((StgWord*)%0)[(StgInt)%1])=(StgWord)%2; '' fo i e
# else
writeWord64OffForeignObj fo i e = _casm_ `` (((StgNat64*)%0)[(StgInt)%1])=(StgNat64)%2; '' fo i e
# endif

#endif

\end{code}
