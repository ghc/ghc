%
% (c) The AQUA Project, Glasgow University, 1997
%
\section[Word]{Module @Word@}

GHC implementation of the standard Hugs/GHC @Word@
interface, types and operations over unsigned, sized
quantities.

\begin{code}
module Word
	( Word8		 -- all abstract.
	, Word16         -- instances: Eq, Ord
	, Word32         --  Num, Bounded, Real,
	, Word64         --  Integral, Ix, Enum,
	                 --  Read, Show, Bits,
			 --  CCallable, CReturnable
			 --  (last two 

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

import GlaExts
import Ix
import Bits
import CCall
import Numeric (readDec)

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
  toInteger (W8# x)       = word2Integer# x
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
  toInteger (W16# x)        = word2Integer# x
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
#if WORD_SIZE_IN_BYTES > 4
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

#if WORD_SIZE_IN_BYTES > 4
intToWord32#  i# = (int2Word# i#) `and#` (int2Word# 0xffffffff)
wordToWord32# w# = w# `and#` (int2Word# 0xffffffff)
#else
intToWord32#  i# = int2Word# i#
wordToWord32# w# = w#
#endif

instance Bounded Word32 where
    minBound = 0
#if WORD_SIZE_IN_BYTES > 4
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
    toInteger (W32# x) = word2Integer# x
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
    toEnum        = intToWord32
    fromEnum      = word32ToInt
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word32)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word32)]
		       where last = if d < c then minBound else maxBound

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

\end{code}

\subsection[Word64]{The @Word64@ interface}

\begin{code}
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
-- Code copied from the Prelude
-----------------------------------------------------------------------------

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

-- showInt is used for positive numbers only
-- stolen from Hugs prelude --SDM
showInt    :: Integral a => a -> ShowS
showInt n r | n < 0 = error "Word.showInt: can't show negative numbers"
            | otherwise =
              let (n',d) = quotRem n 10
		  r'     = toEnum (fromEnum '0' + fromIntegral d) : r
	      in  if n' == 0 then r' else showInt n' r'

\end{code}
