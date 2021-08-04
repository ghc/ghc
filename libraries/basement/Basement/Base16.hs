{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns  #-}
module Basement.Base16
    ( unsafeConvertByte
    , hexWord16
    , hexWord32
    , escapeByte
    , Base16Escape(..)
    ) where

import GHC.Prim
import GHC.Types
import GHC.Word
import Basement.Types.Char7

data Base16Escape = Base16Escape {-# UNPACK #-} !Char7 {-# UNPACK #-} !Char7

-- | Convert a byte value in Word# to two Word#s containing
-- the hexadecimal representation of the Word#
--
-- The output words# are guaranteed to be included in the 0 to 2^7-1 range
--
-- Note that calling convertByte with a value greater than 256
-- will cause segfault or other horrible effect.
unsafeConvertByte :: Word# -> (# Word#, Word# #)
unsafeConvertByte b = (# r tableHi b, r tableLo b #)
  where
    r :: Table -> Word# -> Word#
    r (Table !table) index = indexWord8OffAddr# table (word2Int# index)
{-# INLINE unsafeConvertByte #-}

escapeByte :: Word8 -> Base16Escape
escapeByte !(W8# b) = Base16Escape (r tableHi b) (r tableLo b)
  where
    r :: Table -> Word# -> Char7
    r (Table !table) index = Char7 (W8# (indexWord8OffAddr# table (word2Int# index)))
{-# INLINE escapeByte #-}

-- | hex word16
hexWord16 :: Word16 -> (Char, Char, Char, Char)
hexWord16 (W16# w) = (toChar w1,toChar w2,toChar w3,toChar w4)
  where
    toChar :: Word# -> Char
    toChar c = C# (chr# (word2Int# c))
    !(# w1, w2 #) = unsafeConvertByte (uncheckedShiftRL# w 8#)
    !(# w3, w4 #) = unsafeConvertByte (and# w 0xff##)

-- | hex word32
hexWord32 :: Word32 -> (Char, Char, Char, Char, Char, Char, Char, Char)
hexWord32 (W32# w) = (toChar w1,toChar w2,toChar w3,toChar w4
                     ,toChar w5,toChar w6,toChar w7,toChar w8)
  where
    toChar :: Word# -> Char
    toChar c = C# (chr# (word2Int# c))
    !(# w1, w2 #) = unsafeConvertByte (uncheckedShiftRL# w 24#)
    !(# w3, w4 #) = unsafeConvertByte (and# (uncheckedShiftRL# w 16#) 0xff##)
    !(# w5, w6 #) = unsafeConvertByte (and# (uncheckedShiftRL# w 8#) 0xff##)
    !(# w7, w8 #) = unsafeConvertByte (and# w 0xff##)

data Table = Table Addr#

tableLo:: Table
tableLo = Table
    "0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef"#

tableHi :: Table
tableHi = Table
    "00000000000000001111111111111111\
    \22222222222222223333333333333333\
    \44444444444444445555555555555555\
    \66666666666666667777777777777777\
    \88888888888888889999999999999999\
    \aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb\
    \ccccccccccccccccdddddddddddddddd\
    \eeeeeeeeeeeeeeeeffffffffffffffff"#

