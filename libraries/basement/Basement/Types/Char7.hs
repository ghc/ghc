{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE BangPatterns               #-}
module Basement.Types.Char7
    ( Char7(..)
    , toChar
    , fromCharMask
    , fromChar
    , fromByteMask
    , fromByte
    -- * individual ASCII Characters
    , c7_LF
    , c7_CR
    , c7_minus
    , c7_a
    , c7_A
    , c7_z
    , c7_Z
    , c7_0
    , c7_1
    , c7_2
    , c7_3
    , c7_4
    , c7_5
    , c7_6
    , c7_7
    , c7_8
    , c7_9
    -- * Upper / Lower With ASCII
    , c7Upper
    , c7Lower
    ) where

import GHC.Prim
import GHC.Word
import GHC.Types
import Data.Bits
import Data.Maybe
import Basement.Compat.Base
import Basement.Compat.Primitive (bool#)

-- | ASCII value between 0x0 and 0x7f
newtype Char7 = Char7 { toByte :: Word8 }
    deriving (Show,Eq,Ord,Typeable)

-- | Convert a 'Char7' to a unicode code point 'Char'
toChar :: Char7 -> Char
toChar !(Char7 (W8# w)) = C# (chr# (word2Int# w))

-- | Try to convert a 'Char' to a 'Char7'
-- 
-- If the code point is non ascii, then Nothing is returned.
fromChar :: Char -> Maybe Char7
fromChar !(C# c#)
    | bool# (ltChar# c# (chr# 0x80#)) = Just $ Char7 $ W8# (int2Word# (ord# c#))
    | otherwise                       = Nothing

-- | Try to convert 'Word8' to a 'Char7'
--
-- If the byte got higher bit set, then Nothing is returned.
fromByte :: Word8 -> Maybe Char7
fromByte !w
    | (w .&. 0x80) == 0 = Just $ Char7 w
    | otherwise         = Nothing

-- | Convert a 'Char' to a 'Char7' ignoring all higher bits
fromCharMask :: Char -> Char7
fromCharMask !(C# c#) = Char7 $ W8# (and# (int2Word# (ord# c#)) 0x7f##)

-- | Convert a 'Byte' to a 'Char7' ignoring the higher bit
fromByteMask :: Word8 -> Char7
fromByteMask !(W8# w#) = Char7 $ W8# (and# w# 0x7f##)

c7_LF :: Char7
c7_LF = Char7 0xa

c7_CR :: Char7
c7_CR = Char7 0xd

c7_minus :: Char7
c7_minus = Char7 0x2d

c7_a :: Char7
c7_a = Char7 0x61

c7_A :: Char7
c7_A = Char7 0x41

c7_z :: Char7
c7_z = Char7 0x7a

c7_Z :: Char7
c7_Z = Char7 0x5a

c7_0, c7_1, c7_2, c7_3, c7_4, c7_5, c7_6, c7_7, c7_8, c7_9 :: Char7
c7_0 = Char7 0x30
c7_1 = Char7 0x31
c7_2 = Char7 0x32
c7_3 = Char7 0x33
c7_4 = Char7 0x34
c7_5 = Char7 0x35
c7_6 = Char7 0x36
c7_7 = Char7 0x37
c7_8 = Char7 0x38
c7_9 = Char7 0x39

c7Lower :: Char7 -> Char7
c7Lower c@(Char7 w)
    | c <  c7_A = c
    | c <= c7_Z = Char7 (w .|. 0x20)
    | otherwise = c

c7Upper :: Char7 -> Char7
c7Upper c@(Char7 w)
    | c <  c7_a = c
    | c <= c7_z = Char7 (w .&. 0xdf)
    | otherwise = c
