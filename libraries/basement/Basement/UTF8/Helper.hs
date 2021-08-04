-- |
-- Module      : Basement.UTF8.Helper
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Some low level helpers to use UTF8
--
-- Most helpers are lowlevel and unsafe, don't use
-- directly.
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Basement.UTF8.Helper
    where

import           Basement.Compat.Base
import           Basement.Compat.Primitive
import           Basement.Types.OffsetSize
import           Basement.UTF8.Types
import           GHC.Prim
import           GHC.Types
import           GHC.Word

-- mask an UTF8 continuation byte (stripping the leading 10 and returning 6 valid bits)
maskContinuation# :: Word# -> Word#
maskContinuation# v = and# v 0x3f##
{-# INLINE maskContinuation# #-}

-- mask a UTF8 header for 2 bytes encoding (110xxxxx and 5 valid bits)
maskHeader2# :: Word# -> Word#
maskHeader2# h = and# h 0x1f##
{-# INLINE maskHeader2# #-}

-- mask a UTF8 header for 3 bytes encoding (1110xxxx and 4 valid bits)
maskHeader3# :: Word# -> Word#
maskHeader3# h = and# h 0xf##
{-# INLINE maskHeader3# #-}

-- mask a UTF8 header for 4 bytes encoding (11110xxx and 3 valid bits)
maskHeader4# :: Word# -> Word#
maskHeader4# h = and# h 0x7##
{-# INLINE maskHeader4# #-}

or3# :: Word# -> Word# -> Word# -> Word#
or3# a b c = or# a (or# b c)
{-# INLINE or3# #-}

or4# :: Word# -> Word# -> Word# -> Word# -> Word#
or4# a b c d = or# (or# a b) (or# c d)
{-# INLINE or4# #-}

toChar# :: Word# -> Char
toChar# w = C# (chr# (word2Int# w))
{-# INLINE toChar# #-}

toChar1 :: StepASCII -> Char
toChar1 (StepASCII (W8# w)) = toChar# w

toChar2 :: StepASCII -> Word8 -> Char
toChar2 (StepASCII (W8# w1)) (W8# w2) =
    toChar# (or# (uncheckedShiftL# (maskHeader2# w1) 6#) (maskContinuation# w2))

toChar3 :: StepASCII -> Word8 -> Word8 -> Char
toChar3 (StepASCII (W8# w1)) (W8# w2) (W8# w3) =
    toChar# (or3# (uncheckedShiftL# (maskHeader3# w1) 12#)
                  (uncheckedShiftL# (maskContinuation# w2) 6#)
                  (maskContinuation# w3)
            )

toChar4 :: StepASCII -> Word8 -> Word8 -> Word8 -> Char
toChar4 (StepASCII (W8# w1)) (W8# w2) (W8# w3) (W8# w4) =
    toChar# (or4# (uncheckedShiftL# (maskHeader4# w1) 18#)
                  (uncheckedShiftL# (maskContinuation# w2) 12#)
                  (uncheckedShiftL# (maskContinuation# w3) 6#)
                  (maskContinuation# w4)
            )

-- | Different way to encode a Character in UTF8 represented as an ADT
data UTF8Char =
      UTF8_1 {-# UNPACK #-} !Word8
    | UTF8_2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | UTF8_3 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | UTF8_4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

-- | Transform a Unicode code point 'Char' into
--
-- note that we expect here a valid unicode code point in the *allowed* range.
-- bits will be lost if going above 0x10ffff
asUTF8Char :: Char -> UTF8Char
asUTF8Char !(C# c)
  | bool# (ltWord# x 0x80##   ) = encode1
  | bool# (ltWord# x 0x800##  ) = encode2
  | bool# (ltWord# x 0x10000##) = encode3
  | otherwise                   = encode4
    where
      !x = int2Word# (ord# c)

      encode1 = UTF8_1 (W8# x)
      encode2 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 6#) 0xc0##)
              !x2 = toContinuation x
           in UTF8_2 x1 x2
      encode3 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 12#) 0xe0##)
              !x2 = toContinuation (uncheckedShiftRL# x 6#)
              !x3 = toContinuation x
           in UTF8_3 x1 x2 x3
      encode4 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 18#) 0xf0##)
              !x2 = toContinuation (uncheckedShiftRL# x 12#)
              !x3 = toContinuation (uncheckedShiftRL# x 6#)
              !x4 = toContinuation x
           in UTF8_4 x1 x2 x3 x4

      toContinuation :: Word# -> Word8
      toContinuation w = W8# (or# (and# w 0x3f##) 0x80##)
      {-# INLINE toContinuation #-}

-- given the encoding of UTF8 Char, get the number of bytes of this sequence
numBytes :: UTF8Char -> CountOf Word8
numBytes UTF8_1{} = CountOf 1
numBytes UTF8_2{} = CountOf 2
numBytes UTF8_3{} = CountOf 3
numBytes UTF8_4{} = CountOf 4

-- given the leading byte of a utf8 sequence, get the number of bytes of this sequence
skipNextHeaderValue :: Word8 -> CountOf Word8
skipNextHeaderValue !x
    | x < 0xC0  = CountOf 1 -- 0b11000000
    | x < 0xE0  = CountOf 2 -- 0b11100000
    | x < 0xF0  = CountOf 3 -- 0b11110000
    | otherwise = CountOf 4
{-# INLINE skipNextHeaderValue #-}

headerIsAscii :: StepASCII -> Bool
headerIsAscii (StepASCII x) = x < 0x80

charToBytes :: Int -> CountOf Word8
charToBytes c
    | c < 0x80     = CountOf 1
    | c < 0x800    = CountOf 2
    | c < 0x10000  = CountOf 3
    | c < 0x110000 = CountOf 4
    | otherwise    = error ("invalid code point: " `mappend` show c)

-- | Encode a Char into a CharUTF8
encodeCharUTF8 :: Char -> CharUTF8
encodeCharUTF8 !(C# c)
    | bool# (ltWord# x 0x80##   ) = CharUTF8 (W32# x)
    | bool# (ltWord# x 0x800##  ) = CharUTF8 encode2
    | bool# (ltWord# x 0x10000##) = CharUTF8 encode3
    | otherwise                   = CharUTF8 encode4
  where
    !x = int2Word# (ord# c)

    -- clearing mask, clearing all the bits that need to be clear as per the UTF8 encoding
    mask2 = 0x0000bfdf## -- 1 continuation , 5 bits header
    mask3 = 0x00bfbfef## -- 2 continuations, 4 bits header
    mask4 = 0xbfbfbff7## -- 3 continuations, 3 bits header

    -- setting mask, settings all the bits that need to be set per the UTF8 encoding
    set2  = 0x000080c0## -- 10xxxxxx     110xxxxx
    set3  = 0x008080e0## -- 10xxxxxx * 2 1110xxxx
    set4  = 0x808080f0## -- 10xxxxxx * 3 11111xxx

    encode2 = W32# (and# mask2 (or3# set2
                                     (uncheckedShiftRL# x 6#) -- 5 bits to 1st byte
                                     (uncheckedShiftL# x 8# ) -- move lowest bits to the 2nd byte
                               ))
    encode3 = W32# (and# mask3 (or4# set3
                                     (uncheckedShiftRL# x 12#) -- 4 bits to 1st byte
                                     (and# 0x3f00## (uncheckedShiftL# x 2#)) -- 6 bits to the 2nd byte
                                     (uncheckedShiftL# x 16# ) -- move lowest bits to the 3rd byte
                               ))
    encode4 = W32# (and# mask4 (or4# set4
                                     (uncheckedShiftRL# x 18#) -- 3 bits to 1st byte
                                     (or# (and# 0x3f00## (uncheckedShiftRL# x 4#))   -- 6 bits to the 2nd byte
                                          (and# 0x3f0000## (uncheckedShiftL# x 10#)) -- 6 bits to the 3nd byte
                                     )
                                     (uncheckedShiftL# x 24# ) -- move lowest bits to the 4rd byte
                               ))

-- | decode a CharUTF8 into a Char
--
-- If the value inside a CharUTF8 is not properly encoded, this will result in violation
-- of the Char invariants
decodeCharUTF8 :: CharUTF8 -> Char
decodeCharUTF8 c@(CharUTF8 !(W32# w))
    | isCharUTF8Case1 c = toChar# w
    | isCharUTF8Case2 c = encode2
    | isCharUTF8Case3 c = encode3
    | otherwise         = encode4
  where
    encode2 =
        toChar# (or# (uncheckedShiftL# (maskHeader2# w) 6#)
                     (maskContinuation# (uncheckedShiftRL# w 8#))
                )
    encode3 =
        toChar# (or3# (uncheckedShiftL# (maskHeader3# w) 12#)
                      (uncheckedShiftRL# (and# 0x3f00## w) 8#)
                      (maskContinuation# (uncheckedShiftRL# w 16#))
                )
    encode4 =
        toChar# (or4# (uncheckedShiftL#  (maskHeader4# w) 18#)
                      (uncheckedShiftRL# (and# 0x3f00## w) 10#)
                      (uncheckedShiftL#  (and# 0x3f0000## w) 4#)
                      (maskContinuation# (uncheckedShiftRL# w 24#))
                )

    -- clearing mask, removing all UTF8 metadata and keeping only signal (content)
    --maskContent2 = 0x00003f1f## -- 1 continuation , 5 bits header
    --maskContent3 = 0x003f3f0f## -- 2 continuations, 4 bits header
    --maskContent4 = 0x3f3f3f07## -- 3 continuations, 3 bits header

isCharUTF8Case1 :: CharUTF8 -> Bool
isCharUTF8Case1 (CharUTF8 !(W32# w)) = bool# (eqWord# (and# w 0x80##) 0##)
{-# INLINE isCharUTF8Case1 #-}

isCharUTF8Case2 :: CharUTF8 -> Bool
isCharUTF8Case2 (CharUTF8 !(W32# w)) = bool# (eqWord# (and# w 0x20##) 0##)
{-# INLINE isCharUTF8Case2 #-}

isCharUTF8Case3 :: CharUTF8 -> Bool
isCharUTF8Case3 (CharUTF8 !(W32# w)) = bool# (eqWord# (and# w 0x10##) 0##)
{-# INLINE isCharUTF8Case3 #-}

isCharUTF8Case4 :: CharUTF8 -> Bool
isCharUTF8Case4 (CharUTF8 !(W32# w)) = bool# (eqWord# (and# w 0x08##) 0##)
{-# INLINE isCharUTF8Case4 #-}
