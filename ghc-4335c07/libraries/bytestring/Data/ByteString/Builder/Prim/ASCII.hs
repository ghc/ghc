{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-- | Copyright   : (c) 2010 Jasper Van der Jeugt
--                 (c) 2010 - 2011 Simon Meier
-- License       : BSD3-style (see LICENSE)
--
-- Maintainer    : Simon Meier <iridcode@gmail.com>
-- Portability   : GHC
--
-- Encodings using ASCII encoded Unicode characters.
--
module Data.ByteString.Builder.Prim.ASCII
    (

     -- *** ASCII
     char7

      -- **** Decimal numbers
      -- | Decimal encoding of numbers using ASCII encoded characters.
    , int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    {-
    -- These are the functions currently provided by Bryan O'Sullivans
    -- double-conversion library.
    --
    -- , float
    -- , floatWith
    -- , double
    -- , doubleWith
    -}

      -- **** Hexadecimal numbers

      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- ASCII characters. The shortest possible representation is used. For
      -- example,
      --
      -- > toLazyByteString (primBounded word16Hex 0x0a10) = "a10"
      --
      -- Note that there is no support for using upper-case characters. Please
      -- contact the maintainer if your application cannot work without
      -- hexadecimal encodings that use upper-case characters.
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- **** Fixed-width hexadecimal numbers
      --
      -- | Encoding the bytes of fixed-width types as hexadecimal
      -- numbers using lower-case ASCII characters. For example,
      --
      -- > toLazyByteString (primFixed word16HexFixed 0x0a10) = "0a10"
      --
    , int8HexFixed
    , int16HexFixed
    , int32HexFixed
    , int64HexFixed
    , word8HexFixed
    , word16HexFixed
    , word32HexFixed
    , word64HexFixed
    , floatHexFixed
    , doubleHexFixed

    ) where

import Data.ByteString.Builder.Prim.Binary
import Data.ByteString.Builder.Prim.Internal
import Data.ByteString.Builder.Prim.Internal.Floating
import Data.ByteString.Builder.Prim.Internal.Base16
import Data.ByteString.Builder.Prim.Internal.UncheckedShifts

import Data.Char (ord)

import Foreign
import Foreign.C.Types

-- | Encode the least 7-bits of a 'Char' using the ASCII encoding.
{-# INLINE char7 #-}
char7 :: FixedPrim Char
char7 = (\c -> fromIntegral $ ord c .&. 0x7f) >$< word8


------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

foreign import ccall unsafe "static _hs_bytestring_int_dec" c_int_dec
    :: CInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_long_long_int_dec" c_long_long_int_dec
    :: CLLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeIntDecimal #-}
encodeIntDecimal :: Integral a => Int -> BoundedPrim a
encodeIntDecimal bound = boudedPrim bound $ c_int_dec . fromIntegral

-- | Decimal encoding of an 'Int8'.
{-# INLINE int8Dec #-}
int8Dec :: BoundedPrim Int8
int8Dec = encodeIntDecimal 4

-- | Decimal encoding of an 'Int16'.
{-# INLINE int16Dec #-}
int16Dec :: BoundedPrim Int16
int16Dec = encodeIntDecimal 6


-- | Decimal encoding of an 'Int32'.
{-# INLINE int32Dec #-}
int32Dec :: BoundedPrim Int32
int32Dec = encodeIntDecimal 11

-- | Decimal encoding of an 'Int64'.
{-# INLINE int64Dec #-}
int64Dec :: BoundedPrim Int64
int64Dec = boudedPrim 20 $ c_long_long_int_dec . fromIntegral

-- | Decimal encoding of an 'Int'.
{-# INLINE intDec #-}
intDec :: BoundedPrim Int
intDec = caseWordSize_32_64
    (fromIntegral >$< int32Dec)
    (fromIntegral >$< int64Dec)


-- Unsigned integers
--------------------

foreign import ccall unsafe "static _hs_bytestring_uint_dec" c_uint_dec
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_long_long_uint_dec" c_long_long_uint_dec
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordDecimal #-}
encodeWordDecimal :: Integral a => Int -> BoundedPrim a
encodeWordDecimal bound = boudedPrim bound $ c_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word8'.
{-# INLINE word8Dec #-}
word8Dec :: BoundedPrim Word8
word8Dec = encodeWordDecimal 3

-- | Decimal encoding of a 'Word16'.
{-# INLINE word16Dec #-}
word16Dec :: BoundedPrim Word16
word16Dec = encodeWordDecimal 5

-- | Decimal encoding of a 'Word32'.
{-# INLINE word32Dec #-}
word32Dec :: BoundedPrim Word32
word32Dec = encodeWordDecimal 10

-- | Decimal encoding of a 'Word64'.
{-# INLINE word64Dec #-}
word64Dec :: BoundedPrim Word64
word64Dec = boudedPrim 20 $ c_long_long_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word'.
{-# INLINE wordDec #-}
wordDec :: BoundedPrim Word
wordDec = caseWordSize_32_64
    (fromIntegral >$< word32Dec)
    (fromIntegral >$< word64Dec)

------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

foreign import ccall unsafe "static _hs_bytestring_uint_hex" c_uint_hex
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_long_long_uint_hex" c_long_long_uint_hex
    :: CULLong -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordHex #-}
encodeWordHex :: forall a. (Storable a, Integral a) => BoundedPrim a
encodeWordHex =
    boudedPrim (2 * sizeOf (undefined :: a)) $ c_uint_hex  . fromIntegral

-- | Hexadecimal encoding of a 'Word8'.
{-# INLINE word8Hex #-}
word8Hex :: BoundedPrim Word8
word8Hex = encodeWordHex

-- | Hexadecimal encoding of a 'Word16'.
{-# INLINE word16Hex #-}
word16Hex :: BoundedPrim Word16
word16Hex = encodeWordHex

-- | Hexadecimal encoding of a 'Word32'.
{-# INLINE word32Hex #-}
word32Hex :: BoundedPrim Word32
word32Hex = encodeWordHex

-- | Hexadecimal encoding of a 'Word64'.
{-# INLINE word64Hex #-}
word64Hex :: BoundedPrim Word64
word64Hex = boudedPrim 16 $ c_long_long_uint_hex . fromIntegral

-- | Hexadecimal encoding of a 'Word'.
{-# INLINE wordHex #-}
wordHex :: BoundedPrim Word
wordHex = caseWordSize_32_64
    (fromIntegral >$< word32Hex)
    (fromIntegral >$< word64Hex)


-- fixed width; leading zeroes
------------------------------

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: FixedPrim Word8
word8HexFixed = fixedPrim 2 $
    \x op -> poke (castPtr op) =<< encode8_as_16h lowerTable x

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: FixedPrim Word16
word16HexFixed =
    (\x -> (fromIntegral $ x `shiftr_w16` 8, fromIntegral x))
      >$< pairF word8HexFixed word8HexFixed

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: FixedPrim Word32
word32HexFixed =
    (\x -> (fromIntegral $ x `shiftr_w32` 16, fromIntegral x))
      >$< pairF word16HexFixed word16HexFixed
-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: FixedPrim Word64
word64HexFixed =
    (\x -> (fromIntegral $ x `shiftr_w64` 32, fromIntegral x))
      >$< pairF word32HexFixed word32HexFixed

-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: FixedPrim Int8
int8HexFixed = fromIntegral >$< word8HexFixed

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: FixedPrim Int16
int16HexFixed = fromIntegral >$< word16HexFixed

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: FixedPrim Int32
int32HexFixed = fromIntegral >$< word32HexFixed

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: FixedPrim Int64
int64HexFixed = fromIntegral >$< word64HexFixed

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: FixedPrim Float
floatHexFixed = encodeFloatViaWord32F word32HexFixed

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: FixedPrim Double
doubleHexFixed = encodeDoubleViaWord64F word64HexFixed


