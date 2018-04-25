{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface,
             MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-- | Copyright : (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- Constructing 'Builder's using ASCII-based encodings.
--
module Data.ByteString.Builder.ASCII
    (
      -- ** Formatting numbers as text
      -- | Formatting of numbers as ASCII text.
      --
      -- Note that you can also use these functions for the ISO/IEC 8859-1 and
      -- UTF-8 encodings, as the ASCII encoding is equivalent on the 
      -- codepoints 0-127.

      -- *** Decimal numbers
      -- | Decimal encoding of numbers using ASCII encoded characters.
      int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec
    , integerDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    , floatDec
    , doubleDec

      -- *** Hexadecimal numbers

      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- ASCII characters. The shortest
      -- possible representation is used. For example,
      --
      -- >>> toLazyByteString (word16Hex 0x0a10)
      -- Chunk "a10" Empty
      --
      -- Note that there is no support for using upper-case characters. Please
      -- contact the maintainer, if your application cannot work without
      -- hexadecimal encodings that use upper-case characters.
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- *** Fixed-width hexadecimal numbers
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

    , byteStringHex
    , lazyByteStringHex

    ) where

import           Data.ByteString                                as S
import           Data.ByteString.Lazy                           as L
import           Data.ByteString.Builder.Internal (Builder)
import qualified Data.ByteString.Builder.Prim                   as P

import           Foreign


#if defined(INTEGER_GMP)

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mappend)
# endif
import           Foreign.C.Types

import qualified Data.ByteString.Builder.Prim.Internal          as P
import           Data.ByteString.Builder.Prim.Internal.UncheckedShifts
                   ( caseWordSize_32_64 )

# if __GLASGOW_HASKELL__ < 710
import           GHC.Num     (quotRemInteger)
# endif
import           GHC.Types   (Int(..))


# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif
#endif

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------


-- | Encode a 'String' using 'P.char7'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = P.primMapListFixed P.char7

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

-- | Decimal encoding of an 'Int8' using the ASCII digits.
--
-- e.g.
--
-- > toLazyByteString (int8Dec 42)   = "42"
-- > toLazyByteString (int8Dec (-1)) = "-1"
--
{-# INLINE int8Dec #-}
int8Dec :: Int8 -> Builder
int8Dec = P.primBounded P.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE int16Dec #-}
int16Dec :: Int16 -> Builder
int16Dec = P.primBounded P.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE int32Dec #-}
int32Dec :: Int32 -> Builder
int32Dec = P.primBounded P.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Builder
int64Dec = P.primBounded P.int64Dec

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec = P.primBounded P.intDec


-- Unsigned integers
--------------------

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE word8Dec #-}
word8Dec :: Word8 -> Builder
word8Dec = P.primBounded P.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE word16Dec #-}
word16Dec :: Word16 -> Builder
word16Dec = P.primBounded P.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE word32Dec #-}
word32Dec :: Word32 -> Builder
word32Dec = P.primBounded P.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Builder
word64Dec = P.primBounded P.word64Dec

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE wordDec #-}
wordDec :: Word -> Builder
wordDec = P.primBounded P.wordDec


-- Floating point numbers
-------------------------

-- TODO: Use Bryan O'Sullivan's double-conversion package to speed it up.

-- | /Currently slow./ Decimal encoding of an IEEE 'Float'.
{-# INLINE floatDec #-}
floatDec :: Float -> Builder
floatDec = string7 . show

-- | /Currently slow./ Decimal encoding of an IEEE 'Double'.
{-# INLINE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = string7 . show


------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

-- | Shortest hexadecimal encoding of a 'Word8' using lower-case characters.
{-# INLINE word8Hex #-}
word8Hex :: Word8 -> Builder
word8Hex = P.primBounded P.word8Hex

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE word16Hex #-}
word16Hex :: Word16 -> Builder
word16Hex = P.primBounded P.word16Hex

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE word32Hex #-}
word32Hex :: Word32 -> Builder
word32Hex = P.primBounded P.word32Hex

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
{-# INLINE word64Hex #-}
word64Hex :: Word64 -> Builder
word64Hex = P.primBounded P.word64Hex

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE wordHex #-}
wordHex :: Word -> Builder
wordHex = P.primBounded P.wordHex


-- fixed width; leading zeroes
------------------------------

-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: Int8 -> Builder
int8HexFixed = P.primFixed P.int8HexFixed

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Int16 -> Builder
int16HexFixed = P.primFixed P.int16HexFixed

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Int32 -> Builder
int32HexFixed = P.primFixed P.int32HexFixed

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Int64 -> Builder
int64HexFixed = P.primFixed P.int64HexFixed

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: Word8 -> Builder
word8HexFixed = P.primFixed P.word8HexFixed

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Word16 -> Builder
word16HexFixed = P.primFixed P.word16HexFixed

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Word32 -> Builder
word32HexFixed = P.primFixed P.word32HexFixed

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Word64 -> Builder
word64HexFixed = P.primFixed P.word64HexFixed

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Float -> Builder
floatHexFixed = P.primFixed P.floatHexFixed

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Double -> Builder
doubleHexFixed = P.primFixed P.doubleHexFixed

-- | Encode each byte of a 'S.ByteString' using its fixed-width hex encoding.
{-# NOINLINE byteStringHex #-} -- share code
byteStringHex :: S.ByteString -> Builder
byteStringHex = P.primMapByteStringFixed P.word8HexFixed

-- | Encode each byte of a lazy 'L.ByteString' using its fixed-width hex encoding.
{-# NOINLINE lazyByteStringHex #-} -- share code
lazyByteStringHex :: L.ByteString -> Builder
lazyByteStringHex = P.primMapLazyByteStringFixed P.word8HexFixed


------------------------------------------------------------------------------
-- Fast decimal 'Integer' encoding.
------------------------------------------------------------------------------

#if defined(INTEGER_GMP)
-- An optimized version of the integer serialization code
-- in blaze-textual (c) 2011 MailRank, Inc. Bryan O'Sullivan
-- <bos@mailrank.com>. It is 2.5x faster on Int-sized integers and 4.5x faster
-- on larger integers.

# define PAIR(a,b) (# a,b #)

-- | Maximal power of 10 fitting into an 'Int' without using the MSB.
--     10 ^ 9  for 32 bit ints  (31 * log 2 / log 10 =  9.33)
--     10 ^ 18 for 64 bit ints  (63 * log 2 / log 10 = 18.96)
--
-- FIXME: Think about also using the MSB. For 64 bit 'Int's this makes a
-- difference.
maxPow10 :: Integer
maxPow10 = toInteger $ (10 :: Int) ^ caseWordSize_32_64 (9 :: Int) 18

-- | Decimal encoding of an 'Integer' using the ASCII digits.
integerDec :: Integer -> Builder
integerDec (S# i#) = intDec (I# i#)
integerDec i
    | i < 0     = P.primFixed P.char8 '-' `mappend` go (-i)
    | otherwise =                                   go ( i)
  where
    errImpossible fun =
        error $ "integerDec: " ++ fun ++ ": the impossible happened."

    go :: Integer -> Builder
    go n | n < maxPow10 = intDec (fromInteger n)
         | otherwise    =
             case putH (splitf (maxPow10 * maxPow10) n) of
               (x:xs) -> intDec x `mappend` P.primMapListBounded intDecPadded xs
               []     -> errImpossible "integerDec: go"

    splitf :: Integer -> Integer -> [Integer]
    splitf pow10 n0
      | pow10 > n0  = [n0]
      | otherwise   = splith (splitf (pow10 * pow10) n0)
      where
        splith []     = errImpossible "splith"
        splith (n:ns) =
            case n `quotRemInteger` pow10 of
                PAIR(q,r) | q > 0     -> q : r : splitb ns
                          | otherwise ->     r : splitb ns

        splitb []     = []
        splitb (n:ns) = case n `quotRemInteger` pow10 of
                            PAIR(q,r) -> q : r : splitb ns

    putH :: [Integer] -> [Int]
    putH []     = errImpossible "putH"
    putH (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(x,y)
                        | q > 0     -> q : r : putB ns
                        | otherwise ->     r : putB ns
                        where q = fromInteger x
                              r = fromInteger y

    putB :: [Integer] -> [Int]
    putB []     = []
    putB (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(q,r) -> fromInteger q : fromInteger r : putB ns


foreign import ccall unsafe "static _hs_bytestring_int_dec_padded9"
    c_int_dec_padded9 :: CInt -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static _hs_bytestring_long_long_int_dec_padded18"
    c_long_long_int_dec_padded18 :: CLLong -> Ptr Word8 -> IO ()

{-# INLINE intDecPadded #-}
intDecPadded :: P.BoundedPrim Int
intDecPadded = P.liftFixedToBounded $ caseWordSize_32_64
    (P.fixedPrim  9 $ c_int_dec_padded9            . fromIntegral)
    (P.fixedPrim 18 $ c_long_long_int_dec_padded18 . fromIntegral)

#else
-- compilers other than GHC

-- | Decimal encoding of an 'Integer' using the ASCII digits. Implemented
-- using via the 'Show' instance of 'Integer's.
integerDec :: Integer -> Builder
integerDec = string7 . show
#endif
