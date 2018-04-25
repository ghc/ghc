{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing utilities for comparing
-- for an example on how to use the functions provided here.
--
module Data.ByteString.Builder.Prim.TestUtils (

  -- * Showing
    evalF
  , evalB

  , showF
  , showB

  -- * Testing 'FixedPrim's
  , testF
  , testBoundedF

  , testFixedBoundF

  , compareImpls

  -- * Testing 'BoundedPrim's
  , testBoundedB

  -- * Encoding reference implementations

  , charUtf8_list
  , char8_list

  -- ** ASCII-based encodings
  , encodeASCII
  , encodeForcedASCII
  , char7_list
  , dec_list
  , hex_list
  , wordHexFixed_list
  , int8HexFixed_list
  , int16HexFixed_list
  , int32HexFixed_list
  , int64HexFixed_list
  , floatHexFixed_list
  , doubleHexFixed_list

  -- ** Binary
  , parseVar

  , bigEndian_list
  , littleEndian_list
  , hostEndian_list
  , float_list
  , double_list
  , coerceFloatToWord32
  , coerceDoubleToWord64

  ) where

import           Control.Arrow (first)

import           Data.ByteString.Builder.Prim

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Builder.Prim.Internal as I

import           Data.Char (chr, ord)

import           Numeric (showHex)

#if MIN_VERSION_base(4,4,0)
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
#else
import Foreign
#endif

import           System.ByteOrder

#if defined(HAVE_TEST_FRAMEWORK)
import           Test.HUnit (assertBool)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
#else
import           TestFramework
#endif
import           Test.QuickCheck (Arbitrary(..))

-- Helper functions
-------------------

-- | Quickcheck test that includes a check that the property holds on the
-- bounds of a bounded value.
testBoundedProperty :: forall a. (Arbitrary a, Show a, Bounded a)
                    => String -> (a -> Bool) -> Test
testBoundedProperty name p = testGroup name
  [ testProperty name p
  , testCase (name ++ " minBound") $ assertBool "minBound" (p (minBound :: a))
  , testCase (name ++ " maxBound") $ assertBool "minBound" (p (maxBound :: a))
  ]

-- | Quote a 'String' nicely.
quote :: String -> String
quote cs = '`' : cs ++ "'"

-- | Quote a @[Word8]@ list as as 'String'.
quoteWord8s :: [Word8] -> String
quoteWord8s = quote . map (chr . fromIntegral)


------------------------------------------------------------------------------
-- Testing encodings
------------------------------------------------------------------------------

-- | /For testing use only./ Evaluate a 'FixedPrim' on a given value.
evalF :: FixedPrim a -> a -> [Word8]
evalF fe = S.unpack . S.unsafeCreate (I.size fe) . I.runF fe

-- | /For testing use only./ Evaluate a 'BoundedPrim' on a given value.
evalB :: BoundedPrim a -> a -> [Word8]
evalB be x = S.unpack $ unsafePerformIO $
    S.createAndTrim (I.sizeBound be) $ \op -> do
        op' <- I.runB be x op
        return (op' `minusPtr` op)

-- | /For testing use only./ Show the result of a 'FixedPrim' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showF :: FixedPrim a -> a -> String
showF fe = map (chr . fromIntegral) . evalF fe

-- | /For testing use only./ Show the result of a 'BoundedPrim' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showB :: BoundedPrim a -> a -> String
showB be = map (chr . fromIntegral) . evalB be


-- FixedPrim
----------------

-- TODO: Port code that checks for low-level properties of basic encodings (no
-- overwrites, all bytes written, etc.) from old 'system-io-write' library

-- | Test a 'FixedPrim' against a reference implementation.
testF :: (Arbitrary a, Show a)
      => String
      -> (a -> [Word8])
      -> FixedPrim a
      -> Test
testF name ref fe =
    testProperty name prop
  where
    prop x
      | y == y'   = True
      | otherwise = error $ unlines $
          [ "testF: results disagree for " ++ quote (show x)
          , " fixed encoding: " ++ show y ++ " " ++ quoteWord8s y
          , " reference:      " ++ show y'++ " " ++ quoteWord8s y'
          ]
      where
        y  = evalF fe x
        y' = ref x

-- | Test a 'FixedPrim' of a bounded value against a reference implementation
-- and ensure that the bounds are always included as testcases.
testBoundedF :: (Arbitrary a, Bounded a, Show a)
             => String
             -> (a -> [Word8])
             -> FixedPrim a
             -> Test
testBoundedF name ref fe =
    testBoundedProperty name $ \x -> evalF fe x == ref x

-- FixedPrim derived from a bound on a given value.

testFixedBoundF :: (Arbitrary a, Show a, Integral a)
                => String
                -> (a -> a -> [Word8])
                -> (a -> FixedPrim a)
                -> Test
testFixedBoundF name ref bfe =
    testProperty name prop
  where
    prop (b, x0)
      | y == y'   = True
      | otherwise = error $ unlines $
          [ "testF: results disagree for " ++ quote (show (b, x))
          , " fixed encoding: " ++ show y ++ " " ++ quoteWord8s y
          , " reference:      " ++ show y'++ " " ++ quoteWord8s y'
          ]
      where
        x  | b == 0    = 0
           | otherwise = x0 `mod` b
        y  = evalF (bfe b) x
        y' = ref b x


-- BoundedPrim
------------------

-- | Test a 'BoundedPrim' of a bounded value against a reference implementation
-- and ensure that the bounds are always included as testcases.
testBoundedB :: (Arbitrary a, Bounded a, Show a)
             => String
             -> (a -> [Word8])
             -> BoundedPrim a
             -> Test
testBoundedB name ref fe =
    testBoundedProperty name check
  where
    check x
      | y == y'   = True
      | otherwise = error $ unlines $
          [ "testBoundedB: results disagree for " ++ quote (show x)
          , " fixed encoding: " ++ show y ++ " " ++ quoteWord8s y
          , " reference:      " ++ show y'++ " " ++ quoteWord8s y'
          ]
      where
        y  = evalB fe x
        y' = ref x

-- | Compare two implementations of a function.
compareImpls :: (Arbitrary a, Show a, Show b, Eq b)
             => TestName -> (a -> b) -> (a -> b) -> Test
compareImpls name f1 f2 =
    testProperty name check
  where
    check x
      | y1 == y2  = True
      | otherwise = error $ unlines $
          [ "compareImpls: results disagree for " ++ quote (show x)
          , " f1: " ++ show y1
          , " f2: " ++ show y2
          ]
      where
        y1 = f1 x
        y2 = f2 x



------------------------------------------------------------------------------
-- Encoding reference implementations
------------------------------------------------------------------------------

-- | Char8 encoding: truncate Unicode codepoint to 8-bits.
char8_list :: Char -> [Word8]
char8_list = return . fromIntegral . ord

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
--
-- Copied from 'utf8-string-0.3.6' to make tests self-contained.
-- Copyright (c) 2007, Galois Inc. All rights reserved.
--
charUtf8_list :: Char -> [Word8]
charUtf8_list =
    map fromIntegral . encode . ord
  where
    encode oc
      | oc <= 0x7f       = [oc]

      | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                           , 0x80 + oc .&. 0x3f
                           ]

      | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                           , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                           , 0x80 + oc .&. 0x3f
                           ]
      | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                           , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                           , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                           , 0x80 + oc .&. 0x3f
                           ]

-- ASCII-based encodings
------------------------

-- | Encode a 'String' of only ASCII characters using the ASCII encoding.
encodeASCII :: String -> [Word8]
encodeASCII =
    map encode
  where
    encode c
      | c < '\x7f' = fromIntegral $ ord c
      | otherwise  = error $ "encodeASCII: non-ASCII character '" ++ [c] ++ "'"

-- | Encode an arbitrary 'String' by truncating its characters to the least
-- significant 7-bits.
encodeForcedASCII :: String -> [Word8]
encodeForcedASCII = map ((.&. 0x7f) . fromIntegral . ord)

char7_list :: Char -> [Word8]
char7_list = encodeForcedASCII . return

dec_list :: Show a =>  a -> [Word8]
dec_list = encodeASCII . show

hex_list :: (Integral a, Show a) => a -> [Word8]
hex_list = encodeASCII . (\x -> showHex x "")

wordHexFixed_list :: (Storable a, Integral a, Show a) => a -> [Word8]
wordHexFixed_list x =
   encodeASCII $ pad (2 * sizeOf x) $ showHex x ""
 where
   pad n cs = replicate (n - length cs) '0' ++ cs

int8HexFixed_list :: Int8 -> [Word8]
int8HexFixed_list  = wordHexFixed_list . (fromIntegral :: Int8  -> Word8 )

int16HexFixed_list :: Int16 -> [Word8]
int16HexFixed_list = wordHexFixed_list . (fromIntegral :: Int16 -> Word16)

int32HexFixed_list :: Int32 -> [Word8]
int32HexFixed_list = wordHexFixed_list . (fromIntegral :: Int32 -> Word32)

int64HexFixed_list :: Int64 -> [Word8]
int64HexFixed_list = wordHexFixed_list . (fromIntegral :: Int64 -> Word64)

floatHexFixed_list :: Float -> [Word8]
floatHexFixed_list  = float_list wordHexFixed_list

doubleHexFixed_list :: Double -> [Word8]
doubleHexFixed_list = double_list wordHexFixed_list

-- Binary
---------

bigEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
bigEndian_list = reverse . littleEndian_list

littleEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
littleEndian_list x =
    map (fromIntegral . (x `shiftR`) . (8*)) $ [0..sizeOf x - 1]

hostEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
hostEndian_list = case byteOrder of
    LittleEndian -> littleEndian_list
    BigEndian    -> bigEndian_list
    _            -> error $
        "bounded-encoding: unsupported byteorder '" ++ show byteOrder ++ "'"


float_list :: (Word32 -> [Word8]) -> Float -> [Word8]
float_list f  = f . coerceFloatToWord32

double_list :: (Word64 -> [Word8]) -> Double -> [Word8]
double_list f = f . coerceDoubleToWord64

-- | Convert a 'Float' to a 'Word32'.
{-# NOINLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 x = unsafePerformIO (with x (peek . castPtr))

-- | Convert a 'Double' to a 'Word64'.
{-# NOINLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 x = unsafePerformIO (with x (peek . castPtr))

-- | Parse a variable length encoding
parseVar :: (Num a, Bits a) => [Word8] -> (a, [Word8])
parseVar =
    go
  where
    go []    = error "parseVar: unterminated variable length int"
    go (w:ws)
      | w .&. 0x80 == 0 = (fromIntegral w, ws)
      | otherwise       = first add (go ws)
      where
        add x = (x `shiftL` 7) .|. (fromIntegral w .&. 0x7f)
