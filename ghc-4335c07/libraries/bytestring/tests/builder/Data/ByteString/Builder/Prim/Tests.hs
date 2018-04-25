{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing all encodings provided by this library.

module Data.ByteString.Builder.Prim.Tests (tests) where

import           Data.Char  (ord)
import qualified Data.ByteString.Lazy                  as L
import           Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim          as BP
import           Data.ByteString.Builder.Prim.TestUtils

#if defined(HAVE_TEST_FRAMEWORK)
import           Test.Framework
#else
import           TestFramework
#endif


tests :: [Test]
tests = concat [ testsBinary, testsASCII, testsChar8, testsUtf8
               , testsCombinatorsB ]


------------------------------------------------------------------------------
-- Binary
------------------------------------------------------------------------------

testsBinary :: [Test]
testsBinary =
  [ testBoundedF "word8"     bigEndian_list    BP.word8
  , testBoundedF "int8"      bigEndian_list    BP.int8

  --  big-endian
  , testBoundedF "int16BE"   bigEndian_list    BP.int16BE
  , testBoundedF "int32BE"   bigEndian_list    BP.int32BE
  , testBoundedF "int64BE"   bigEndian_list    BP.int64BE

  , testBoundedF "word16BE"  bigEndian_list    BP.word16BE
  , testBoundedF "word32BE"  bigEndian_list    BP.word32BE
  , testBoundedF "word64BE"  bigEndian_list    BP.word64BE

  , testF "floatLE"     (float_list  littleEndian_list) BP.floatLE
  , testF "doubleLE"    (double_list littleEndian_list) BP.doubleLE

  --  little-endian
  , testBoundedF "int16LE"   littleEndian_list BP.int16LE
  , testBoundedF "int32LE"   littleEndian_list BP.int32LE
  , testBoundedF "int64LE"   littleEndian_list BP.int64LE

  , testBoundedF "word16LE"  littleEndian_list BP.word16LE
  , testBoundedF "word32LE"  littleEndian_list BP.word32LE
  , testBoundedF "word64LE"  littleEndian_list BP.word64LE

  , testF "floatBE"     (float_list  bigEndian_list)   BP.floatBE
  , testF "doubleBE"    (double_list bigEndian_list)   BP.doubleBE

  --  host dependent
  , testBoundedF "int16Host"   hostEndian_list  BP.int16Host
  , testBoundedF "int32Host"   hostEndian_list  BP.int32Host
  , testBoundedF "int64Host"   hostEndian_list  BP.int64Host
  , testBoundedF "intHost"     hostEndian_list  BP.intHost

  , testBoundedF "word16Host"  hostEndian_list  BP.word16Host
  , testBoundedF "word32Host"  hostEndian_list  BP.word32Host
  , testBoundedF "word64Host"  hostEndian_list  BP.word64Host
  , testBoundedF "wordHost"    hostEndian_list  BP.wordHost

  , testF "floatHost"   (float_list  hostEndian_list)   BP.floatHost
  , testF "doubleHost"  (double_list hostEndian_list)   BP.doubleHost
  ]


------------------------------------------------------------------------------
-- Latin-1  aka  Char8
------------------------------------------------------------------------------

testsChar8 :: [Test]
testsChar8 =
  [ testBoundedF "char8"     char8_list        BP.char8  ]


------------------------------------------------------------------------------
-- ASCII
------------------------------------------------------------------------------

testsASCII :: [Test]
testsASCII =
  [ testBoundedF "char7" char7_list BP.char7

  , testBoundedB "int8Dec"   dec_list BP.int8Dec
  , testBoundedB "int16Dec"  dec_list BP.int16Dec
  , testBoundedB "int32Dec"  dec_list BP.int32Dec
  , testBoundedB "int64Dec"  dec_list BP.int64Dec
  , testBoundedB "intDec"    dec_list BP.intDec

  , testBoundedB "word8Dec"  dec_list BP.word8Dec
  , testBoundedB "word16Dec" dec_list BP.word16Dec
  , testBoundedB "word32Dec" dec_list BP.word32Dec
  , testBoundedB "word64Dec" dec_list BP.word64Dec
  , testBoundedB "wordDec"   dec_list BP.wordDec

  , testBoundedB "word8Hex"  hex_list BP.word8Hex
  , testBoundedB "word16Hex" hex_list BP.word16Hex
  , testBoundedB "word32Hex" hex_list BP.word32Hex
  , testBoundedB "word64Hex" hex_list BP.word64Hex
  , testBoundedB "wordHex"   hex_list BP.wordHex

  , testBoundedF "word8HexFixed"  wordHexFixed_list BP.word8HexFixed
  , testBoundedF "word16HexFixed" wordHexFixed_list BP.word16HexFixed
  , testBoundedF "word32HexFixed" wordHexFixed_list BP.word32HexFixed
  , testBoundedF "word64HexFixed" wordHexFixed_list BP.word64HexFixed

  , testBoundedF "int8HexFixed"  int8HexFixed_list  BP.int8HexFixed
  , testBoundedF "int16HexFixed" int16HexFixed_list BP.int16HexFixed
  , testBoundedF "int32HexFixed" int32HexFixed_list BP.int32HexFixed
  , testBoundedF "int64HexFixed" int64HexFixed_list BP.int64HexFixed

  , testF "floatHexFixed"  floatHexFixed_list  BP.floatHexFixed
  , testF "doubleHexFixed" doubleHexFixed_list BP.doubleHexFixed
  ]


------------------------------------------------------------------------------
-- UTF-8
------------------------------------------------------------------------------

testsUtf8 :: [Test]
testsUtf8 =
  [ testBoundedB "charUtf8"  charUtf8_list  BP.charUtf8 ]


------------------------------------------------------------------------------
-- BoundedPrim combinators
------------------------------------------------------------------------------

maybeB :: BP.BoundedPrim () -> BP.BoundedPrim a -> BP.BoundedPrim (Maybe a)
maybeB nothing just = maybe (Left ()) Right BP.>$< BP.eitherB nothing just

testsCombinatorsB :: [Test]
testsCombinatorsB =
  [ compareImpls "mapMaybe (via BoundedPrim)"
        (L.pack . concatMap encChar)
        (toLazyByteString . encViaBuilder)

  , compareImpls "filter (via BoundedPrim)"
        (L.pack . filter (< 32))
        (toLazyByteString . BP.primMapListBounded (BP.condB (< 32) (BP.liftFixedToBounded BP.word8) BP.emptyB))

  , compareImpls "pairB"
        (L.pack . concatMap (\(c,w) -> charUtf8_list c ++ [w]))
        (toLazyByteString . BP.primMapListBounded
            ((\(c,w) -> (c,(w,undefined))) BP.>$<
                BP.charUtf8 BP.>*< (BP.liftFixedToBounded BP.word8) BP.>*< (BP.liftFixedToBounded BP.emptyF)))
  ]
  where
    encChar = maybe [112] (hostEndian_list . ord)

    encViaBuilder = BP.primMapListBounded $ maybeB (BP.liftFixedToBounded $ (\_ -> 112) BP.>$< BP.word8)
                                                (ord BP.>$< (BP.liftFixedToBounded $ BP.intHost))
