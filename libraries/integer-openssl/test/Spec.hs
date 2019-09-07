{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE PackageImports      #-}

module Main where

import           Data.Bits             ((.&.))
import           Data.Char             (intToDigit)
import           GHC.Prim
import           GHC.Types
import           Numeric               (showHex)
import           Test.Hspec            (Expectation, describe, hspec, it,
                                        shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       hiding ((.&.), NonZero)

import qualified GHC.Integer              as Y
import qualified OpenSSL.GHC.Integer.Type as X

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
# define INT_MINBOUND      -0x8000000000000000
# define INT_MAXBOUND       0x7fffffffffffffff
# define ABS_INT_MINBOUND   0x8000000000000000
# define WORD_SIZE_IN_BYTES 8
# define WORD_SHIFT         3
# define WORD_MINBOUND      0x0000000000000000
# define WORD_MAXBOUND      0xffffffffffffffff
#elif WORD_SIZE_IN_BITS == 32
# define INT_MINBOUND       -0x80000000
# define INT_MAXBOUND       0x7fffffff
# define ABS_INT_MINBOUND   0x80000000
# define WORD_SIZE_IN_BYTES 4
# define WORD_SHIFT         2
# define WORD_MINBOUND       0x00000000
# define WORD_MAXBOUND       0xffffffff
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

main :: IO ()
main = hspec $ do
  describe "BigNum" $ do
    prop "bitNumToWord (wordToBigNum x) === x" $ \w@(W# w#) ->
      W# (X.bigNumToWord (X.wordToBigNum w#)) === w

    prop "notBigNum (notBigNum x) === x" $ \bn1 ->
      show (X.notBigNum (X.notBigNum bn1)) === show bn1

    describe "notBigNum" $ do
      it "works for single word bignums" $
        show (X.notBigNum (X.wordToBigNum 0xffffffffff000000##))
        `shouldBe` show (X.wordToBigNum   0x0000000000ffffff##)
      it "works for double word bignums" $
        show (X.notBigNum (X.wordToBigNum2 0xfffffffff0000000## 0xffffffff00000000##))
        `shouldBe` show (X.wordToBigNum2   0x000000000fffffff## 0x00000000ffffffff##)

    describe "orBigNum" $ do
      it "works for single word bignums" $
        show ((X.wordToBigNum 0x00000000ff000000##) `X.orBigNum`
              (X.wordToBigNum 0x00000000000000ff##)) `shouldBe`
        show  (X.wordToBigNum 0x00000000ff0000ff##)
      it "works for double word bignums" $
        show ((X.wordToBigNum2 0x0000000000000002## 0x0000000000000001##) `X.orBigNum`
              (X.wordToBigNum2 0x0000000000000001## 0x0000000000000002##)) `shouldBe`
        show  (X.wordToBigNum2 0x0000000000000003## 0x0000000000000003##)

    describe "andBigNum" $ do
      it "works for single word bignums" $
        show ((X.wordToBigNum 0x80000000ff0000f0##) `X.andBigNum`
              (X.wordToBigNum 0x80000000f00000ff##)) `shouldBe`
        show  (X.wordToBigNum 0x80000000f00000f0##)
      it "works for double word bignums" $
        show ((X.wordToBigNum2 0x000f000000000002## 0x0040000000000002##) `X.andBigNum`
              (X.wordToBigNum2 0x000f000000000003## 0x0040000000000003##)) `shouldBe`
        show  (X.wordToBigNum2 0x000f000000000002## 0x0040000000000002##)
      it "works for different size bignums" $
        show ((X.wordToBigNum2 0xf000000f## 0xf000000f##) `X.andBigNum`
              (X.wordToBigNum               0xf0000001##)) `shouldBe`
        show  (X.wordToBigNum               0xf0000001##)
      it "works for different size bignums (flipped)" $
        show ((X.wordToBigNum               0xf000000f##) `X.andBigNum`
              (X.wordToBigNum2 0xf000000f## 0xf0000003##)) `shouldBe`
        show  (X.wordToBigNum               0xf0000003##)

    describe "andnBigNum" $ do
      it "works for single word bignums" $
        show ((X.wordToBigNum 0x000000ff##) `X.andnBigNum`
              (X.wordToBigNum 0xffffff00##)) `shouldBe`
        show  (X.wordToBigNum 0x000000ff##)
      it "works for double word bignums" $
        show ((X.wordToBigNum2 0xf000000f## 0x00000001##) `X.andnBigNum`
              (X.wordToBigNum2 0x0ffffff0## 0x00000000##)) `shouldBe`
        show  (X.wordToBigNum2 0xf000000f## 0x00000001##)
      it "works for different size bignums" $
        show ((X.wordToBigNum2 0xf000000f## 0xf000000f##) `X.andnBigNum`
              (X.wordToBigNum               0xf0000000##)) `shouldBe`
        show  (X.wordToBigNum2 0xf000000f## 0x0000000f##)
      it "works for different size bignums (flipped)" $
        show ((X.wordToBigNum               0xf000000f##) `X.andnBigNum`
              (X.wordToBigNum2 0xf000000f## 0x0fffffff##)) `shouldBe`
        show  (X.wordToBigNum               0xf0000000##)

    prop "wordToBigNum (w1 `or` w2) === (wordToBigNum w1) `orBigNum` (wordToBigNum w2)" $ \(W# w1, W# w2) ->
      show (X.wordToBigNum (w1 `or#` w2)) === show ((X.wordToBigNum w1) `X.orBigNum` (X.wordToBigNum w2))

    prop "wordToBigNum (w1 `and` w2) === (wordToBigNum w1) `andBigNum` (wordToBigNum w2)" $ \(W# w1, W# w2) ->
      show (X.wordToBigNum (w1 `and#` w2)) === show ((X.wordToBigNum w1) `X.andBigNum` (X.wordToBigNum w2))

    prop "wordToBigNum (w1 `xor` w2) === (wordToBigNum w1) `xorBigNum` (wordToBigNum w2)" $ \(W# w1, W# w2) ->
      show (X.wordToBigNum (w1 `xor#` w2)) === show ((X.wordToBigNum w1) `X.xorBigNum` (X.wordToBigNum w2))

  describe "Integer" $ do
    describe "smallIntger" $ do
      prop "identical to builtin" $ \(SmallInt (I# i)) ->
        X.smallInteger i <<>> Y.smallInteger i

    describe "mkInteger" $ do
      it "can create some Integers" $ do
        shouldEqualHex (X.mkInteger True [0xbb, 0xaa])
                       (Y.mkInteger True [0xbb, 0xaa])
        shouldEqualHex (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
                       (Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
      prop "identical to builtin" $ \(b, is) ->
        let ints = map truncate32pos is
        in  X.mkInteger b ints <<>> Y.mkInteger b ints

    describe "wordToInteger" $ do
      prop "identical to builtin" $ \(Positive (W# c#)) ->
        X.wordToInteger c# <<>> Y.wordToInteger c#

    describe "integerToWord" $ do
      prop "identical to builtin" $ \(Integers x1 y1) ->
        isTrue# (eqWord# (X.integerToWord x1) (Y.integerToWord y1))

    describe "integerToInt" $ do
      prop "identical to builtin" $ \(Integers x1 y1) ->
        isTrue# (X.integerToInt x1 ==# Y.integerToInt y1)

    describe "doubleFromInteger" $ do
      prop "identical to builtin" $ \(Integers x1 y1) ->
        isTrue# (X.doubleFromInteger x1 ==## Y.doubleFromInteger y1)

    describe "encodeDoubleInteger" $ do
      -- TODO(SN): broken in lts14, but okay in lts12 ?? --seed 489753666"
      prop "identical to builtin" $ \(Integers x1 y1, SmallInt (I# i)) ->
        isTrue# (X.encodeDoubleInteger x1 i ==## Y.encodeDoubleInteger y1 i)

    describe "decodeDoubleInteger" $ do
      prop "identical to builtin" $ \(D# x1)  ->
        let (# m1, e1 #) = X.decodeDoubleInteger x1
            (# m2, e2 #) = Y.decodeDoubleInteger x1
        in  m1 <<>> m2 .&&. isTrue# (e1 ==# e2)

    describe "encodeFloatInteger" $ do
      prop "identical to builtin" $ \(Integers x1 y1, SmallInt (I# i)) ->
        isTrue# (X.encodeFloatInteger x1 i `eqFloat#` Y.encodeFloatInteger y1 i)

    describe "hashInteger" $ do
      prop "identical to builtin" $ \(Integers x1 y1) ->
        isTrue# (X.hashInteger x1 ==# Y.hashInteger y1)

    describe "plusInteger" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.plusInteger x1 x2 <<>> Y.plusInteger y1 y2

    describe "minusInteger" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.minusInteger x1 x2 <<>> Y.minusInteger y1 y2

    describe "negateInteger" $ do
      it "considers min bound Int" $
        shouldEqualHex (X.negateInteger $ X.smallInteger INT_MINBOUND#)
                       (Y.negateInteger $ Y.smallInteger INT_MINBOUND#)

    describe "absInteger" $ do
      it "works for 0" $ do
        shouldEqualHex (X.absInteger (X.smallInteger 0#)) (Y.smallInteger 0#)
      it "works for INT_MINBOUND" $ do
        shouldEqualHex (X.absInteger (X.smallInteger INT_MINBOUND#)) (Y.absInteger (Y.smallInteger INT_MINBOUND#))
      prop "identical to builtin (small integers)" $ \(SmallInt (I# i)) ->
        X.absInteger (X.smallInteger i) <<>> Y.absInteger (Y.smallInteger i)
      prop "identical to builtin (any integers)" $ \(Integers x y) ->
        X.absInteger x <<>> Y.absInteger y

    describe "signumInteger" $ do
      prop "identical to builtin" $ \(Integers x1 y1) ->
        X.signumInteger x1 <<>> Y.signumInteger y1

    describe "timesInteger" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.timesInteger x1 x2 <<>> Y.timesInteger y1 y2

    describe "quotRemInteger" $ do
      -- division by zero cannot be tested properly here
      prop "identical to builtin" $ \((Integers x1 y1), NonZero (Integers x2 y2)) ->
        let (# xq, xr #) = X.quotRemInteger x1 x2
            (# yq, yr #) = Y.quotRemInteger y1 y2
        in xq <<>> yq .&&. xr <<>> yr

    describe "divModInteger" $ do
      -- division by zero cannot be tested properly here
      prop "can divide random Integers" $ \((Integers x1 y1), NonZero (Integers x2 y2)) ->
        let (# xd, xm #) = X.divModInteger x1 x2
            (# yd, ym #) = Y.divModInteger y1 y2
        in xd <<>> yd .&&. xm <<>> ym

    describe "orInteger" $ do
      prop "identical to builtins" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.orInteger x1 x2 <<>> Y.orInteger y1 y2

    describe "andInteger" $ do
      prop "identical to builtins" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.andInteger x1 x2 <<>> Y.andInteger y1 y2

    describe "xorInteger" $ do
      prop "identical to builtins" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.xorInteger x1 x2 <<>> Y.xorInteger y1 y2

    describe "complementInteger" $ do
      prop "identical to builtins" $ \(Integers x1 y1) ->
        X.complementInteger x1 <<>> Y.complementInteger y1
      prop "not . not === id" $ \x1 ->
        X.complementInteger (X.complementInteger x1) === x1

    describe "shiftLInteger" $ do
      prop "identical to builtins and positive shifts" $ \((Integers x1 y1), Positive (I# i#)) ->
        X.shiftLInteger x1 i# <<>> Y.shiftLInteger y1 i#

    describe "shiftRInteger" $ do
      prop "identical to builtins and positive shifts" $ \((Integers x1 y1), Positive (I# i#)) ->
        X.shiftRInteger x1 i# <<>> Y.shiftRInteger y1 i#

    describe "testBitInteger" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (I# i#)) ->
        X.testBitInteger x1 i# === Y.testBitInteger y1 i#

    describe "bitInteger" $ do
      it "works for single word integers" $ do
        X.bitInteger 0# `shouldBe` (X.smallInteger 0x0000000000000001#)
        X.bitInteger 1# `shouldBe` (X.smallInteger 0x0000000000000002#)
        X.bitInteger 4# `shouldBe` (X.smallInteger 0x0000000000000010#)
      -- Test against "builtin" bitInteger not possible as it is not part of the GHC.Integer interface
      prop "testBitInteger (bitInteger i) i" $ \(Positive (I# i#)) ->
        X.testBitInteger (X.bitInteger i#) i#

    describe "popCountInteger" $ do
      -- Test against "builtin" popCountInteger not possible as it is not part of the GHC.Integer interface
      prop "popCountInteger (bitInteger i) === 1" $ \(Positive (I# i#)) ->
        isTrue# (X.popCountInteger (X.bitInteger i#) ==# 1#)

      prop "popCountInteger (negateInteger (bitInteger i)) === -1" $ \(Positive (I# i#)) ->
        isTrue# (X.popCountInteger (X.negateInteger (X.bitInteger i#)) ==# -1#)

    describe "compareInteger" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        X.compareInteger x1 x2 === Y.compareInteger y1 y2

    describe "eqInteger" $ do
      prop "x == x" $ \x1 -> isTrue# (X.eqInteger# x1 x1)
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        isTrue# ((X.eqInteger# x1 x2) ==# (Y.eqInteger# y1 y2))

    describe "neqInteger" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        isTrue# (X.neqInteger# x1 x1 ==# 0#) && isTrue# ((X.neqInteger# x1 x2) ==# (Y.neqInteger# y1 y2))

    describe "geInteger#" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        isTrue# ((X.geInteger# x1 x2) ==# (Y.geInteger# y1 y2))

    describe "gtInteger#" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        isTrue# ((X.gtInteger# x1 x2) ==# (Y.gtInteger# y1 y2))

    describe "leInteger#" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        isTrue# ((X.leInteger# x1 x2) ==# (Y.leInteger# y1 y2))

    describe "ltInteger#" $ do
      prop "identical to builtin" $ \((Integers x1 y1), (Integers x2 y2)) ->
        isTrue# ((X.ltInteger# x1 x2) ==# (Y.ltInteger# y1 y2))

showHexY :: Y.Integer -> String
showHexY i
  | i >= 0 = showHex i ""
  | otherwise = "-" ++ (showHex (Y.negateInteger i) "")

showHexX :: X.Integer -> String
showHexX i@(X.S# i#)
  | isTrue# (i# >=# 0#) = showHex (I# i#) ""
  | otherwise = "-" ++ (showHexX (X.negateInteger i))
showHexX (X.Bn# bn) = "-" ++ (show bn)
showHexX (X.Bp# bn) = show bn

-- TODO(SN): decimal show instance
instance Show X.Integer where
  show = showHexX

instance Show X.BigNum where
  -- Return base16 encoded string of underlying byte array, MSB first
  show (X.BN# ba#) =
    case dropWhile (== '0') $ go (sizeofByteArray# ba# -# 1#) of
      "" -> "0"
      x -> x
   where
    go 0# = word8hex (indexWord8Array# ba# 0#)
    go i# = word8hex (indexWord8Array# ba# i#) ++ go (i# -# 1#)

    word8hex :: Word# -> String
    word8hex w
      | isTrue# (w `ltWord#` 0xA##) = ['0', wordToDigit w]
      | True = [wordToDigit ((w `uncheckedShiftRL#` 4#) `and#` 0xF##), wordToDigit (w `and#` 0xF##)]

    wordToDigit w = intToDigit $ I# (word2Int# w)

-- | HSpec expectation using hex representation
shouldEqualHex :: X.Integer -> Y.Integer -> Expectation
shouldEqualHex x y = showHexX x `shouldBe` showHexY y

(<<>>) :: X.Integer -> Y.Integer -> Property
x <<>> y =
  counterexample (showHexX x ++ " /= " ++ showHexY y) (showHexX x == showHexY y)

-- | Newtype to generate minbound/maxbound values more often in QuickCheck
-- Arbitrary instance.
newtype SmallInt = SmallInt Int deriving Show

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> frequency [ (4, arbitrary)
                                     , (1, pure 0)
                                     , (1, pure (I# INT_MINBOUND#))
                                     , (1, pure (I# INT_MAXBOUND#))
                                     ]

-- | Datatype to test various Integers via QuickCheck.
data Integers = Integers X.Integer Y.Integer deriving Show

instance Arbitrary Integers where
  arbitrary = oneof [small, big]
   where
    small = do
      (SmallInt (I# i)) <- arbitrary
      pure $ Integers (X.smallInteger i) (Y.smallInteger i)

    big = do
      positive <- arbitrary
      ints <- map truncate32pos <$> arbitrary -- 31bit int chunks
      pure $ Integers (X.mkInteger positive ints) (Y.mkInteger positive ints)

instance Arbitrary X.Integer where
  arbitrary = oneof [small, big]
   where
    small = do
      (I# i) <- arbitrary
      pure $ X.smallInteger i

    big = do
      positive <- arbitrary
      ints <- map truncate32pos <$> arbitrary -- 31bit int chunks
      pure $ X.mkInteger positive ints

instance Arbitrary X.BigNum where
  arbitrary =
    suchThatMap arbitrary $ \case
      (X.Bp# bn) -> pure bn
      _ -> Nothing

-- | Newtype to generate non-zero integer Arbitrary instance.
newtype NonZeroIntegers = NonZero Integers deriving Show

instance Arbitrary NonZeroIntegers where
  arbitrary = NonZero <$> arbitrary `suchThat` notZero
   where
    notZero (Integers (X.S# 0#) _) = False
    notZero (Integers _ y) = y /= 0

-- Truncate to a positive 32bit integer (required for mkInteger)
truncate32pos :: Int -> Int
truncate32pos i = abs i .&. 0x7fffffff
