{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Prelude hiding (Integer)

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Bits
import GHC.Types
import GHC.Word
import GHC.Exts
import GHC.Num.BigNat
import GHC.Num.Natural
import GHC.Num.Integer

#include "MachDeps.h"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "integer"
   [ bigNatTests
   , naturalTests
   , integerTests
   ]

data BN = BN BigNat

(====) :: BigNat -> BigNat -> Property
(====) x y = BN x === BN y

infix 4 ====

bigNatQuotRemWord :: BigNat -> Word -> (BN,Word)
bigNatQuotRemWord bn (W# y) = case bigNatQuotRemWord# bn y of
   (# q, r #) -> (BN q, W# r)

bigNatQuotRem :: BigNat -> BigNat -> (BN,BN)
bigNatQuotRem a b = case bigNatQuotRem# a b of
   (# q, r #) -> (BN q, BN r)


instance Arbitrary BN where
   arbitrary = do
      ws <- arbitrary
      pure (BN (bigNatFromWordList ws))

instance Eq BN where
   BN x == BN y = bigNatEq x y

instance Arbitrary Natural where
   arbitrary = do
      BN x <- arbitrary
      pure (naturalFromBigNat x)

instance Arbitrary Integer where
   arbitrary = do
      I# sign <- arbitrary
      BN bn <- arbitrary
      pure (integerFromBigNatSign sign bn)

instance Show BN where
   show (BN b) = show (bigNatToWordList b)

instance Show Natural where
   show (NS w)  = show (W# w)
   show (NB bn) = show (bigNatToWordList bn)

instance Show Integer where
   show (IS i)  = show (I# i)
   show (IN bn) = '-' : show (bigNatToWordList bn)
   show (IP bn) = show (bigNatToWordList bn)

bigNatTests :: TestTree
bigNatTests = testGroup "BigNat"
   [ testProperty "Size 0" $
         bigNatSize (bigNatZero ()) === 0
   , testProperty "Size 1" $
         bigNatSize (bigNatFromWord# 5##) === 1
   , testProperty "Size 2" $
         bigNatSize (bigNatFromWord2# 5## 7##) === 2
   , testProperty "Size N" $
         \xs -> bigNatSize (bigNatFromWordList (fmap getNonZero xs))
                === fromIntegral (length xs)

   , testProperty "bigNatFromWordList [x] = bigNatFromWord# x" $
         \x@(W# x') ->
            bigNatFromWordList [x] ==== bigNatFromWord# x'
   , testProperty "bigNatFromWordList [x,y] = bigNatFromWord2# x y" $
         \x@(W# x') y@(W# y') ->
            bigNatFromWordList [x,y] ==== bigNatFromWord2# x' y'
   , testProperty "Test zero" $
         bigNatIsZero (bigNatZero ()) === True

   , testProperty "Trim ending zeroes: BigNat xs == BigNat (replicate i 0 ++ xs)" $
         \xs i -> bigNatFromWordList xs
                  ==== bigNatFromWordList (replicate i 0 ++ xs)

   , testProperty "Word roundtrip through BigNat" $
         \x -> bigNatToWord (bigNatFromWord x) === x
   , testProperty "BigNat [x] > y <=> x > y" $
         \x y -> (bigNatFromWord x `bigNatGtWord` y) === (x > y)
   , testProperty "BigNat [x] <= y <=> x <= y" $
         \x y -> (bigNatFromWord x `bigNatLeWord` y) === (x <= y)
   , testProperty "BigNat xs + 0 == BigNat xs" $
         \(BN x) -> bigNatAddWord# x 0## ==== x
   , testProperty "BigNat [x] + y == x+y (x and y small)" $
         \(W16# x) (W16# y) ->
            bigNatToWord (bigNatAddWord# (bigNatFromWord# x) y)
            === W# (x `plusWord#` y)
   , testProperty "BigNat [x] `compare` BigNat [y] == x `compare` y" $
         \x y -> bigNatFromWord x `bigNatCompare` bigNatFromWord y == x `compare` y


   , testProperty "BigNat [x,0,0] `compare` BigNat [y,0,0] == x `compare` y" $
         \x y -> bigNatFromWordList [x,0,0] `bigNatCompare` bigNatFromWordList [y,0,0]
                 === x `compare` y

   , testProperty "popCount (BigNat xs) == sum (fmap popCount xs)" $
         \xs -> bigNatPopCount (bigNatFromWordList xs)
                === fromIntegral (sum (fmap popCount xs))

   -- Or, And, Xor

   , testProperty "checkInvariants (x `or` y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatOr` y)

   , testProperty "checkInvariants (x `xor` y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatXor` y)

   , testProperty "checkInvariants (x `and` y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatAnd` y)

   , testProperty "BigNat xs `or` BigNat ys == BigNat (zipWith or xs' ys') (padded xs',ys')" $
         \xs ys ->
            let
               (xs',ys') = case length xs - length ys of
                  0             -> (xs,ys)
                  l | l > 0     -> (xs,replicate l 0 ++ ys)
                    | otherwise -> (replicate (negate l) 0 ++ xs,ys)
            in bigNatFromWordList xs `bigNatOr` bigNatFromWordList ys
               ==== bigNatFromWordList (zipWith (.|.) xs' ys')

   , testProperty "BigNat xs `xor` BigNat ys == BigNat (zipWith xor xs' ys') (padded xs',ys')" $
         \xs ys ->
            let
               (xs',ys') = case length xs - length ys of
                  0             -> (xs,ys)
                  l | l > 0     -> (xs,replicate l 0 ++ ys)
                    | otherwise -> (replicate (negate l) 0 ++ xs,ys)
            in bigNatFromWordList xs `bigNatXor` bigNatFromWordList ys
               ==== bigNatFromWordList (zipWith xor xs' ys')

   , testProperty "BigNat xs `and` BigNat ys == BigNat (zipWith and xs' ys')" $
         \xs ys ->
            bigNatFromWordList xs `bigNatAnd` bigNatFromWordList ys
            ==== bigNatFromWordList (reverse (zipWith (Data.Bits..&.) (reverse xs) (reverse ys)))

   -- Bit shifts

   , testProperty "checkInvariants (BigNat xs `shiftL` n)" $
         \xs n -> bigNatCheck (bigNatFromWordList xs `bigNatShiftL` n)

   , testProperty "checkInvariants (BigNat xs `shiftR` n)" $
         \xs n -> bigNatCheck (bigNatFromWordList xs `bigNatShiftR` n)


   , testProperty "BigNat xs `shiftR` WS*n == BigNat (take (length xs - n) xs)" $
         \xs n -> bigNatFromWordList xs `bigNatShiftR` (WORD_SIZE_IN_BITS*n)
                ==== bigNatFromWordList (take (length xs - fromIntegral n) xs)

   , testProperty "1 `shiftL` n ==> isPowerOf2 n" $
         \n -> let
                  a = bigNatOne () `bigNatShiftL` n
               in case bigNatIsPowerOf2# a of
                     (# | n' #) -> n == W# n'
                     _          -> False

#if WORD_SIZE_IN_BITS == 64
   , testProperty "BigNat [0xAABBAA...,0xAABBAA..] `shiftR` WS+4 == BigNat [0x0AABB,..])" $
         let v  = 0xAABBAABBAABBAABB
             v0 = 0x0AABBAABBAABBAAB
             v1 = 0xBAABBAABBAABBAAB
         in bigNatFromWordList [v,v,v] `bigNatShiftR` 68
            ==== bigNatFromWordList [v0,v1]
#elif WORD_SIZE_IN_BITS == 32
   , testProperty "BigNat [0xAABBAA...,0xAABBAA..] `shiftR` WS+4 == BigNat [0x0AABB,..])" $
         let v  = 0xAABBAABB
             v0 = 0x0AABBAAB
             v1 = 0xBAABBAAB
         in bigNatFromWordList [v,v,v] `bigNatShiftR` 36
            ==== bigNatFromWordList [v0,v1]
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

   , testProperty "BigNat xs `shiftL` WS*n == BigNat (xs ++ replicate n 0)" $
         \xs n -> bigNatFromWordList xs `bigNatShiftL` (WORD_SIZE_IN_BITS*n)
                  ==== bigNatFromWordList (xs ++ replicate (fromIntegral n) 0)

#if WORD_SIZE_IN_BITS == 64
   , testProperty "BigNat [0xAABBAA...,0xAABBAA..] `shiftL` WS+4 == BigNat [0xA,0xABB,..])" $
         let v  = 0xAABBAABBAABBAABB
             v0 = 0xABBAABBAABBAABBA
             v1 = 0xABBAABBAABBAABB0
         in bigNatFromWordList [v,v,v] `bigNatShiftL` 68
            ==== bigNatFromWordList [0xA,v0,v0,v1,0]
#elif WORD_SIZE_IN_BITS == 32
   , testProperty "BigNat [0xAABBAA...,0xAABBAA..] `shiftL` WS+4 == BigNat [0xA,0xABB,..])" $
         let v  = 0xAABBAABB
             v0 = 0xABBAABBA
             v1 = 0xABBAABB0
         in bigNatFromWordList [v,v,v] `bigNatShiftL` 36
            ==== bigNatFromWordList [0xA,v0,v0,v1,0]
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

   , testProperty "BigNat [2] `shiftL` 1 == BigNat [4]" $
         bigNatFromWord 2 `bigNatShiftL` 1 ==== bigNatFromWord 4


   -- bit, testBit

   , testProperty "testBit n (BigNat 1 `shiftL` n)) == True" $
      \n -> bigNatTestBit (bigNatFromWord# 1## `bigNatShiftL` n) n

   , testProperty "testBit n (bigNatBit n) == True" $
      \n -> bigNatTestBit (bigNatBit n) n

   , testProperty "BigNat 1 `shiftL` n == bigNatBit n" $
      \n -> bigNatFromWord# 1## `bigNatShiftL` n ==== bigNatBit n


   , testProperty "checkInvariants (bigNatBit n)" $
      \n -> bigNatCheck (bigNatBit n)


   -- Addition

   , testProperty "checkInvariants (x + y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatAdd` y)

   , testProperty "BigNat [x] + BigNat [y] == BigNat (x `plusWord2#` y)" $
         \(W# x) (W# y)
            -> bigNatFromWord# x `bigNatAdd` bigNatFromWord# y
               ==== case plusWord2# x y of
                     (# c,r #) -> bigNatFromWord2# c r

   , testProperty "BigNat xs + BigNat ys == BigNat ys + BigNat xs" $
         \xs ys
            -> bigNatFromWordList xs `bigNatAdd` bigNatFromWordList ys
               ==== bigNatFromWordList ys `bigNatAdd` bigNatFromWordList xs



   -- Multiplication

   , testProperty "checkInvariants (x * y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatMul` y)

   , testProperty "Simple example" $
         bigNatFromWordList [6148914691236517205,12297829382473034410] `bigNatMulWord` 3 ==== bigNatFromWordList [1,0,18446744073709551614]

   , testProperty "Simple example bis" $
         bigNatFromWordList [6148914691236517205,12297829382473034410] `bigNatMul` bigNatFromWord 3 ==== bigNatFromWordList [1,0,18446744073709551614]


   , testProperty "BigNat [x] * y == BigNat (x `timesWord2#` y)" $
         \(W# x) (W# y)
            -> bigNatFromWord# x `bigNatMulWord#` y
               ==== case timesWord2# x y of
                     (# c,r #) -> bigNatFromWord2# c r

   , testProperty "BigNat [1,0,0] * 2 == BigNat [2,0,0]" $
         bigNatFromWordList [1,0,0] `bigNatMulWord` 2 ==== bigNatFromWordList [2,0,0]

   , testProperty "BigNat [1 << WS-1,0,0] * 2 == BigNat [1,0,0,0]" $
         bigNatFromWordList [1 `shiftL` (WORD_SIZE_IN_BITS - 1),0,0] `bigNatMulWord` 2 ==== bigNatFromWordList [1,0,0,0]

   , testProperty "BigNat xs * y == BigNat xs * Bignat [y]" $
         \xs y
            -> let
                  x = bigNatFromWordList xs
               in x`bigNatMulWord` y ==== x `bigNatMul` bigNatFromWord y

   , testProperty "BigNat xs * BigNat ys == BigNat ys * BigNat xs" $
         \xs ys
            -> bigNatFromWordList xs `bigNatMul` bigNatFromWordList ys
               ==== bigNatFromWordList ys `bigNatMul` bigNatFromWordList xs

   , testProperty "BigNat [1,0] * BigNat [1,0] == BigNat [1,0,0]" $
         bigNatFromWordList [1,0] `bigNatMul` bigNatFromWordList [1,0]
               ==== bigNatFromWordList [1,0,0]



   -- Subtraction
   , testProperty "checkInvariants (x - y)" $
         \(BN x) (BN y) -> x `bigNatCompare` y /= LT ==> bigNatCheck (x `bigNatSubUnsafe` y)

   , testProperty "BigNat [1,1,0] - 1 == BigNat [1,0,MAX_WORD]" $
         bigNatFromWordList [1,1,0] `bigNatSubWordUnsafe` 1 ==== bigNatFromWordList [1,0,maxBound]

   , testProperty "BigNat [1,1,0] - BigNat [2] == BigNat [1,0,MAX_WORD-1]" $
         bigNatFromWordList [1,1,0] `bigNatSubUnsafe` bigNatFromWordList [2] ==== bigNatFromWordList [1,0,maxBound-1]

   , testProperty "BigNat [x] - y == BigNat (x-y)" $
         \x y -> if
            | x > y -> bigNatFromWord x `bigNatSubWordUnsafe` y
                       ==== bigNatFromWord (x-y)
            | otherwise -> bigNatFromWord y `bigNatSubWordUnsafe` x
                       ==== bigNatFromWord (y-x)

   , testProperty "BigNat [3,2] - BigNat [3,1] == BigNat [1]" $
         bigNatFromWordList [3,2] `bigNatSubUnsafe` bigNatFromWordList [3,1]
         ==== bigNatFromWordList [1]

   , testProperty "BigNat [x] - BigNat [y] + BigNat [y] == BigNat [x]" $
         \wx wy ->
            let
               x = bigNatFromWord wx
               y = bigNatFromWord wy
               test u v = (u `bigNatSubUnsafe` v) `bigNatAdd` v ==== u
            in case bigNatCompare x y of
                  LT -> test y x
                  _  -> test x y

   , testProperty "BigNat xs - BigNat ys + BigNat ys == BigNat xs" $
         \xs ys ->
            let
               x = bigNatFromWordList xs
               y = bigNatFromWordList ys
               test u v = (u `bigNatSubUnsafe` v) `bigNatAdd` v ==== u
            in case bigNatCompare x y of
                  LT -> test y x
                  _  -> test x y

   -- QuotRem by Word

   , testProperty "(x * n) `div` n === x" $
         \xs (NonZero n) ->
            let
               x     = bigNatFromWordList xs
               xn    = x `bigNatMulWord` n
               x'    = bigNatQuotWord xn n
               prop  = x ==== x'
            in counterexample (show (BN x) ++ " * " ++ show n ++ " / " ++ show n ++ " /= " ++ show (BN x)) prop

   , testProperty "(q,r) <- BigNat xs `quotRem` y <==> (BigNat xs - r) `div` y == q" $
         \xs (NonZero y) ->
            let
               x     = bigNatFromWordList xs
               (q,r) = bigNatQuotRemWord x y
            in BN((x `bigNatSubWordUnsafe` r) `bigNatQuotWord` y) === q

   , testProperty "(q,r) <- BigNat xs `quotRem` y <==> (BigNat xs - r) == q*y" $
         \xs (NonZero y) ->
            let
               x     = bigNatFromWordList xs
               !(BN q,r) = bigNatQuotRemWord x y
            in (x `bigNatSubWordUnsafe` r) ==== q `bigNatMulWord` y

   , testProperty "(q,r) <- BigNat xs `quotRem` y <==> BigNat xs == q * y + r)" $
         \xs (NonZero y) ->
            let
               x     = bigNatFromWordList xs
               !(BN q,r) = bigNatQuotRemWord x y
            in (q `bigNatMulWord` y) `bigNatAddWord` r ==== x

   -- QuotRem by BigNat

   , testProperty "checkInvariants (q,r) | (q,r) <- (x `quotRem` y)" $
         \(BN x) (BN y) -> not (bigNatIsZero y) ==>
                  let
                     !(BN q,BN r) = x `bigNatQuotRem` y
                  in bigNatCheck q && bigNatCheck r

   , testProperty "(q,r) <- BigNat [1,0] `quotRem` BigNat [1,0]" $
            let
               x     = bigNatFromWordList [1,0]
               y     = bigNatFromWordList [1,0]
               (q,r) = bigNatQuotRem x y
            in (q,r) === (BN (bigNatFromWord 1), BN (bigNatFromWord 0))

   , testProperty "x `quotRem` x == (BigNat [1], 0)" $
         \xs ->
            let
               x     = bigNatFromWordList xs
            in not (bigNatIsZero x) ==> (x `bigNatQuotRem` x) === (BN (bigNatOne ()), BN (bigNatZero ()))

   , testProperty "BigNat [MAX_WORD,3] `quotRem` BigNat [MAX_WORD,0] == (BigNat [1], BigNat [3])" $
         (bigNatFromWordList [maxBound,3] `bigNatQuotRem` bigNatFromWordList [maxBound,0])
         === (BN (bigNatFromWordList [1]), BN (bigNatFromWordList [3]))

   , testProperty "BigNat [MAX_WORD,3] `quotRem` BigNat [MAX_WORD,1] == (BigNat [1], BigNat [2])" $
         (bigNatFromWordList [maxBound,3] `bigNatQuotRem` bigNatFromWordList [maxBound,1])
         === (BN (bigNatFromWordList [1]), BN (bigNatFromWordList [2]))

   , testProperty "BigNat [MAX_WORD,MAX_WORD,3] `quotRem` BigNat [MAX_WORD,0] == (BigNat [1,1], BigNat [3])" $
         (bigNatFromWordList [maxBound,maxBound,3] `bigNatQuotRem` bigNatFromWordList [maxBound,0])
         === (BN (bigNatFromWordList [1,1]), BN (bigNatFromWordList [3]))


   , testProperty "BigNat [2,3,4] `quotRem` BigNat [1,0] == (BigNat [2,3], BigNat [4])" $
         (bigNatFromWordList [2,3,4] `bigNatQuotRem` bigNatFromWordList [1,0])
         === (BN (bigNatFromWordList [2,3]), BN (bigNatFromWordList [4]))

   , testProperty "(q,r) <- x `quotRem` y <==> (x-r) `div` y == q" $
         \xs ys ->
            let
               x     = bigNatFromWordList xs
               y     = bigNatFromWordList ys
               !(BN q,BN r) = bigNatQuotRem x y
            in not (bigNatIsZero y) ==> (x `bigNatSubUnsafe` r) `bigNatQuot` y ==== q

   , testProperty "(q,r) <- x `quotRem` y ==> r < y" $
         \xs ys ->
            let
               x     = bigNatFromWordList xs
               y     = bigNatFromWordList ys
               !(_,BN r) = bigNatQuotRem x y
               prop  = (r `bigNatCompare` y) == LT
            in not (bigNatIsZero y) ==>
                  counterexample (show (BN r) ++ " is not < " ++ show (BN y)) prop

   , testProperty "(q,r) <- x `quotRem` y ==> q*y <= x" $
         \xs ys ->
            let
               x     = bigNatFromWordList xs
               y     = bigNatFromWordList ys
               !(BN q,_) = bigNatQuotRem x y
               prop  = ((q `bigNatMul` y) `bigNatCompare` x) /= GT
            in not (bigNatIsZero y) ==>
                  counterexample (show (BN q) ++ " * " ++ show (BN y) ++ " is not <= " ++ show (BN x)) prop

   , testProperty "(q,r) <- x `quotRem` y <==> x-r == q*y" $
         \xs ys ->
            let
               x     = bigNatFromWordList xs
               y     = bigNatFromWordList ys
               !(BN q,BN r) = bigNatQuotRem x y
               prop  = (x `bigNatSubUnsafe` r) ==== q `bigNatMul` y
            in not (bigNatIsZero y) ==>
                  counterexample (show (BN x) ++ " - " ++ show (BN r) ++ " /= " ++ show (BN q) ++ " * " ++ show (BN y)) prop

   , testProperty "(q,r) <- x `quotRem` y <==> x == q * y + r)" $
         \xs ys ->
            let
               x     = bigNatFromWordList xs
               y     = bigNatFromWordList ys
               !(BN q,BN r) = bigNatQuotRem x y
            in not (bigNatIsZero y) ==> (q `bigNatMul` y) `bigNatAdd` r ==== x

   -- Quot by BigNat

   , testProperty "checkInvariants (x `quot` y)" $
         \(BN x) (BN y) -> not (bigNatIsZero y) ==> bigNatCheck (x `bigNatQuot` y)

   , testProperty "(q,_) <- (x `quotRem` y) <==> q == x `quot` y" $
         \(BN x) (BN y) -> not (bigNatIsZero y) ==>
                  let
                     !(BN q,_r) = x `bigNatQuotRem` y
                     q'     = x `bigNatQuot` y
                  in q ==== q'

   -- Rem by BigNat

   , testProperty "checkInvariants (x `rem` y)" $
         \(BN x) (BN y) -> not (bigNatIsZero y) ==> bigNatCheck (x `bigNatRem` y)

   , testProperty "BigNat [10,20,30] `rem` BigNat [22] == BigNat [6]" $
         bigNatFromWordList [10,20,30] `bigNatRem` bigNatFromWordList[22]
         ==== bigNatFromWordList [6]

   , testProperty "(_,r) <- (x `quotRem` y) <==> r == x `rem` y" $
         \(BN x) (BN y) -> not (bigNatIsZero y) ==>
                  let
                     !(_q,BN r) = x `bigNatQuotRem` y
                     r'     = x `bigNatRem` y
                  in r ==== r'

   -- GCD

   , testProperty "checkInvariants (x `gcd` y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatGcd` y)

   , testProperty "BigNat [10,20,30] `gcd` BigNat [22,34,50] == BigNat [2]" $
         bigNatFromWordList [10,20,30] `bigNatGcd` bigNatFromWordList[22,34,50]
         ==== bigNatFromWordList [2]

   , testProperty "BigNat [10,0,0] `gcd` BigNat [5,0,0] == BigNat [5,0,0]" $
         bigNatFromWordList [10,0,0] `bigNatGcd` bigNatFromWordList[5,0,0]
         ==== bigNatFromWordList [5,0,0]


   , testProperty "x `gcd` 0 == x" $
         \(BN x) -> x `bigNatGcd` bigNatFromWord 0
         ==== x

   , testProperty "BigNat [30] `gcd` BigNat [50] == BigNat [2]" $
         bigNatFromWordList [30] `bigNatGcd` bigNatFromWordList[50]
         ==== bigNatFromWordList [10]

   -- LCM

   , testProperty "checkInvariants (x `lcm` y)" $
         \(BN x) (BN y) -> bigNatCheck (x `bigNatLcm` y)

   , testProperty "x `lcm` y == (x * y) / (a `gcd` b)" $
         \(BN x) (BN y) ->
            not (bigNatIsZero x && bigNatIsZero y)
            ==> x `bigNatLcm` y
                ==== (x `bigNatMul` y) `bigNatQuot` (x `bigNatGcd` y)

   -- Log

   , testProperty "logBase 2 8 == 3" $
         bigNatLogBaseWord 2 (bigNatFromWord 8) === 3

   , testProperty "logBase 5 625 == 4" $
         bigNatLogBaseWord 5 (bigNatFromWord 625) === 4

   , testProperty "logBase 5 626 == 4" $
         bigNatLogBaseWord 5 (bigNatFromWord 626) === 4

   , testProperty "logBase 2 0xFF == 7" $
         bigNatLogBaseWord 2 (bigNatFromWord 0xFF) === 7

   , testProperty "logBase 3 0xFF == 5" $
         bigNatLogBaseWord 3 (bigNatFromWord 0xFF) === 5


   -- Size in base

   , testProperty "sizeInBase n 0x00 == 0" $
         \n -> n >= 2 ==> bigNatSizeInBase n (bigNatFromWord 0x0) === 0

   , testProperty "sizeInBase 2 0xFF == 8" $
         bigNatSizeInBase 2 (bigNatFromWord 0xFF) === 8

   , testProperty "sizeInBase 4 (BigNat [1,2]) == WORD_SIZE_IN_BITS/2 + 1" $
         bigNatSizeInBase 4 (bigNatFromWordList [1,2]) === WORD_SIZE_IN_BITS `div`2 + 1

   , testProperty "sizeInBase 3 0xABCD == 10" $
         bigNatSizeInBase 3 (bigNatFromWord 0xABCD) === 10

   ]

naturalTests :: TestTree
naturalTests = testGroup "Natural"
   [ testProperty "check invariants on random Natural" $
         \n -> naturalCheck n
   , testProperty "Natural [x] `compare` Natural [y] == x `compare` y" $
         \x y -> naturalFromWord x `compare` naturalFromWord y == x `compare` y
   , testProperty "Natural xs `shiftR` WS*n == Natural (take (length xs - n) xs)" $
         \xs n -> naturalFromWordList xs `naturalShiftR` (WORD_SIZE_IN_BITS*n)
                === naturalFromWordList (take (length xs - fromIntegral n) xs)

   ]

integerTests :: TestTree
integerTests = testGroup "Integer"
   [ testProperty "check invariants on random Integer" $
         \i -> integerCheck i
   , testProperty "Integer [x] `compare` Integer [y] == x `compare` y (x,y :: Word)" $
         \x y -> integerFromWord x `integerCompare` integerFromWord y == x `compare` y

   , testProperty "Integer [x] `compare` Integer [y] == x `compare` y (x,y :: Int)" $
         \x y -> integerFromInt x `integerCompare` integerFromInt y == x `compare` y

   , testProperty "compare x y === compare (x-y) 0" $
         \x y -> x `integerCompare` y === (x `integerSub` y) `integerCompare` integerFromInt 0

   , testProperty "x + y - x == y" $
         \x y -> x `integerAdd` y `integerSub` x === y

   , testProperty "signum (-10) == -1" $
         integerSignum (integerFromInt (-10)) === integerFromInt (-1)

   , testProperty "signum 10 == 1" $
         integerSignum (integerFromInt 10) === integerFromInt 1

   , testProperty "x + y == y + x" $
         \x y -> x `integerAdd` y  === y `integerAdd` x

   , testProperty "BigNat (Integer (BigNat x)) = x" $
         \(BN x) -> integerToBigNatClamp (integerFromBigNat x) ==== x

   , testProperty "BigNat x `compare` BigNat y == Integer x `compare` Integer y" $
         \(BN x) (BN y) -> x `bigNatCompare` y
                 === integerFromBigNat x `integerCompare` integerFromBigNat y

   , testProperty "-1 * x == negate x" $
         \x -> integerFromInt (-1) `integerMul` x === integerNegate x

   , testProperty "x <= 0 ==> abs x == negate x" $
         \x -> x `integerLe` integerZero
                  ==> integerAbs x === integerNegate x

   , testProperty "integerEncodeDouble# 2 1 = 4.0" $
         D# (integerEncodeDouble# (IS 2#) 1#) === D# 4.0##

   , testProperty "integerEncodeDouble# 2 -2 = 0.5" $
         D# (integerEncodeDouble# (IS 2#) -2#) === D# 0.5##

   , testProperty "integerDecodeDouble 0.5 = (..., ...)" $
         integerDecodeDouble 0.5 === (IS 4503599627370496#,-53)

   , testProperty "integerDecodeDouble -18 = (..., ...)" $
         integerDecodeDouble (-18) === (IS -5066549580791808#,-48)


   , testProperty "integerDecodeDouble 4.2 = (..., ...)" $
         integerDecodeDouble 4.2 === (IS 4728779608739021#,-50)

   , testProperty "integerEncodeDouble (integerDecodeDouble x) = x " $
         \x -> uncurry integerEncodeDouble (integerDecodeDouble x) === x

   , testProperty "(q,r) <- x `quotRem` y <==> x == q * y + r)" $
         \x y ->
            let (q,r) = integerQuotRem x y
            in not (integerIsZero y) ==> (q `integerMul` y) `integerAdd` r === x

   , testProperty "(0,-1) <- -1 `quotRem` -2" $
            (IS 0#, IS -1#) === integerQuotRem (IS -1#) (IS -2#)

   , testProperty "(4,1) <- 21 `quotRem` 5" $
            (IS 4#, IS 1#) === integerQuotRem (IS 21#) (IS 5#)

   , testProperty "checkInvariants (x `shiftL` n)" $
         \x n -> integerCheck (x `integerShiftL` n)

   , testProperty "1 `shiftL` n ==> isPowerOf2 n" $
         \n -> let
                  a = integerOne `integerShiftL` n
               in case integerIsPowerOf2# a of
                     (# | n' #) -> n == W# n'
                     _          -> False

   , testProperty "x `shiftL` 1 ==> x*2" $
         \x -> x `integerShiftL` 1 === x `integerMul` (IS 2#)

   , testProperty "integerSignum (x `shiftL` n) === integerSignum x" $
         \x n -> integerSignum (x `integerShiftL` n) === integerSignum x

   , testProperty "Negative shiftR (T12136)" $
      let
         w  = integerFromWord 1 `integerShiftL` 128
         w1 = integerFromWord 1 `integerSub` w
      in
         w1 `integerShiftR` 64 === integerFromWordList True [1,0]
   ]
