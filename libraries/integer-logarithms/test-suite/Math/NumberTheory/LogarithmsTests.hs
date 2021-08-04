-- |
-- Module:      Math.NumberTheory.LogarithmsTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Logarithms
--

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.LogarithmsTests
  ( testSuite
  ) where

import Test.Tasty

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif
import Numeric.Natural

import Math.NumberTheory.Logarithms
import Math.NumberTheory.TestUtils

-- Arbitrary Natural
import Orphans ()

-- | Check that 'integerLogBase' returns the largest integer @l@ such that @b@ ^ @l@ <= @n@ and @b@ ^ (@l@+1) > @n@.
integerLogBaseProperty :: Positive Integer -> Positive Integer -> Bool
integerLogBaseProperty (Positive b) (Positive n) = b == 1 || b ^ l <= n && b ^ (l + 1) > n
  where
    l = toInteger $ integerLogBase b n

-- | Check that 'integerLog2' returns the largest integer @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
integerLog2Property :: Positive Integer -> Bool
integerLog2Property (Positive n) = 2 ^ l <= n && 2 ^ (l + 1) > n
  where
    l = toInteger $ integerLog2 n

integerLog2HugeProperty :: Huge (Positive Integer) -> Bool
integerLog2HugeProperty (Huge (Positive n)) = 2 ^ l <= n && 2 ^ (l + 1) > n
  where
    l = toInteger $ integerLog2 n

-- | Check that 'integerLog10' returns the largest integer @l@ such that 10 ^ @l@ <= @n@ and 10 ^ (@l@+1) > @n@.
integerLog10Property :: Positive Integer -> Bool
integerLog10Property (Positive n) = 10 ^ l <= n && 10 ^ (l + 1) > n
  where
    l = toInteger $ integerLog10 n

-- | Check that 'naturalLogBase' returns the largest natural @l@ such that @b@ ^ @l@ <= @n@ and @b@ ^ (@l@+1) > @n@.
naturalLogBaseProperty :: Positive Natural -> Positive Natural -> Bool
naturalLogBaseProperty (Positive b) (Positive n) = b == 1 || b ^ l <= n && b ^ (l + 1) > n
  where
    l = fromIntegral $ naturalLogBase b n

-- | Check that 'naturalLog2' returns the largest natural @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
naturalLog2Property :: Positive Natural -> Bool
naturalLog2Property (Positive n) = 2 ^ l <= n && 2 ^ (l + 1) > n
  where
    l = fromIntegral $ naturalLog2 n

naturalLog2HugeProperty :: Huge (Positive Natural) -> Bool
naturalLog2HugeProperty (Huge (Positive n)) = 2 ^ l <= n && 2 ^ (l + 1) > n
  where
    l = naturalLog2 n

-- | Check that 'naturalLog10' returns the largest natural @l@ such that 10 ^ @l@ <= @n@ and 10 ^ (@l@+1) > @n@.
naturalLog10Property :: Positive Natural -> Bool
naturalLog10Property (Positive n) = 10 ^ l <= n && 10 ^ (l + 1) > n
  where
    l = fromIntegral $ naturalLog10 n

-- | Check that 'intLog2' returns the largest integer @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
intLog2Property :: Positive Int -> Bool
intLog2Property (Positive n) = 2 ^ l <= n && (2 ^ (l + 1) > n || n > maxBound `div` 2)
  where
    l = intLog2 n

-- | Check that 'wordLog2' returns the largest integer @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
wordLog2Property :: Positive Word -> Bool
wordLog2Property (Positive n) = 2 ^ l <= n && (2 ^ (l + 1) > n || n > maxBound `div` 2)
  where
    l = wordLog2 n

-- | Check that 'integerLogBase'' returns the largest integer @l@ such that @b@ ^ @l@ <= @n@ and @b@ ^ (@l@+1) > @n@.
integerLogBase'Property :: Positive Integer -> Positive Integer -> Bool
integerLogBase'Property (Positive b) (Positive n) = b == 1 || b ^ l <= n && b ^ (l + 1) > n
  where
    l = toInteger $ integerLogBase' b n

-- | Check that 'integerLogBase'' returns the largest integer @l@ such that @b@ ^ @l@ <= @n@ and @b@ ^ (@l@+1) > @n@ for @b@ > 32 and @n@ >= @b@ ^ 2.
integerLogBase'Property2 :: Positive Integer -> Positive Integer -> Bool
integerLogBase'Property2 (Positive b') (Positive n') = b ^ l <= n && b ^ (l + 1) > n
  where
    b = b' + 32
    n = n' + b ^ 2 - 1
    l = toInteger $ integerLogBase' b n

-- | Check that 'integerLog2'' returns the largest integer @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
integerLog2'Property :: Positive Integer -> Bool
integerLog2'Property (Positive n) = 2 ^ l <= n && 2 ^ (l + 1) > n
  where
    l = toInteger $ integerLog2' n

-- | Check that 'integerLog10'' returns the largest integer @l@ such that 10 ^ @l@ <= @n@ and 10 ^ (@l@+1) > @n@.
integerLog10'Property :: Positive Integer -> Bool
integerLog10'Property (Positive n) = 10 ^ l <= n && 10 ^ (l + 1) > n
  where
    l = toInteger $ integerLog10' n

-- | Check that 'intLog2'' returns the largest integer @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
intLog2'Property :: Positive Int -> Bool
intLog2'Property (Positive n) = 2 ^ l <= n && (2 ^ (l + 1) > n || n > maxBound `div` 2)
  where
    l = intLog2' n

-- | Check that 'wordLog2'' returns the largest integer @l@ such that 2 ^ @l@ <= @n@ and 2 ^ (@l@+1) > @n@.
wordLog2'Property :: Positive Word -> Bool
wordLog2'Property (Positive n) = 2 ^ l <= n && (2 ^ (l + 1) > n || n > maxBound `div` 2)
  where
    l = wordLog2' n

testSuite :: TestTree
testSuite = testGroup "Logarithms"
  [ testSmallAndQuick "integerLogBase"  integerLogBaseProperty
  , testSmallAndQuick "integerLog2"     integerLog2Property
  , testSmallAndQuick "integerLog2Huge" integerLog2HugeProperty
  , testSmallAndQuick "integerLog10"    integerLog10Property
  , testSmallAndQuick "naturalLogBase"  naturalLogBaseProperty
  , testSmallAndQuick "naturalLog2"     naturalLog2Property
  , testSmallAndQuick "naturalLog2Huge" naturalLog2HugeProperty
  , testSmallAndQuick "naturalLog10"    naturalLog10Property
  , testSmallAndQuick "intLog2"         intLog2Property
  , testSmallAndQuick "wordLog2"        wordLog2Property

  , testSmallAndQuick "integerLogBase'" integerLogBase'Property
  , testSmallAndQuick "integerLogBase' with base > 32 and n >= base ^ 2"
      integerLogBase'Property2
  , testSmallAndQuick "integerLog2'"    integerLog2'Property
  , testSmallAndQuick "integerLog10'"   integerLog10'Property
  , testSmallAndQuick "intLog2'"        intLog2'Property
  , testSmallAndQuick "wordLog2'"       wordLog2'Property
  ]
