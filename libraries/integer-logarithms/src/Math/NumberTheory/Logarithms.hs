-- |
-- Module:      Math.NumberTheory.Logarithms
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Integer Logarithms. For efficiency, the internal representation of 'Integer's
-- from integer-gmp is used.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Math.NumberTheory.Logarithms
    ( -- * Integer logarithms with input checks
      integerLogBase
    , integerLog2
    , integerLog10

    , naturalLogBase
    , naturalLog2
    , naturalLog10

    , intLog2
    , wordLog2

      -- * Integer logarithms without input checks
      --
      -- | These functions are total, however, don't rely on the values with out-of-domain arguments.
    , integerLogBase'
    , integerLog2'
    , integerLog10'

    , intLog2'
    , wordLog2'
    ) where

import GHC.Exts

import Data.Bits
import Data.Array.Unboxed
import Numeric.Natural

#ifdef MIN_VERSION_ghc_bignum
import qualified GHC.Num.Natural as BN
#endif

import GHC.Integer.Logarithms.Compat
#if MIN_VERSION_base(4,8,0) && defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals (Integer (..))
import GHC.Natural
#endif

#if CheckBounds
import Data.Array.IArray (IArray, (!))
#else
import Data.Array.Base (unsafeAt)
#endif

-- | Calculate the integer logarithm for an arbitrary base.
--   The base must be greater than 1, the second argument, the number
--   whose logarithm is sought, must be positive, otherwise an error is thrown.
--   If @base == 2@, the specialised version is called, which is more
--   efficient than the general algorithm.
--
--   Satisfies:
--
-- > base ^ integerLogBase base m <= m < base ^ (integerLogBase base m + 1)
--
-- for @base > 1@ and @m > 0@.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b n
  | n < 1       = error "Math.NumberTheory.Logarithms.integerLogBase: argument must be positive."
  | n < b       = 0
  | b == 2      = integerLog2' n
  | b < 2       = error "Math.NumberTheory.Logarithms.integerLogBase: base must be greater than one."
  | otherwise   = integerLogBase' b n

-- | Calculate the integer logarithm of an 'Integer' to base 2.
--   The argument must be positive, otherwise an error is thrown.
integerLog2 :: Integer -> Int
integerLog2 n
  | n < 1       = error "Math.NumberTheory.Logarithms.integerLog2: argument must be positive"
  | otherwise   = I# (integerLog2# n)

-- | Cacluate the integer logarithm for an arbitrary base.
--   The base must be greater than 1, the second argument, the number
--   whose logarithm is sought, must be positive, otherwise an error is thrown.
--   If @base == 2@, the specialised version is called, which is more
--   efficient than the general algorithm.
--
--   Satisfies:
--
-- > base ^ integerLogBase base m <= m < base ^ (integerLogBase base m + 1)
--
-- for @base > 1@ and @m > 0@.
naturalLogBase :: Natural -> Natural -> Int
naturalLogBase b n
  | n < 1       = error "Math.NumberTheory.Logarithms.naturalLogBase: argument must be positive."
  | n < b       = 0
  | b == 2      = naturalLog2' n
  | b < 2       = error "Math.NumberTheory.Logarithms.naturalLogBase: base must be greater than one."
  | otherwise   = naturalLogBase' b n

-- | Calculate the natural logarithm of an 'Natural' to base 2.
--   The argument must be non-zero, otherwise an error is thrown.
naturalLog2 :: Natural -> Int
naturalLog2 n
  | n < 1       = error "Math.NumberTheory.Logarithms.naturalLog2: argument must be non-zero"
  | otherwise   = I# (naturalLog2# n)

-- | Calculate the integer logarithm of an 'Int' to base 2.
--   The argument must be positive, otherwise an error is thrown.
intLog2 :: Int -> Int
intLog2 (I# i#)
  | isTrue# (i# <# 1#)  = error "Math.NumberTheory.Logarithms.intLog2: argument must be positive"
  | otherwise           = I# (wordLog2# (int2Word# i#))

-- | Calculate the integer logarithm of a 'Word' to base 2.
--   The argument must be positive, otherwise an error is thrown.
wordLog2 :: Word -> Int
wordLog2 (W# w#)
  | isTrue# (w# `eqWord#` 0##)  = error "Math.NumberTheory.Logarithms.wordLog2: argument must not be 0."
  | otherwise                   = I# (wordLog2# w#)

-- | Same as 'integerLog2', but without checks, saves a little time when
--   called often for known good input.
integerLog2' :: Integer -> Int
integerLog2' n = I# (integerLog2# n)

-- | Same as 'naturalLog2', but without checks, saves a little time when
--   called often for known good input.
naturalLog2' :: Natural -> Int
naturalLog2' n = I# (naturalLog2# n)

-- | Same as 'intLog2', but without checks, saves a little time when
--   called often for known good input.
intLog2' :: Int -> Int
intLog2' (I# i#) = I# (wordLog2# (int2Word# i#))

-- | Same as 'wordLog2', but without checks, saves a little time when
--   called often for known good input.
wordLog2' :: Word -> Int
wordLog2' (W# w#) = I# (wordLog2# w#)

-- | Calculate the integer logarithm of an 'Integer' to base 10.
--   The argument must be positive, otherwise an error is thrown.
integerLog10 :: Integer -> Int
integerLog10 n
  | n < 1     = error "Math.NumberTheory.Logarithms.integerLog10: argument must be positive"
  | otherwise = integerLog10' n

-- | Calculate the integer logarithm of an 'Integer' to base 10.
--   The argument must be not zero, otherwise an error is thrown.
naturalLog10 :: Natural -> Int
naturalLog10 n
  | n < 1     = error "Math.NumberTheory.Logarithms.naturalaLog10: argument must be non-zero"
  | otherwise = naturalLog10' n

-- | Same as 'integerLog10', but without a check for a positive
--   argument. Saves a little time when called often for known good
--   input.
integerLog10' :: Integer -> Int
integerLog10' n
  | n < 10      = 0
  | n < 100     = 1
  | otherwise   = ex + integerLog10' (n `quot` 10 ^ ex)
    where
      ln = I# (integerLog2# n)
      -- u/v is a good approximation of log 2/log 10
      u  = 1936274
      v  = 6432163
      -- so ex is a good approximation to integerLogBase 10 n
      ex = fromInteger ((u * fromIntegral ln) `quot` v)

-- | Same as 'naturalLog10', but without a check for a positive
--   argument. Saves a little time when called often for known good
--   input.
naturalLog10' :: Natural -> Int
naturalLog10' n
  | n < 10      = 0
  | n < 100     = 1
  | otherwise   = ex + naturalLog10' (n `quot` 10 ^ ex)
    where
      ln = I# (naturalLog2# n)
      -- u/v is a good approximation of log 2/log 10
      u  = 1936274
      v  = 6432163
      -- so ex is a good approximation to naturalLogBase 10 n
      ex = fromInteger ((u * fromIntegral ln) `quot` v)

-- | Same as 'integerLogBase', but without checks, saves a little time when
--   called often for known good input.
integerLogBase' :: Integer -> Integer -> Int
integerLogBase' b n
  | n < b       = 0
  | ln-lb < lb  = 1     -- overflow safe version of ln < 2*lb, implies n < b*b
  | b < 33      = let bi = fromInteger b
                      ix = 2*bi-4
                      -- u/v is a good approximation of log 2/log b
                      u  = logArr `unsafeAt` ix
                      v  = logArr `unsafeAt` (ix+1)
                      -- hence ex is a rather good approximation of integerLogBase b n
                      -- most of the time, it will already be exact
                      ex = fromInteger ((fromIntegral u * fromIntegral ln) `quot` fromIntegral v)
                  in case u of
                      1 -> ln `quot` v      -- a power of 2, easy
                      _ -> ex + integerLogBase' b (n `quot` b ^ ex)
  | otherwise   = let -- shift b so that 16 <= bi < 32
                      bi = fromInteger (b `shiftR` (lb-4))
                      -- we choose an approximation of log 2 / log (bi+1) to
                      -- be sure we underestimate
                      ix = 2*bi-2
                      -- u/w is a reasonably good approximation to log 2/log b
                      -- it is too small, but not by much, so the recursive call
                      -- should most of the time be caught by one of the first
                      -- two guards unless n is huge, but then it'd still be
                      -- a call with a much smaller second argument.
                      u  = fromIntegral $ logArr `unsafeAt` ix
                      v  = fromIntegral $ logArr `unsafeAt` (ix+1)
                      w  = v + u*fromIntegral (lb-4)
                      ex = fromInteger ((u * fromIntegral ln) `quot` w)
                  in ex + integerLogBase' b (n `quot` b ^ ex)
    where
      lb = integerLog2' b
      ln = integerLog2' n

-- | Same as 'naturalLogBase', but without checks, saves a little time when
--   called often for known good input.
naturalLogBase' :: Natural -> Natural -> Int
naturalLogBase' b n
    | n < b       = 0
  | ln-lb < lb  = 1     -- overflow safe version of ln < 2*lb, implies n < b*b
  | b < 33      = let bi = fromIntegral b
                      ix = 2*bi-4
                      -- u/v is a good approximation of log 2/log b
                      u  = logArr `unsafeAt` ix
                      v  = logArr `unsafeAt` (ix+1)
                      -- hence ex is a rather good approximation of integerLogBase b n
                      -- most of the time, it will already be exact
                      ex = fromNatural ((fromIntegral u * fromIntegral ln) `quot` fromIntegral v)
                  in case u of
                      1 -> ln `quot` v      -- a power of 2, easy
                      _ -> ex + naturalLogBase' b (n `quot` b ^ ex)
  | otherwise   = let -- shift b so that 16 <= bi < 32
                      bi = fromNatural (b `shiftR` (lb-4))
                      -- we choose an approximation of log 2 / log (bi+1) to
                      -- be sure we underestimate
                      ix = 2*bi-2
                      -- u/w is a reasonably good approximation to log 2/log b
                      -- it is too small, but not by much, so the recursive call
                      -- should most of the time be caught by one of the first
                      -- two guards unless n is huge, but then it'd still be
                      -- a call with a much smaller second argument.
                      u  = fromIntegral $ logArr `unsafeAt` ix
                      v  = fromIntegral $ logArr `unsafeAt` (ix+1)
                      w  = v + u*fromIntegral (lb-4)
                      ex = fromNatural ((u * fromIntegral ln) `quot` w)
                  in ex + naturalLogBase' b (n `quot` b ^ ex)
    where
      lb = naturalLog2' b
      ln = naturalLog2' n

-- Lookup table for logarithms of 2 <= k <= 32
-- In each row "x , y", x/y is a good rational approximation of log 2  / log k.
-- For the powers of 2, it is exact, otherwise x/y < log 2/log k, since we don't
-- want to overestimate integerLogBase b n = floor $ (log 2/log b)*logBase 2 n.
logArr :: UArray Int Int
logArr = listArray (0, 61)
          [ 1 , 1,
            190537 , 301994,
            1 , 2,
            1936274 , 4495889,
            190537 , 492531,
            91313 , 256348,
            1 , 3,
            190537 , 603988,
            1936274 , 6432163,
            1686227 , 5833387,
            190537 , 683068,
            5458 , 20197,
            91313 , 347661,
            416263 , 1626294,
            1 , 4,
            32631 , 133378,
            190537 , 794525,
            163451 , 694328,
            1936274 , 8368437,
            1454590 , 6389021,
            1686227 , 7519614,
            785355 , 3552602,
            190537 , 873605,
            968137 , 4495889,
            5458 , 25655,
            190537 , 905982,
            91313 , 438974,
            390321 , 1896172,
            416263 , 2042557,
            709397 , 3514492,
            1 , 5
          ]

-------------------------------------------------------------------------------
-- Unsafe
-------------------------------------------------------------------------------

#if CheckBounds
unsafeAt :: (IArray a e, Ix i) => a i e -> i -> e
unsafeAt = (!)
#endif

-------------------------------------------------------------------------------
-- Natural helpers
-------------------------------------------------------------------------------

fromNatural :: Num a => Natural -> a
fromNatural = fromIntegral

naturalLog2# :: Natural -> Int#
#ifdef MIN_VERSION_ghc_bignum
naturalLog2# n = word2Int# (BN.naturalLog2# n)
#else
#if MIN_VERSION_base(4,8,0) && defined(MIN_VERSION_integer_gmp)
naturalLog2# (NatS# b) = wordLog2# b
naturalLog2# (NatJ# n) = integerLog2# (Jp# n)
#else
naturalLog2# n = integerLog2# (toInteger n)
#endif
#endif

#if __GLASGOW_HASKELL__ < 707
-- The times they are a-changing. The types of primops too :(
isTrue# :: Bool -> Bool
isTrue# = id
#endif
