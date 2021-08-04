-- |
-- Module      : Crypto.Number.Compat
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE UnboxedTuples #-}
module Crypto.Number.Compat
    ( GmpSupported(..)
    , onGmpUnsupported
    , gmpGcde
    , gmpLog2
    , gmpPowModSecInteger
    , gmpPowModInteger
    , gmpInverse
    , gmpNextPrime
    , gmpTestPrimeMillerRabin
    , gmpSizeInBytes
    , gmpSizeInBits
    , gmpExportInteger
    , gmpExportIntegerLE
    , gmpImportInteger
    , gmpImportIntegerLE
    ) where

#ifndef MIN_VERSION_integer_gmp
#define MIN_VERSION_integer_gmp(a,b,c) 0
#endif

#if MIN_VERSION_integer_gmp(0,5,1)
import GHC.Integer.GMP.Internals
import GHC.Base
import GHC.Integer.Logarithms (integerLog2#)
#endif
import Data.Word
import GHC.Ptr (Ptr(..))

-- | GMP Supported / Unsupported
data GmpSupported a = GmpSupported a
                    | GmpUnsupported
                    deriving (Show,Eq)

-- | Simple combinator in case the operation is not supported through GMP
onGmpUnsupported :: GmpSupported a -> a -> a
onGmpUnsupported (GmpSupported a) _ = a
onGmpUnsupported GmpUnsupported   f = f

-- | Compute the GCDE of a two integer through GMP
gmpGcde :: Integer -> Integer -> GmpSupported (Integer, Integer, Integer)
#if MIN_VERSION_integer_gmp(0,5,1)
gmpGcde a b =
    GmpSupported (s, t, g)
  where (# g, s #) = gcdExtInteger a b
        t = (g - s * a) `div` b
#else
gmpGcde _ _ = GmpUnsupported
#endif

-- | Compute the binary logarithm of an integer through GMP
gmpLog2 :: Integer -> GmpSupported Int
#if MIN_VERSION_integer_gmp(0,5,1)
gmpLog2 0 = GmpSupported 0
gmpLog2 x = GmpSupported (I# (integerLog2# x))
#else
gmpLog2 _ = GmpUnsupported
#endif

-- | Compute the power modulus using extra security to remain constant
-- time wise through GMP
gmpPowModSecInteger :: Integer -> Integer -> Integer -> GmpSupported Integer
#if MIN_VERSION_integer_gmp(1,1,0)
gmpPowModSecInteger _ _ _ = GmpUnsupported
#elif MIN_VERSION_integer_gmp(1,0,2)
gmpPowModSecInteger b e m = GmpSupported (powModSecInteger b e m)
#elif MIN_VERSION_integer_gmp(1,0,0)
gmpPowModSecInteger _ _ _ = GmpUnsupported
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpPowModSecInteger b e m = GmpSupported (powModSecInteger b e m)
#else
gmpPowModSecInteger _ _ _ = GmpUnsupported
#endif

-- | Compute the power modulus through GMP
gmpPowModInteger :: Integer -> Integer -> Integer -> GmpSupported Integer
#if MIN_VERSION_integer_gmp(0,5,1)
gmpPowModInteger b e m = GmpSupported (powModInteger b e m)
#else
gmpPowModInteger _ _ _ = GmpUnsupported
#endif

-- | Inverse modulus of a number through GMP
gmpInverse :: Integer -> Integer -> GmpSupported (Maybe Integer)
#if MIN_VERSION_integer_gmp(0,5,1)
gmpInverse g m
    | r == 0    = GmpSupported Nothing
    | otherwise = GmpSupported (Just r)
  where r = recipModInteger g m
#else
gmpInverse _ _ = GmpUnsupported
#endif

-- | Get the next prime from a specific value through GMP
gmpNextPrime :: Integer -> GmpSupported Integer
#if MIN_VERSION_integer_gmp(1,1,0)
gmpNextPrime _ = GmpUnsupported
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpNextPrime n = GmpSupported (nextPrimeInteger n)
#else
gmpNextPrime _ = GmpUnsupported
#endif

-- | Test if a number is prime using Miller Rabin
gmpTestPrimeMillerRabin :: Int -> Integer -> GmpSupported Bool
#if MIN_VERSION_integer_gmp(1,1,0)
gmpTestPrimeMillerRabin _ _ = GmpUnsupported
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpTestPrimeMillerRabin (I# tries) !n = GmpSupported $
    case testPrimeInteger n tries of
        0# -> False
        _  -> True
#else
gmpTestPrimeMillerRabin _ _ = GmpUnsupported
#endif

-- | Return the size in bytes of an integer
gmpSizeInBytes :: Integer -> GmpSupported Int
#if MIN_VERSION_integer_gmp(0,5,1)
gmpSizeInBytes n = GmpSupported (I# (word2Int# (sizeInBaseInteger n 256#)))
#else
gmpSizeInBytes _ = GmpUnsupported
#endif

-- | Return the size in bits of an integer
gmpSizeInBits :: Integer -> GmpSupported Int
#if MIN_VERSION_integer_gmp(0,5,1)
gmpSizeInBits n = GmpSupported (I# (word2Int# (sizeInBaseInteger n 2#)))
#else
gmpSizeInBits _ = GmpUnsupported
#endif

-- | Export an integer to a memory (big-endian)
gmpExportInteger :: Integer -> Ptr Word8 -> GmpSupported (IO ())
#if MIN_VERSION_integer_gmp(1,0,0)
gmpExportInteger n (Ptr addr) = GmpSupported $ do
    _ <- exportIntegerToAddr n addr 1#
    return ()
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpExportInteger n (Ptr addr) = GmpSupported $ IO $ \s ->
    case exportIntegerToAddr n addr 1# s of
        (# s2, _ #) -> (# s2, () #)
#else
gmpExportInteger _ _ = GmpUnsupported
#endif

-- | Export an integer to a memory (little-endian)
gmpExportIntegerLE :: Integer -> Ptr Word8 -> GmpSupported (IO ())
#if MIN_VERSION_integer_gmp(1,0,0)
gmpExportIntegerLE n (Ptr addr) = GmpSupported $ do
    _ <- exportIntegerToAddr n addr 0#
    return ()
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpExportIntegerLE n (Ptr addr) = GmpSupported $ IO $ \s ->
    case exportIntegerToAddr n addr 0# s of
        (# s2, _ #) -> (# s2, () #)
#else
gmpExportIntegerLE _ _ = GmpUnsupported
#endif

-- | Import an integer from a memory (big-endian)
gmpImportInteger :: Int -> Ptr Word8 -> GmpSupported (IO Integer)
#if MIN_VERSION_integer_gmp(1,0,0)
gmpImportInteger (I# n) (Ptr addr) = GmpSupported $
    importIntegerFromAddr addr (int2Word# n) 1#
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpImportInteger (I# n) (Ptr addr) = GmpSupported $ IO $ \s ->
    importIntegerFromAddr addr (int2Word# n) 1# s
#else
gmpImportInteger _ _ = GmpUnsupported
#endif

-- | Import an integer from a memory (little-endian)
gmpImportIntegerLE :: Int -> Ptr Word8 -> GmpSupported (IO Integer)
#if MIN_VERSION_integer_gmp(1,0,0)
gmpImportIntegerLE (I# n) (Ptr addr) = GmpSupported $
    importIntegerFromAddr addr (int2Word# n) 0#
#elif MIN_VERSION_integer_gmp(0,5,1)
gmpImportIntegerLE (I# n) (Ptr addr) = GmpSupported $ IO $ \s ->
    importIntegerFromAddr addr (int2Word# n) 0# s
#else
gmpImportIntegerLE _ _ = GmpUnsupported
#endif
