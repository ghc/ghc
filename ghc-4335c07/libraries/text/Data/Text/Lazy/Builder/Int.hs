{-# LANGUAGE BangPatterns, CPP, MagicHash, RankNTypes, ScopedTypeVariables,
    UnboxedTuples #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- Module:      Data.Text.Lazy.Builder.Int
-- Copyright:   (c) 2013 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD-style
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently write an integral value to a 'Builder'.

module Data.Text.Lazy.Builder.Int
    (
      decimal
    , hexadecimal
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mempty)
import qualified Data.ByteString.Unsafe as B
import Data.Text.Internal.Builder.Functions ((<>), i2d)
import Data.Text.Internal.Builder
import Data.Text.Internal.Builder.Int.Digits (digits)
import Data.Text.Array
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Base (quotInt, remInt)
import GHC.Num (quotRemInteger)
import GHC.Types (Int(..))
import Control.Monad.ST
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

#ifdef  __GLASGOW_HASKELL__
# if defined(INTEGER_GMP)
import GHC.Integer.GMP.Internals (Integer(S#))
# elif defined(INTEGER_SIMPLE)
import GHC.Integer
# else
# error "You need to use either GMP or integer-simple."
# endif
#endif

#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

decimal :: Integral a => a -> Builder
{-# RULES "decimal/Int8" decimal = boundedDecimal :: Int8 -> Builder #-}
{-# RULES "decimal/Int" decimal = boundedDecimal :: Int -> Builder #-}
{-# RULES "decimal/Int16" decimal = boundedDecimal :: Int16 -> Builder #-}
{-# RULES "decimal/Int32" decimal = boundedDecimal :: Int32 -> Builder #-}
{-# RULES "decimal/Int64" decimal = boundedDecimal :: Int64 -> Builder #-}
{-# RULES "decimal/Word" decimal = positive :: Data.Word.Word -> Builder #-}
{-# RULES "decimal/Word8" decimal = positive :: Word8 -> Builder #-}
{-# RULES "decimal/Word16" decimal = positive :: Word16 -> Builder #-}
{-# RULES "decimal/Word32" decimal = positive :: Word32 -> Builder #-}
{-# RULES "decimal/Word64" decimal = positive :: Word64 -> Builder #-}
{-# RULES "decimal/Integer" decimal = integer 10 :: Integer -> Builder #-}
decimal i = decimal' (<= -128) i
{-# NOINLINE decimal #-}

boundedDecimal :: (Integral a, Bounded a) => a -> Builder
{-# SPECIALIZE boundedDecimal :: Int -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int8 -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int16 -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int32 -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int64 -> Builder #-}
boundedDecimal i = decimal' (== minBound) i

decimal' :: (Integral a) => (a -> Bool) -> a -> Builder
{-# INLINE decimal' #-}
decimal' p i
    | i < 0 = if p i
              then let (q, r) = i `quotRem` 10
                       qq = -q
                       !n = countDigits qq
                   in writeN (n + 2) $ \marr off -> do
                       unsafeWrite marr off minus
                       posDecimal marr (off+1) n qq
                       unsafeWrite marr (off+n+1) (i2w (-r))
              else let j = -i
                       !n = countDigits j
                   in writeN (n + 1) $ \marr off ->
                       unsafeWrite marr off minus >> posDecimal marr (off+1) n j
    | otherwise = positive i

positive :: (Integral a) => a -> Builder
{-# SPECIALIZE positive :: Int -> Builder #-}
{-# SPECIALIZE positive :: Int8 -> Builder #-}
{-# SPECIALIZE positive :: Int16 -> Builder #-}
{-# SPECIALIZE positive :: Int32 -> Builder #-}
{-# SPECIALIZE positive :: Int64 -> Builder #-}
{-# SPECIALIZE positive :: Word -> Builder #-}
{-# SPECIALIZE positive :: Word8 -> Builder #-}
{-# SPECIALIZE positive :: Word16 -> Builder #-}
{-# SPECIALIZE positive :: Word32 -> Builder #-}
{-# SPECIALIZE positive :: Word64 -> Builder #-}
positive i
    | i < 10    = writeN 1 $ \marr off -> unsafeWrite marr off (i2w i)
    | otherwise = let !n = countDigits i
                  in writeN n $ \marr off -> posDecimal marr off n i

posDecimal :: (Integral a) =>
              forall s. MArray s -> Int -> Int -> a -> ST s ()
{-# INLINE posDecimal #-}
posDecimal marr off0 ds v0 = go (off0 + ds - 1) v0
  where go off v
           | v >= 100 = do
               let (q, r) = v `quotRem` 100
               write2 off r
               go (off - 2) q
           | v < 10    = unsafeWrite marr off (i2w v)
           | otherwise = write2 off v
        write2 off i0 = do
          let i = fromIntegral i0; j = i + i
          unsafeWrite marr off $ get (j + 1)
          unsafeWrite marr (off - 1) $ get j
        get = fromIntegral . B.unsafeIndex digits

minus, zero :: Word16
{-# INLINE minus #-}
{-# INLINE zero #-}
minus = 45
zero = 48

i2w :: (Integral a) => a -> Word16
{-# INLINE i2w #-}
i2w v = zero + fromIntegral v

countDigits :: (Integral a) => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

hexadecimal :: Integral a => a -> Builder
{-# SPECIALIZE hexadecimal :: Int -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int8 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int16 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int32 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int64 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word8 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word16 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word32 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word64 -> Builder #-}
{-# RULES "hexadecimal/Integer"
    hexadecimal = hexInteger :: Integer -> Builder #-}
hexadecimal i
    | i < 0     = error hexErrMsg
    | otherwise = go i
  where
    go n | n < 16    = hexDigit n
         | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)
{-# NOINLINE[0] hexadecimal #-}

hexInteger :: Integer -> Builder
hexInteger i
    | i < 0     = error hexErrMsg
    | otherwise = integer 16 i

hexErrMsg :: String
hexErrMsg = "Data.Text.Lazy.Builder.Int.hexadecimal: applied to negative number"

hexDigit :: Integral a => a -> Builder
hexDigit n
    | n <= 9    = singleton $! i2d (fromIntegral n)
    | otherwise = singleton $! toEnum (fromIntegral n + 87)
{-# INLINE hexDigit #-}

data T = T !Integer !Int

integer :: Int -> Integer -> Builder
#ifdef INTEGER_GMP
integer 10 (S# i#) = decimal (I# i#)
integer 16 (S# i#) = hexadecimal (I# i#)
#endif
integer base i
    | i < 0     = singleton '-' <> go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

    T maxInt10 maxDigits10 =
        until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
      where mi = fromIntegral (maxBound :: Int)
    T maxInt16 maxDigits16 =
        until ((>mi) . (*16) . fstT) (\(T n d) -> T (n*16) (d+1)) (T 16 1)
      where mi = fromIntegral (maxBound :: Int)

    fstT (T a _) = a

    maxInt | base == 10 = maxInt10
           | otherwise  = maxInt16
    maxDigits | base == 10 = maxDigits10
              | otherwise  = maxDigits16

    putH (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y)
                        | q > 0     -> int q <> pblock r <> putB ns
                        | otherwise -> int r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putH _ = error "putH: the impossible happened"

    putB (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y) -> pblock q <> pblock r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putB _ = Data.Monoid.mempty

    int :: Int -> Builder
    int x | base == 10 = decimal x
          | otherwise  = hexadecimal x

    pblock = loop maxDigits
      where
        loop !d !n
            | d == 1    = hexDigit n
            | otherwise = loop (d-1) q <> hexDigit r
            where q = n `quotInt` base
                  r = n `remInt` base
