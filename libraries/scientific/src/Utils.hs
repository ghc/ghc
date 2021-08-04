{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( roundTo
    , i2d
    , maxExpt
    , magnitude
    ) where

import GHC.Base (Int(I#), Char(C#), chr#, ord#, (+#))

import qualified Data.Primitive.Array as Primitive
import           Control.Monad.ST             (runST)

#if MIN_VERSION_base(4,5,0)
import           Data.Bits                    (unsafeShiftR)
#else
import           Data.Bits                    (shiftR)
#endif

roundTo :: Int -> [Int] -> (Int, [Int])
roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  base = 10

  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i# ))

----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

-- | The same limit as in GHC.Float.
maxExpt :: Int
maxExpt = 324

expts10 :: Primitive.Array Integer
expts10 = runST $ do
    ma <- Primitive.newArray maxExpt uninitialised
    Primitive.writeArray ma 0  1
    Primitive.writeArray ma 1 10
    let go !ix
          | ix == maxExpt = Primitive.unsafeFreezeArray ma
          | otherwise = do
              Primitive.writeArray ma  ix        xx
              Primitive.writeArray ma (ix+1) (10*xx)
              go (ix+2)
          where
            xx = x * x
            x  = Primitive.indexArray expts10 half
#if MIN_VERSION_base(4,5,0)
            !half = ix `unsafeShiftR` 1
#else
            !half = ix `shiftR` 1
#endif
    go 2

uninitialised :: error
uninitialised = error "Data.Scientific: uninitialised element"

-- | @magnitude e == 10 ^ e@
magnitude :: Num a => Int -> a
magnitude e | e < maxExpt = cachedPow10 e
            | otherwise   = cachedPow10 hi * 10 ^ (e - hi)
    where
      cachedPow10 = fromInteger . Primitive.indexArray expts10

      hi = maxExpt - 1
