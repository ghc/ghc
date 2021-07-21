{-# LANGUAGE BangPatterns, CPP, MagicHash, RankNTypes, ScopedTypeVariables, UnboxedTuples #-}
{-# OPTIONS_GHC -O2 -dcmm-lint #-}

module Data.Text.Lazy.Builder.Int ( hexadecimal) where

import GHC.Exts
import Data.Int (Int8)
import Prelude

type Builder = [Char]

singleton :: a -> [a]
singleton x = [x]

hexadecimal :: Int8 -> Builder
hexadecimal i = go i
  where
    go n | n < 16    = hexDigit n
         | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)
{-# NOINLINE[0] hexadecimal #-}

hexDigit :: Integral a => a -> Builder
hexDigit n
    | n <= 9    = singleton $! i2d (fromIntegral n)
    | otherwise = singleton $! toEnum (fromIntegral n + 87)
{-# INLINE hexDigit #-}

{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))
