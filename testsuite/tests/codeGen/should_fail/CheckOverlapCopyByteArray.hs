{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newByteArray# 7# s0 of
        (# s1, marr #) -> case unsafeFreezeByteArray# marr s1 of
          (# s2, arr #) -> (# copyByteArray# arr 3# marr 0# 4# s2, () #)

