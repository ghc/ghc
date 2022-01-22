{-# LANGUAGE NondecreasingIndentation  #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newByteArray# 4# s0 of
        (# s1, a_marr #) -> case newByteArray# 4# s1 of
          (# s2, b_marr #) -> case unsafeFreezeByteArray# a_marr s2 of
            (# s3, a_arr #) -> case unsafeFreezeByteArray# b_marr s2 of
              (# s4, b_arr #) -> case compareByteArrays# a_arr 0# b_arr 1# 4# of
                0# -> (# s4, () #)

