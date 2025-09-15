{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newByteArray# 7# s0 of
        (# s1, marr #) ->
          case readWord8ArrayAsWord32# marr (-3#) s1 of
            -- only the last byte of the desired word32 is in bounds
            (# s2, _n #) -> (# s2, () #)

