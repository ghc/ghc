{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newByteArray# 23# s0 of
        (# s1, marr #) ->
          case readInt64Array# marr 2# s1 of
            (# s2, _n #) -> (# s2, () #)

