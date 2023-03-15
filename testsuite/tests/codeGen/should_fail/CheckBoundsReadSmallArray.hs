{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newSmallArray# 5# () s0 of
        (# s1, marr #) -> readSmallArray# marr (-1#) s1

