{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newArray# 5# () s0 of
        (# s1, marr #) ->
          case writeArray# marr 5# () s1 of
            s2 -> (# s2, () #)

