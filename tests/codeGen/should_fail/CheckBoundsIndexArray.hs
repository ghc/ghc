{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newArray# 4# () s0 of
        (# s1, marr #) ->
          case unsafeFreezeArray# marr s1 of
            (# s2, arr #) ->
              case indexArray# arr 5# of
                (# () #) -> (# s2, () #)

