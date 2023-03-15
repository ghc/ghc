{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newPinnedByteArray# 7# s0 of
        (# s1, marr #) -> case mutableByteArrayContents# marr of
          ptr -> (# copyAddrToByteArray# ptr marr 3# 4# s1, () #)
