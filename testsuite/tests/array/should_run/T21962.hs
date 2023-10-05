{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.IO
import GHC.Exts

main :: IO ()
main = do
   IO $ \s0 -> case newArray# 0# () s0 of (# s1, arr #) -> (# s1, () #)
