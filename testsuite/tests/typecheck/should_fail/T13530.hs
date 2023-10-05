{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T13530 where

import GHC.Exts

g :: Int -> (# Int#, a #)
g (I# y) = (# y, undefined #)

f :: Int -> (# Int#, Int# #)
f x = g x
