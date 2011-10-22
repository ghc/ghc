{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module T5573b where

import GHC.Exts

foo :: Double# -> (# (# Double#, Double# #), Double# #)
foo x = (# (# x, x #), x #)

