{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.Base

f :: Int -> Int# ->  Int#
f x i = i

test = let !(I# i) = 3 in I# (f 2 i)