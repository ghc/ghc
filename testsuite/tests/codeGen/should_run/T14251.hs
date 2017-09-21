{-# LANGUAGE MagicHash, BangPatterns #-}
module Main where

-- A minor modification from T8064.hs.
--
-- The key here is that we ensure that
-- subsequently passed floats do not
-- accidentally end up in previous
-- registers.
--

import GHC.Exts

{-# NOINLINE f #-}
f :: (Int# -> Float# -> Double# -> Float# -> Double# -> String) -> String
f g = g 3# 4.0# 5.0## 6.0# 6.9## ++ " World!"

{-# NOINLINE q #-}
q :: Int# -> Float# -> Double# -> Float# -> Double# -> String
q i j k l m = "Hello " ++ show (F# l) ++ " " ++ show (D# m)

main = putStrLn (f $ q)
