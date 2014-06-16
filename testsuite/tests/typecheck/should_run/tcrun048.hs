{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main where

import GHC.Prim (Int#, Double#)

main :: IO ()
main = let f = int2Integer# 0# in putStrLn ""


{-# NOINLINE int2Integer# #-}
int2Integer# :: Int# -> (# Int#, Double# #)
int2Integer# x = (# x, 1.0## #)
