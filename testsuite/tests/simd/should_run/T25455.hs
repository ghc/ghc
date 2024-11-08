{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main where

import GHC.Exts

unpackFloatX4 :: FloatX4# -> (Float, Float, Float, Float)
unpackFloatX4 v = case unpackFloatX4# v of
  (# a0, a1, a2, a3 #) -> (F# a0, F# a1, F# a2, F# a3)

main :: IO ()
main = do
    let v = packFloatX4# (# 0.0#, 1.0#, 2.0#, 3.0# #)
    print $ unpackFloatX4 v
