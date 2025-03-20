{-# LANGUAGE UnboxedTuples, MagicHash #-}

module Main where

import GHC.Exts

tuple6 :: () -> (# Float#, Double#, Float#, Double#, Float#, Double# #)
tuple6 _ = (# 1.0#, 2.0##, 3.0#, 4.0##, 5.0#, 6.0## #)
{-# NOINLINE tuple6 #-}

main :: IO ()
main =
  case tuple6 () of
    (# _, _, _, _, _, d' #) ->
      print (D# d')
