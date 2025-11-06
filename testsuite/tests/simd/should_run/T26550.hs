{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts

type D3# = (# Double#, DoubleX2# #)

unD# :: Double -> Double#
unD# (D# x) = x

mkD3# :: Double -> D3#
mkD3# x =
  (# unD# (x + 2)
   , packDoubleX2# (# unD# (x + 3), unD# (x + 4) #)
   #)
{-# NOINLINE mkD3# #-}

main :: IO ()
main = do
  let
    !(# _ten, eleven_twelve #) = mkD3# 8
    !(# eleven, twelve #) = unpackDoubleX2# eleven_twelve

  putStrLn $ unlines
    [ "eleven: " ++ show (D# eleven)
    , "twelve: " ++ show (D# twelve)
    ]
