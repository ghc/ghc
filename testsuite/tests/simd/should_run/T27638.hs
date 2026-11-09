{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- The broadcast writes x's register at DoubleX2#, so x is spilled into a new,
-- wider slot before the second call, while on the other path to the join
-- it stays in the slot of its first spill: the join moves x from one stack
-- slot to the other (#27638).
module Main where

import GHC.Exts

foreign import ccall unsafe "fabs" c_fabs :: Double# -> Double#

f :: Int# -> Double# -> Double#
f b x =
  let !y = c_fabs x
      !r = case b of
             0# -> case unpackDoubleX2# (broadcastDoubleX2# x) of
                     (# u, _ #) -> c_fabs u
             _  -> y
  in r *## x
{-# NOINLINE f #-}

main :: IO ()
main = do
  print (D# (f 0# (negateDouble# 1.5##)))
  print (D# (f 1# (negateDouble# 2.5##)))
