{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- T27638 with x used again in the other arm, so that the join's target
-- expects x both in a register and in a stack slot: the join moves x from one
-- stack slot to the other through a free vector register (#27638).
module Main where

import GHC.Exts

foreign import ccall unsafe "fabs" c_fabs :: Double# -> Double#

f :: Int# -> Double# -> Double#
f b x =
  let !y = c_fabs x
      !r = case b of
             0# -> case unpackDoubleX2# (broadcastDoubleX2# x) of
                     (# u, _ #) -> c_fabs u
             _  -> y +## x
  in r *## x
{-# NOINLINE f #-}

main :: IO ()
main = do
  print [D# (f 0# (negateDouble# 1.5##)), D# (f 1# (negateDouble# 2.5##))]
