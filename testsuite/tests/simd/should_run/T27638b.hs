{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- T27638 with x used again in the other arm, which comes first, the broadcast
-- being the default: the join's target, as that arm leaves it, expects x both
-- in a register and in the slot of its first spill, and the jump from the
-- broadcasting arm loads x from the new, wider slot into that register and
-- stores it from there into the other (#27638).
module Main where

import GHC.Exts

foreign import ccall unsafe "fabs" c_fabs :: Double# -> Double#

f :: Int# -> Double# -> Double#
f b x =
  let !y = c_fabs x
      !r = case b of
             1# -> y +## x
             _  -> case unpackDoubleX2# (broadcastDoubleX2# x) of
                     (# u, _ #) -> c_fabs u
  in r *## x
{-# NOINLINE f #-}

main :: IO ()
main = do
  print [D# (f 0# (negateDouble# 1.5##)), D# (f 1# (negateDouble# 2.5##))]
