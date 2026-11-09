{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- T27638 at Float#: the broadcast writes x's register at FloatX4#, so x is
-- spilled into a new, wider slot before the second call, and the join moves x
-- from one stack slot to the other through a free vector register (#27638).
module Main where

import GHC.Exts

foreign import ccall unsafe "fabsf" c_fabsf :: Float# -> Float#

f :: Int# -> Float# -> Float#
f b x =
  let !y = c_fabsf x
      !r = case b of
             0# -> case unpackFloatX4# (broadcastFloatX4# x) of
                     (# u, _, _, _ #) -> c_fabsf u
             _  -> y
  in r `timesFloat#` x
{-# NOINLINE f #-}

main :: IO ()
main = do
  print [F# (f 0# (negateFloat# 1.5#)), F# (f 1# (negateFloat# 2.5#))]
