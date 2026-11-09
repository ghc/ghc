{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- T27638 with nineteen Doubles live at the join, so that no vector register
-- is free there: the join moves x from one stack slot to the other through
-- a free general-purpose register (#27638).
module Main where

import GHC.Exts
import Foreign.Marshal.Array (newArray)
import GHC.Ptr (Ptr (..))

foreign import ccall unsafe "fabs" c_fabs :: Double# -> Double#

f :: Int# -> Addr# -> Addr# -> Double# -> Double#
f b p q x =
  let !y = c_fabs x
      !(# r, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16 #) = case b of
             0# -> case unpackDoubleX2# (broadcastDoubleX2# x) of
                     (# u, _ #) -> let !z = c_fabs u in (# z, indexDoubleOffAddr# p 0#, indexDoubleOffAddr# p 1#, indexDoubleOffAddr# p 2#, indexDoubleOffAddr# p 3#, indexDoubleOffAddr# p 4#, indexDoubleOffAddr# p 5#, indexDoubleOffAddr# p 6#, indexDoubleOffAddr# p 7#, indexDoubleOffAddr# p 8#, indexDoubleOffAddr# p 9#, indexDoubleOffAddr# p 10#, indexDoubleOffAddr# p 11#, indexDoubleOffAddr# p 12#, indexDoubleOffAddr# p 13#, indexDoubleOffAddr# p 14#, indexDoubleOffAddr# p 15#, indexDoubleOffAddr# p 16# #)
             _  -> (# y, indexDoubleOffAddr# p 0#, indexDoubleOffAddr# p 1#, indexDoubleOffAddr# p 2#, indexDoubleOffAddr# p 3#, indexDoubleOffAddr# p 4#, indexDoubleOffAddr# p 5#, indexDoubleOffAddr# p 6#, indexDoubleOffAddr# p 7#, indexDoubleOffAddr# p 8#, indexDoubleOffAddr# p 9#, indexDoubleOffAddr# p 10#, indexDoubleOffAddr# p 11#, indexDoubleOffAddr# p 12#, indexDoubleOffAddr# p 13#, indexDoubleOffAddr# p 14#, indexDoubleOffAddr# p 15#, indexDoubleOffAddr# p 16# #)
  in (r *## x) +## (d0 +## d1 +## d2 +## d3 +## d4 +## d5 +## d6 +## d7 +## d8 +## d9 +## d10 +## d11 +## d12 +## d13 +## d14 +## d15 +## d16) +## int2Double# (0#)
{-# NOINLINE f #-}

main :: IO ()
main = do
  Ptr p <- newArray [fromIntegral k + 0.25 :: Double | k <- [1 .. 17 :: Int]]
  Ptr q <- newArray [k * 3 :: Int | k <- [1 .. 1]]
  let ref b x = abs x * x + sum [fromIntegral k + 0.25 | k <- [1 .. 17 :: Int]] + fromIntegral (sum [k * 3 | k <- [1 .. 0 :: Int]])
  print [D# (f 0# p q (negateDouble# 1.5##)), D# (f 1# p q (negateDouble# 2.5##))]
