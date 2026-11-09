{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- T27638c with thirteen Int#s live at the join beside the nineteen Doubles,
-- so that the join moves x from one stack slot to the other through
-- a general-purpose register other than the first free one (#27638).
module Main where

import GHC.Exts
import Foreign.Marshal.Array (newArray)
import GHC.Ptr (Ptr (..))

foreign import ccall unsafe "fabs" c_fabs :: Double# -> Double#

f :: Int# -> Addr# -> Addr# -> Double# -> Double#
f b p q x =
  let !y = c_fabs x
      !(# r, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12 #) = case b of
             0# -> case unpackDoubleX2# (broadcastDoubleX2# x) of
                     (# u, _ #) -> let !z = c_fabs u in (# z, indexDoubleOffAddr# p 0#, indexDoubleOffAddr# p 1#, indexDoubleOffAddr# p 2#, indexDoubleOffAddr# p 3#, indexDoubleOffAddr# p 4#, indexDoubleOffAddr# p 5#, indexDoubleOffAddr# p 6#, indexDoubleOffAddr# p 7#, indexDoubleOffAddr# p 8#, indexDoubleOffAddr# p 9#, indexDoubleOffAddr# p 10#, indexDoubleOffAddr# p 11#, indexDoubleOffAddr# p 12#, indexDoubleOffAddr# p 13#, indexDoubleOffAddr# p 14#, indexDoubleOffAddr# p 15#, indexDoubleOffAddr# p 16#, indexIntOffAddr# q 0#, indexIntOffAddr# q 1#, indexIntOffAddr# q 2#, indexIntOffAddr# q 3#, indexIntOffAddr# q 4#, indexIntOffAddr# q 5#, indexIntOffAddr# q 6#, indexIntOffAddr# q 7#, indexIntOffAddr# q 8#, indexIntOffAddr# q 9#, indexIntOffAddr# q 10#, indexIntOffAddr# q 11#, indexIntOffAddr# q 12# #)
             _  -> (# y, indexDoubleOffAddr# p 0#, indexDoubleOffAddr# p 1#, indexDoubleOffAddr# p 2#, indexDoubleOffAddr# p 3#, indexDoubleOffAddr# p 4#, indexDoubleOffAddr# p 5#, indexDoubleOffAddr# p 6#, indexDoubleOffAddr# p 7#, indexDoubleOffAddr# p 8#, indexDoubleOffAddr# p 9#, indexDoubleOffAddr# p 10#, indexDoubleOffAddr# p 11#, indexDoubleOffAddr# p 12#, indexDoubleOffAddr# p 13#, indexDoubleOffAddr# p 14#, indexDoubleOffAddr# p 15#, indexDoubleOffAddr# p 16#, indexIntOffAddr# q 0#, indexIntOffAddr# q 1#, indexIntOffAddr# q 2#, indexIntOffAddr# q 3#, indexIntOffAddr# q 4#, indexIntOffAddr# q 5#, indexIntOffAddr# q 6#, indexIntOffAddr# q 7#, indexIntOffAddr# q 8#, indexIntOffAddr# q 9#, indexIntOffAddr# q 10#, indexIntOffAddr# q 11#, indexIntOffAddr# q 12# #)
  in (r *## x) +## (d0 +## d1 +## d2 +## d3 +## d4 +## d5 +## d6 +## d7 +## d8 +## d9 +## d10 +## d11 +## d12 +## d13 +## d14 +## d15 +## d16) +## int2Double# (i0 +# i1 +# i2 +# i3 +# i4 +# i5 +# i6 +# i7 +# i8 +# i9 +# i10 +# i11 +# i12)
{-# NOINLINE f #-}

main :: IO ()
main = do
  Ptr p <- newArray [fromIntegral k + 0.25 :: Double | k <- [1 .. 17 :: Int]]
  Ptr q <- newArray [k * 3 :: Int | k <- [1 .. 13]]
  let ref b x = abs x * x + sum [fromIntegral k + 0.25 | k <- [1 .. 17 :: Int]] + fromIntegral (sum [k * 3 | k <- [1 .. 13 :: Int]])
  print [D# (f 0# p q (negateDouble# 1.5##)), D# (f 1# p q (negateDouble# 2.5##))]
