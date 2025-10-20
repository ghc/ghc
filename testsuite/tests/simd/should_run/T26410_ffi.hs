{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
import GHC.Exts
import GHC.Int

foreign import ccall unsafe "minInt64X8"
  minInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#

data Int64X8 = Int64X8# Int64X8#

minInt64X8 :: Int64X8 -> Int64X8 -> Int64X8
minInt64X8 (Int64X8# a) (Int64X8# b) = Int64X8# (minInt64X8# a b)
{-# NOINLINE minInt64X8 #-}

broadcastInt64X8 :: Int64 -> Int64X8
broadcastInt64X8 (I64# x) = Int64X8# (broadcastInt64X8# x)

packInt64X8 :: (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64) -> Int64X8
packInt64X8 (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7) = Int64X8# (packInt64X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))

unpackInt64X8 :: Int64X8 -> (Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64)
unpackInt64X8 (Int64X8# a) = case unpackInt64X8# a of
  (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7)

-- You can check the assembly code for this function to see if ZMM registers are used
plusInt64X8 :: Int64X8 -> Int64X8 -> Int64X8
plusInt64X8 (Int64X8# a) (Int64X8# b) = Int64X8# (plusInt64X8# a b)

main :: IO ()
main = do
  let a = broadcastInt64X8 3
      b = packInt64X8 (1, 2, 3, 4, 5, 6, 7, 8)
      c = minInt64X8 a b
  print $ unpackInt64X8 c
  let d = packInt64X8 (-1, -2, -3, -4, -5, -6, -7, -8)
      e = broadcastInt64X8 (-3)
      f = minInt64X8 d e
  print $ unpackInt64X8 f
  print $ unpackInt64X8 (plusInt64X8 a b)
