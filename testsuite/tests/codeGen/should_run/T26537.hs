{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts

type D8 = (# Double#, Double#, Double#, Double#, Double#, Double#, Double#, Double# #)
type D64 = (# D8, D8, D8, D8, D8, D8, D8, D8 #)
type D512 = (# D64, D64, D64, D64, D64, D64, D64, D64 #)

unD# :: Double -> Double#
unD# (D# x) = x

mkD8 :: Double -> D8
mkD8 x = (# unD# x, unD# (x + 1), unD# (x + 2), unD# (x + 3), unD# (x + 4), unD# (x + 5), unD# (x + 6), unD# (x + 7) #)
{-# NOINLINE mkD8 #-}

mkD64 :: Double -> D64
mkD64 x = (# mkD8 x, mkD8 (x + 8), mkD8 (x + 16), mkD8 (x + 24), mkD8 (x + 32), mkD8 (x + 40), mkD8 (x + 48), mkD8 (x + 56) #)
{-# NOINLINE mkD64 #-}

mkD512 :: Double -> D512
mkD512 x = (# mkD64 x, mkD64 (x + 64), mkD64 (x + 128), mkD64 (x + 192), mkD64 (x + 256), mkD64 (x + 320), mkD64 (x + 384), mkD64 (x + 448) #)
{-# NOINLINE mkD512 #-}

addD8 :: D8 -> D8 -> D8
addD8 (# x0, x1, x2, x3, x4, x5, x6, x7 #) (# y0, y1, y2, y3, y4, y5, y6, y7 #) = (# x0 +## y0, x1 +## y1, x2 +## y2, x3 +## y3, x4 +## y4, x5 +## y5, x6 +## y6, x7 +## y7 #)
{-# NOINLINE addD8 #-}

addD64 :: D64 -> D64 -> D64
addD64 (# x0, x1, x2, x3, x4, x5, x6, x7 #) (# y0, y1, y2, y3, y4, y5, y6, y7 #) = (# addD8 x0 y0, addD8 x1 y1, addD8 x2 y2, addD8 x3 y3, addD8 x4 y4, addD8 x5 y5, addD8 x6 y6, addD8 x7 y7 #)
{-# NOINLINE addD64 #-}

addD512 :: D512 -> D512 -> D512
addD512 (# x0, x1, x2, x3, x4, x5, x6, x7 #) (# y0, y1, y2, y3, y4, y5, y6, y7 #) = (# addD64 x0 y0, addD64 x1 y1, addD64 x2 y2, addD64 x3 y3, addD64 x4 y4, addD64 x5 y5, addD64 x6 y6, addD64 x7 y7 #)
{-# NOINLINE addD512 #-}

toListD8 :: D8 -> [Double]
toListD8 (# x0, x1, x2, x3, x4, x5, x6, x7 #) = [D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7]
{-# NOINLINE toListD8 #-}

toListD64 :: D64 -> [Double]
toListD64 (# x0, x1, x2, x3, x4, x5, x6, x7 #) = concat [toListD8 x0, toListD8 x1, toListD8 x2, toListD8 x3, toListD8 x4, toListD8 x5, toListD8 x6, toListD8 x7]
{-# NOINLINE toListD64 #-}

toListD512 :: D512 -> [Double]
toListD512 (# x0, x1, x2, x3, x4, x5, x6, x7 #) = concat [toListD64 x0, toListD64 x1, toListD64 x2, toListD64 x3, toListD64 x4, toListD64 x5, toListD64 x6, toListD64 x7]
{-# NOINLINE toListD512 #-}

data T = MkT D512 D64

mkT :: Double -> T
mkT x = MkT (mkD512 x) (mkD64 (x + 512))
{-# NOINLINE mkT #-}

addT :: T -> T -> T
addT (MkT x0 x1) (MkT y0 y1) = MkT (addD512 x0 y0) (addD64 x1 y1)
{-# NOINLINE addT #-}

toListT :: T -> [Double]
toListT (MkT x0 x1) = toListD512 x0 ++ toListD64 x1
{-# NOINLINE toListT #-}

main :: IO ()
main = do
  let n = 512 + 64
  let !x = mkT 0
      !y = mkT n
  print $ toListT x
  print $ toListT y
  print $ toListT (addT x y)
  print $ toListT x == [0..n-1]
  print $ toListT y == [n..2*n-1]
  print $ toListT (addT x y) == zipWith (+) [0..n-1] [n..2*n-1]
