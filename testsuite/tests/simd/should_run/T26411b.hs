{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import GHC.Exts

data DoubleX32 = DoubleX32
  DoubleX2# DoubleX2# DoubleX2# DoubleX2#
  DoubleX2# DoubleX2# DoubleX2# DoubleX2#
  DoubleX2# DoubleX2# DoubleX2# DoubleX2#
  DoubleX2# DoubleX2# DoubleX2# DoubleX2#

doubleX32ToList :: DoubleX32 -> [Double]
doubleX32ToList (DoubleX32 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  = a v0 . a v1 . a v2 . a v3 . a v4 . a v5 . a v6 . a v7 . a v8 . a v9 . a v10 . a v11 . a v12 . a v13 . a v14 . a v15 $ []
  where
    a v xs = case unpackDoubleX2# v of
      (# x0, x1 #) -> D# x0 : D# x1 : xs
{-# INLINE doubleX32ToList #-}

doubleX32FromList :: [Double] -> DoubleX32
doubleX32FromList [D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7, D# x8, D# x9, D# x10, D# x11, D# x12, D# x13, D# x14, D# x15, D# x16, D# x17, D# x18, D# x19, D# x20, D# x21, D# x22, D# x23, D# x24, D# x25, D# x26, D# x27, D# x28, D# x29, D# x30, D# x31]
  = DoubleX32
  (packDoubleX2# (# x0, x1 #)) (packDoubleX2# (# x2, x3 #)) (packDoubleX2# (# x4, x5 #)) (packDoubleX2# (# x6, x7 #))
  (packDoubleX2# (# x8, x9 #)) (packDoubleX2# (# x10, x11 #)) (packDoubleX2# (# x12, x13 #)) (packDoubleX2# (# x14, x15 #))
  (packDoubleX2# (# x16, x17 #)) (packDoubleX2# (# x18, x19 #)) (packDoubleX2# (# x20, x21 #)) (packDoubleX2# (# x22, x23 #))
  (packDoubleX2# (# x24, x25 #)) (packDoubleX2# (# x26, x27 #)) (packDoubleX2# (# x28, x29 #)) (packDoubleX2# (# x30, x31 #))
{-# NOINLINE doubleX32FromList #-}

broadcastDoubleX32 :: Double -> DoubleX32
broadcastDoubleX32 (D# x)
  = let !v = broadcastDoubleX2# x
    in DoubleX32 v v v v v v v v v v v v v v v v
{-# INLINE broadcastDoubleX32 #-}

negateDoubleX32 :: DoubleX32 -> DoubleX32
negateDoubleX32 (DoubleX32 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  = DoubleX32
  (negateDoubleX2# v0) (negateDoubleX2# v1) (negateDoubleX2# v2) (negateDoubleX2# v3)
  (negateDoubleX2# v4) (negateDoubleX2# v5) (negateDoubleX2# v6) (negateDoubleX2# v7)
  (negateDoubleX2# v8) (negateDoubleX2# v9) (negateDoubleX2# v10) (negateDoubleX2# v11)
  (negateDoubleX2# v12) (negateDoubleX2# v13) (negateDoubleX2# v14) (negateDoubleX2# v15)
{-# NOINLINE negateDoubleX32 #-}

recipDoubleX2# :: DoubleX2# -> DoubleX2#
recipDoubleX2# v = divideDoubleX2# (broadcastDoubleX2# 1.0##) v
{-# NOINLINE recipDoubleX2# #-}

recipDoubleX32 :: DoubleX32 -> DoubleX32
recipDoubleX32 (DoubleX32 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  = DoubleX32
  (recipDoubleX2# v0) (recipDoubleX2# v1) (recipDoubleX2# v2) (recipDoubleX2# v3)
  (recipDoubleX2# v4) (recipDoubleX2# v5) (recipDoubleX2# v6) (recipDoubleX2# v7)
  (recipDoubleX2# v8) (recipDoubleX2# v9) (recipDoubleX2# v10) (recipDoubleX2# v11)
  (recipDoubleX2# v12) (recipDoubleX2# v13) (recipDoubleX2# v14) (recipDoubleX2# v15)
{-# NOINLINE recipDoubleX32 #-}

divideDoubleX32 :: DoubleX32 -> DoubleX32 -> DoubleX32
divideDoubleX32 (DoubleX32 u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15) (DoubleX32 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  = DoubleX32
  (divideDoubleX2# u0 v0) (divideDoubleX2# u1 v1) (divideDoubleX2# u2 v2) (divideDoubleX2# u3 v3)
  (divideDoubleX2# u4 v4) (divideDoubleX2# u5 v5) (divideDoubleX2# u6 v6) (divideDoubleX2# u7 v7)
  (divideDoubleX2# u8 v8) (divideDoubleX2# u9 v9) (divideDoubleX2# u10 v10) (divideDoubleX2# u11 v11)
  (divideDoubleX2# u12 v12) (divideDoubleX2# u13 v13) (divideDoubleX2# u14 v14) (divideDoubleX2# u15 v15)
{-# INLINE divideDoubleX32 #-}

main :: IO ()
main = do
  let a = doubleX32FromList [0..31]
      b = divideDoubleX32 (broadcastDoubleX32 1.0) a
  print $ doubleX32ToList b
  putStrLn $ if doubleX32ToList b == map recip [0..31] then "OK" else "Wrong"
