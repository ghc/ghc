{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

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

doubleX32FromList :: [Double] -> DoubleX32
doubleX32FromList [D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7, D# x8, D# x9, D# x10, D# x11, D# x12, D# x13, D# x14, D# x15, D# x16, D# x17, D# x18, D# x19, D# x20, D# x21, D# x22, D# x23, D# x24, D# x25, D# x26, D# x27, D# x28, D# x29, D# x30, D# x31]
  = DoubleX32
  (packDoubleX2# (# x0, x1 #)) (packDoubleX2# (# x2, x3 #)) (packDoubleX2# (# x4, x5 #)) (packDoubleX2# (# x6, x7 #))
  (packDoubleX2# (# x8, x9 #)) (packDoubleX2# (# x10, x11 #)) (packDoubleX2# (# x12, x13 #)) (packDoubleX2# (# x14, x15 #))
  (packDoubleX2# (# x16, x17 #)) (packDoubleX2# (# x18, x19 #)) (packDoubleX2# (# x20, x21 #)) (packDoubleX2# (# x22, x23 #))
  (packDoubleX2# (# x24, x25 #)) (packDoubleX2# (# x26, x27 #)) (packDoubleX2# (# x28, x29 #)) (packDoubleX2# (# x30, x31 #))

negateDoubleX32 :: DoubleX32 -> DoubleX32
negateDoubleX32 (DoubleX32 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  = DoubleX32
  (negateDoubleX2# v0) (negateDoubleX2# v1) (negateDoubleX2# v2) (negateDoubleX2# v3)
  (negateDoubleX2# v4) (negateDoubleX2# v5) (negateDoubleX2# v6) (negateDoubleX2# v7)
  (negateDoubleX2# v8) (negateDoubleX2# v9) (negateDoubleX2# v10) (negateDoubleX2# v11)
  (negateDoubleX2# v12) (negateDoubleX2# v13) (negateDoubleX2# v14) (negateDoubleX2# v15)

recipDoubleX2# :: DoubleX2# -> DoubleX2#
recipDoubleX2# v = divideDoubleX2# (broadcastDoubleX2# 1.0##) v
{-# INLINE recipDoubleX2# #-}

recipDoubleX32 :: DoubleX32 -> DoubleX32
recipDoubleX32 (DoubleX32 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  = DoubleX32
  (recipDoubleX2# v0) (recipDoubleX2# v1) (recipDoubleX2# v2) (recipDoubleX2# v3)
  (recipDoubleX2# v4) (recipDoubleX2# v5) (recipDoubleX2# v6) (recipDoubleX2# v7)
  (recipDoubleX2# v8) (recipDoubleX2# v9) (recipDoubleX2# v10) (recipDoubleX2# v11)
  (recipDoubleX2# v12) (recipDoubleX2# v13) (recipDoubleX2# v14) (recipDoubleX2# v15)

main :: IO ()
main = do
  let a = doubleX32FromList [0..31]
      b = negateDoubleX32 a
      c = recipDoubleX32 a
  print $ doubleX32ToList b
  putStrLn $ if doubleX32ToList b == map negate [0..31] then "OK" else "Wrong"
  print $ doubleX32ToList c
  putStrLn $ if doubleX32ToList c == map recip [0..31] then "OK" else "Wrong"
