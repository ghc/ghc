{-# LANGUAGE MagicHash #-}
module T20333 (foo, bar) where

import GHC.Exts

-- Opaque, so that bar's f calls it with y boxed and evaluates no argument.
{-# OPAQUE g #-}
g :: Double -> Double
g y = y * 2

-- #20333's example, in Double#s so that their stores show in the assembly:
-- f's closure holds x1 to x4, which f loads where it starts, and f uses them
-- all after the evaluation of y.
foo :: Double# -> Double# -> Double# -> Double# -> [Double] -> [Double]
foo x1 x2 x3 x4 list = map f list
  where
    f (D# y) = D# (y +## x1 +## x2 +## x3 +## x4)

-- As foo, but f uses them after a call to g.
bar :: Double# -> Double# -> Double# -> Double# -> [Double] -> [Double]
bar x1 x2 x3 x4 list = map f list
  where
    f y = case g y of D# z -> D# (z +## x1 +## x2 +## x3 +## x4)
