{-# LANGUAGE DuplicateRecordFields #-}

module T24035b where
import T24035_aux (R1 (MkR1, ra, rb), R2(rb))

x :: R1 -> Bool
x (MkR1 { rb = x0 }) = x0

y :: R1 -> Int
y (MkR1 { ra = y0 }) = y0

-- Use R2 to avoid unused import warning for R2
useR2 :: R2 -> Int
useR2 _ = 42
