{-# LANGUAGE DuplicateRecordFields #-}
module T24035 where
import T24035_aux (R1 (MkR1, ra), rb)

x :: R1 -> Bool
x (MkR1 { rb = x0 }) = x0

y :: R1 -> Int
y (MkR1 { ra = y0 }) = y0
