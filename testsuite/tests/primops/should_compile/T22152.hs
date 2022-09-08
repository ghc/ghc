{-# OPTIONS_GHC -O2 -ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques #-}
module T22152 (toHours) where

{-# INLINE toHoursMinutesSeconds #-}
toHoursMinutesSeconds :: Int -> (Int, Int, Int)
toHoursMinutesSeconds t = (h, m', s)
  where
    (h, m') = m `quotRem` 60
    (m, s) = toMinutesSeconds t

toMinutesSeconds :: Int -> (Int, Int)
toMinutesSeconds t = t `quotRem` 60

toHours t = h
  where
    (h, _, _) = toHoursMinutesSeconds t
