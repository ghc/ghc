module GHC.Data.List where

import Data.Foldable

mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])
mapAndUnzip _ [] = ([], [])
mapAndUnzip f (x:xs)
  = let (r1,  r2)  = f x
        (rs1, rs2) = mapAndUnzip f xs
    in
    (r1:rs1, r2:rs2)

mapAndUnzip3 :: (a -> (b, c, d)) -> [a] -> ([b], [c], [d])
mapAndUnzip3 _ [] = ([], [], [])
mapAndUnzip3 f (x:xs)
  = let (r1,  r2,  r3)  = f x
        (rs1, rs2, rs3) = mapAndUnzip3 f xs
    in
    (r1:rs1, r2:rs2, r3:rs3)

mapAndUnzip4 :: (a -> (b, c, d, e)) -> [a] -> ([b], [c], [d], [e])
mapAndUnzip4 _ [] = ([], [], [], [])
mapAndUnzip4 f (x:xs)
  = let (r1,  r2,  r3, r4)  = f x
        (rs1, rs2, rs3, rs4) = mapAndUnzip4 f xs
    in
    (r1:rs1, r2:rs2, r3:rs3, r4:rs4)

-- | @'reverseAppend' xs ys@ is equivalent to @'reverse' xs '++' ys@,
-- except it's defined in terms of 'foldl'' and more efficient.
reverseAppend :: [a] -> [a] -> [a]
reverseAppend xs ys = foldl' (\acc x -> x : acc) ys xs
{-# INLINE reverseAppend #-}
