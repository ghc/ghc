{-# LANGUAGE MagicHash #-}
module T21763 where

import GHC.Exts

-- We should get ONE SpecConstr-generated rule, for f2,
-- not one for f1 and one for f2

f1 :: Int -> [Int] -> (Int, [Int])
-- This one only seq's x, so SpecConstr should not specialise it
f1 x []     = (x, x `seq` [])
f1 x (_:ys) = f1 x ys


f2 :: Int -> [Int] -> (Int, [Int])
-- This one takes x apart, so SpecConstr should specialise it
f2 x []     = (x+1, x `seq` [])
f2 x (_:ys) = f2 x ys

foo1 :: [Int] -> (Int, [Int])
foo1 ys = f1 9 ys

foo2 :: [Int] -> (Int, [Int])
foo2 ys = f2 9 ys
