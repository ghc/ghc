{-# OPTIONS_GHC -O #-}

module T19766 where

data T a b = T !a !b

data HasT = A (T Int Int) | B (T Int Int)

getT :: HasT -> T Int Int
getT (A t) = t
getT (B t) = t

f :: HasT -> [Int]
f ht = case getT ht of t@(T _ _) -> reverse $ reverse $ reverse $ reverse $ reverse $ reverse $ lookupGRE t 15 [1,2,3,4]
{-# NOINLINE f #-}

lookupGRE :: T Int a -> Int -> [Int] -> [Int]
lookupGRE ~(T n _) !k xs = [ x | x <- xs, x+k == n ]
{-# NOINLINE lookupGRE #-}
