{-# OPTIONS_GHC -fdicts-strict #-}
module T23398 where

type PairDict a = (Eq a, Show a)

foo :: PairDict a => a -> a -> String
foo x y | x==y      = show x
        | otherwise = show y

-- In worker/wrapper we'd like to unbox the pair
-- but not (Eq a) and (Show a)

bar :: (a ~ b, Show a) => Int -> a -> (b, String)
bar 0 x = (x, show x)
bar n x = bar (n-1) x
