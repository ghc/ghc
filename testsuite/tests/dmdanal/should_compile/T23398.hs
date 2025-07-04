{-# OPTIONS_GHC -fdicts-strict #-}
module T23398 where

type PairDict a = (Eq a, Show a)

foo :: PairDict a => a -> a -> String
foo x y | x==y      = show x
        | otherwise = show y
-- In worker/wrapper we don't want to unbox PairDict
-- See (DNB1) Note [Do not unbox class dictionaries] in GHC.Core.Opt.DmdAnal

bar :: (a ~ b, Show a) => Int -> a -> (b, String)
bar 0 x = (x, show x)
bar n x = bar (n-1) x
-- ...but we do want to unbox the (a~b)
-- see (DNB2) in the same Note
