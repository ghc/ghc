{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -dno-typeable-binds #-}

module T21851_2 where

import T21851_2a

g :: forall a. (Ord a, Num a) => a -> (a,String)
g n | n < 10    = (0, f n True)
    | otherwise = g (n-2)
-- The specialised version of g leads to a specialised
-- call to (f @Int @Bool).  Then we want to fire f's RULE
-- and specialise 'wombat'

h = g (3::Int)

