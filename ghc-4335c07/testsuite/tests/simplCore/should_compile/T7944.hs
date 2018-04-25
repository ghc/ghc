module T7944 where

import GHC.Exts

-- Force specialisation of "go"
data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

-- This is more or less just an ordinary fold
go :: SPEC -> [a] -> IntMap a -> IntMap a
go SPEC [] m = m
go SPEC (_:xs) m
 = go SPEC xs
 -- This would be the "worker function" of the fold
 $ Unary m


-- Both constructors are necessary, despite only one being used
data IntMap a = Nil | Unary (IntMap a)
