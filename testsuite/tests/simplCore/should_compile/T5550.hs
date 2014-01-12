module T5550 where

import GHC.Exts ( SpecConstrAnnotation(..) )

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

loop :: SPEC -> [Int] -> [Int] -> [Int]
loop SPEC z [] = z
loop SPEC z (x:xs) = loop SPEC (x:z) xs

