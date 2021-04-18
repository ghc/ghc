module OpaqueNoSpecConstr where

import GHC.Exts ( SpecConstrAnnotation(..) )

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

-- Would normally induce a SpecConstr on the constructors of SPEC
loop :: SPEC -> [Int] -> [Int] -> [Int]
loop SPEC z [] = z
loop SPEC z (x:xs) = loop SPEC (x:z) xs
{-# OPAQUE loop #-}
