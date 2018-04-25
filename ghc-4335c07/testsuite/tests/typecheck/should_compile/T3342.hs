{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module T3342 where

data F = FT String [F]
data G = GX F F | GY

spec :: F -> G
spec (FT "X"  [t1, t2]) = GX t1 t2
spec _                  = GY

-- walk :: F -> F
walk (spec -> GX _ t2) = walk t2
walk t@(FT _ _)        = t
