module Algo.Leaffix where

import Data.Vector.Unboxed as V

leaffix :: (Vector Int, Vector Int) -> Vector Int
{-# NOINLINE leaffix #-}
leaffix (ls,rs)
    = leaffix (V.replicate (V.length ls) 1) ls rs
    where
      leaffix xs ls rs
        = let zs   = V.replicate (V.length ls * 2) 0
              vs   = V.update_ zs ls xs
              sums = V.prescanl' (+) 0 vs
          in
          V.zipWith (-) (V.backpermute sums ls) (V.backpermute sums rs)

