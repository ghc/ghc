module Algo.Rootfix where

import Data.Vector.Unboxed as V

rootfix :: (V.Vector Int, V.Vector Int) -> V.Vector Int
{-# NOINLINE rootfix #-}
rootfix (ls, rs) = rootfix (V.replicate (V.length ls) 1) ls rs
    where
      rootfix xs ls rs
        = let zs   = V.replicate (V.length ls * 2) 0
              vs   = V.update_ (V.update_ zs ls xs) rs (V.map negate xs)
              sums = V.prescanl' (+) 0 vs
          in
          V.backpermute sums ls

