{-# LANGUAGE Arrows #-}
-- Failed in ghci

module T where

import Prelude
import Control.Arrow

mapAC :: Arrow arr => Int -> arr (env, (b,())) c -> arr (env, ([b],())) [c]  
mapAC n farr = go 0
  where
    go i | i == n = arr (\(_env, ([], ())) -> [])
         | otherwise = proc ~(env, (b : bs, ())) ->
             do c  <- farr -< (env, (b, ()))
                cs <- go (i+1) -< (env, (bs, ()))
                returnA -< c : cs

t :: Arrow arr => arr [a] [a]
t = proc ys ->
     (| (mapAC 3) (\y -> returnA -< y) |) ys
