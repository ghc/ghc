{-# LANGUAGE BangPatterns, MagicHash #-}
module T8327a (sumTo) where

import GHC.Exts

-- At -O1, which runs no SpecConstr, the loop keeps its SPEC argument, and
-- the bang's case binder on it is unused.  Copying the argument into that
-- binder kept it live, so that the loop assigned SPEC_closure+1 to a register
-- at every iteration, which nothing read (#8327).
sumTo :: Int -> Int
sumTo (I# n) = I# (go SPEC 0# 0#)
  where go !_ acc i | isTrue# (i >=# n) = acc
                    | otherwise = go SPEC (acc +# i) (i +# 1#)
