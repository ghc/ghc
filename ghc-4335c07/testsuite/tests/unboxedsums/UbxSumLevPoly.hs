{-# LANGUAGE UnboxedSums #-}

module UbxSumLevPoly where

-- this failed thinking that (# Any | True #) :: TYPE (SumRep [LiftedRep, b])
-- But of course that b should be Lifted!

-- It was due to silliness in TysWiredIn using the same uniques for different
-- things in mk_sum.

p = True
  where (# _x | #) = (# | True #)
