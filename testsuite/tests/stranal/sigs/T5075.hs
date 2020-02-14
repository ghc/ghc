-- | This module currently asserts that we trim CPR for local bindings
-- returning a sum. We can hopefully give @loop@ a CPR signature some day, but
-- we first have to fix #5075/#16570.
module T5075 where

-- Omission of the type signature is deliberate, otherwise we won't get a join
-- point (this is up to the desugarer, not sure why).
-- loop :: (Ord a, Num a) => a -> Either a b
loop x = case x < 10 of
  True -> Left x
  False -> loop (x*2)
