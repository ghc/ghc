-- | This module currently asserts that we give functions that always return
-- the same constructor of a sum type the CPR property.
module T5075 where

-- Omission of the type signature is deliberate, otherwise we won't get a join
-- point (this is up to the desugarer, not sure why).
-- f :: (Ord a, Num a) => a -> Either a b
f x = case x < 10 of
  True  -> Left x
  False -> f (x*2)

-- Similarly a join point. Should WW nonetheless
g :: Int -> Int -> Maybe Int
g x y = go x
  where
    go x = case x < y of
      True  -> Just x
      False -> go (x*2)

-- Here, go is not a join point, but still should be WW'd for Just.
-- Unfortunately, CPR can't see that (+?) returns Just, so h won't get the CPR
-- property. It probably could by only considering the @Just@ case of the
-- inlined (+?).
h :: Int -> Maybe Int
h x = go x +? go (x+1)
  where
    Just x +? Just y = Just (x + y)
    _      +? _      = Nothing
    go z
      | z > 10    = Just (x + z)
      | otherwise = go (z*2)
