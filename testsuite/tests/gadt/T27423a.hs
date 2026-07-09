{-# LANGUAGE GADTs #-}
module T27423a where

data G a where
  MkG1 :: a -> G a
  MkG2 :: (a -> G a)
  MkG3 :: forall a. a -> G a
  MkG4 :: forall a. (a -> G a)

-- this is equivalent to `forall {b}. (forall a. a -> b -> T)`.
data T where
  MkT2 :: (forall a. a -> b -> T)

-- this is equivalent to `forall. (forall a. S a)`.
data S a where
  MkS :: (forall a. S a)

-- A forall and a context combined inside the same parentheses, with no
-- outer forall at all.
data Y a where
  MkY :: (forall a. Show a => a -> Y a)

-- Multiple, redundant nested parentheses around the whole type should
-- be accepted.
data H a where
  MkH :: ((a -> H a))

-- An unparenthesised outer forall together with an independent,
-- parenthesised inner forall.
data I a where
  MkI :: forall a. (forall b. I (b, a))
