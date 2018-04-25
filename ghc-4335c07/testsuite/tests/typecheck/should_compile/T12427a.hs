{-# LANGUAGE GADTs, RankNTypes #-}

-- Test pattern bindings, existentials, and higher rank

module T12427a where

data T where
  T1 :: a -> ((forall b. [b]->[b]) -> Int) -> T
  T2 :: ((forall b. [b]->[b]) -> Int) -> T

-- Inference
-- Worked in 7.10 (probably wrongly)
-- Failed in 8.0.1
-- Fails in 8.2 because v is polymorphic
--     and the T1 pattern match binds existentials,
--     and hence bumps levels
h11 y = case y of T1 _ v -> v

-- Worked in 7.10 (probably wrongly)
-- Failed in 8.0.1
-- Succeeds in 8.2 because the pattern match has
--   no existentials, so it doesn't matter than
--   v is polymorphic
h12 y = case y of T2 v -> v

-- Inference
-- Same results as for h11 and h12 resp
T1 _ x1 = undefined
T2 x2 = undefined

-- Works in all versions
h2 :: T -> (forall b. [b] -> [b]) -> Int
h2 y = case y of T1 _ v -> v

-- Checking
-- Fails in 7.10  (head exploded)
-- Fails in 8.0.1 (ditto)
-- Succeeds in 8.2
x3 :: (forall a. a->a) -> Int
T1 _ x3 = undefined
