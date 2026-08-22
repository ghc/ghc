-- The good case from Note [Reboxing]: 'x' is only passed along at the
-- self-call, which the specialisation's RULE rewrites, so no reboxing.
-- Expect NO -Wspec-constr-reboxing warning.
module T27628c where

foo :: Maybe Int -> Int -> Int
foo (Just m) 0 = m
foo x@(Just m) n = foo x (n - m)
foo Nothing _ = 0

f :: Int -> Int
f n = foo (Just n) n
