{-# LANGUAGE UnboxedTuples #-}
module EnforceEpt005 where

-- If we have
-- f x y =
--   join j =
--       <rhs> -- <TagTuple[TagEPT, TagEPT]]>
--   in case x of
--       1 -> j
--       2 -> j
--       _ ->
--         <something> -- <TagTuple[TagEPT, TagEPT]]>

-- Consider the case where rhs and something both represent a tag value of <TagTuple[TagEPT, TagEPT]]>
-- In this case we want `f` to also give rise to TagFun [TagTuple[TagEPT, TagEPT]]]

-- To achieve this we have to recognize j as a zero-arity function equivalent
-- which is what this test checks.

-- The payoff is in `g` `r` is then known to be tagged, so `S r` needs no eval.
-- Only an unboxed tuple result exposes this.

-- We disable worker/wrapper and CPR so `f` matches more closely the tests source.

data T = A Int Int Int Int Int Int | B

f :: Int -> Int -> (# T, T #)
f x y = case x of
  0 -> j
  1 -> j
  _ -> (# B, B #)
  where
    j = (# A (y+1) (y*2) (y-3) (y*y) (y+x) (y*x), B #)
{-# NOINLINE f #-}

data S = S !T

g :: Int -> Int -> S
g x y = case f x y of (# r, _ #) -> S r
