{-# LANGUAGE UnboxedTuples #-}
module EnforceEpt006 where

-- If we have
-- f x y =
--   join j =
--       <rhs> -- TagBottoming
--   in case x of
--       1 -> j
--       2 -> j
--       _ ->
--         <something> -- <TagTuple[TagEPT, TagEPT]]>

-- Similar to inferTage005 we want to give `f` the signature: TagFun <TagTuple[TagEPT, TagEPT]]>
-- This requires us to give `j` a `TagFun[TagBottoming]` signature, which is safe because
-- either its execution will diverge, or the resulting tuple *will* have two
-- tagged values.

data T = A Int Int Int Int Int Int | B

f :: Int -> Int -> (# T, T #)
-- This case-of-case is just here to be optimized into a nullary join point.
f x y = case (case x of { 0 -> False; 1 -> False; _ -> True }) of
  False -> error ("f: bad input " ++ show x ++ " " ++ show y ++ " " ++ show (x*y))
  True  -> (# A (y+1) (y*2) (y-3) (y*y) (y+x) (y*x), B #)
{-# NOINLINE f #-}

data S = S !T

g :: Int -> Int -> S
g x y = case f x y of (# r, _ #) -> S r
