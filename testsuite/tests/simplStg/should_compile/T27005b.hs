{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module T27005b where

-- A function returning an unboxed tuple whose single component is itself a
-- function. With the TagFun/TagVal split, `f`'s tag signature records that the
-- returned component is properly tagged (TagEPT), so when `g` scrutinises the
-- result of `f h` the binder `x` is already known to be evaluated. Hence the
-- pair `(x, x)` is built without re-evaluating `x`.
f :: (Bool -> Bool) -> (# Bool -> Bool #)
f !x = (# x #)
{-# NOINLINE f #-}

g :: (Bool -> Bool) -> (Bool -> Bool, Bool -> Bool)
g h =
  case f h of
    (# !x #) -> (x, x)
