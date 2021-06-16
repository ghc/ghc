-- variant of T5327, where we force the newtype to have a wrapper
{-# LANGUAGE GADTs, ExplicitForAll #-}
module T16254 where

newtype Size a b where
  Size :: forall b a. Int -> Size a b

{-# INLINABLE val2 #-}
val2 = Size 17

-- In the core, we should see 34#, i.e. constant folding
-- should have happened.
--
-- We actually get eta-reduction thus:
--    tmp = I# 34#
--    f = gtInt tmp
-- beucase gtInt is marked INLINE with two parameters.
-- But that's ok
f n = case val2 of Size s -> s + s > n
