module T5327 where

newtype Size = Size Int

{-# INLINABLE val2 #-}
val2 = Size 17

-- In the core, we should see 34#, i.e. constant folding
-- should have happened.
--
-- We actually get eta-reduction thus:
--    tmp = I# 34#
--    f = gtInt tmp
-- because gtInt is marked INLINE with two parameters.
-- But that's ok
f n = case val2 of Size s -> s + s > n

