module T26323b where

f :: Int -> Int
f _ = 0
{-# NOINLINE f #-}

g :: Int -> Int
g _ = 1
{-# NOINLINE g #-}

h :: Int -> Int
h _ = 2
{-# NOINLINE h #-}

-- These two RULES loop, but that's OK because they are never active
-- at the same time.
{-# RULES "t1" [1]  forall x. g x = f x #-}
{-# RULES "t2" [~1] forall x. f x = g x #-}

-- Make sure we don't fire "t1" and "t2" in a loop in the RHS of a never-active rule.
{-# RULES "t"  [~]  forall x. h x = f x #-}

test :: Int
test = f 4 + g 5 + h 6
