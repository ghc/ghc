-- A variant of T5654 where instead of evaluating directly to a
-- function, f evaluates to a new PAP.  This exposes a slightly
-- different but related bug, where when we create a new PAP by
-- applying arguments to an existing PAP, we should take into account
-- the stack on the original PAP.

-- The stack we should see is main->f->g->h, but if we get this wrong
-- (GHC 7.10) then the stack is main->f->h.

{-# NOINLINE f #-}
f :: Int -> Int
f = g 3

{-# NOINLINE g #-}
g :: Int -> Int -> Int
g = h 4

{-# NOINLINE h #-}
h :: Int -> Int -> Int -> Int
h x y z = x + y + z

main =  return $! f 5
