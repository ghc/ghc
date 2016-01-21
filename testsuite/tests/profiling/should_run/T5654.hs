-- Tests for a bug in the handling of cost-centre stacks in the
-- runtime, where we lose the current cost-centre stack when
-- evaluating a function.

{-# NOINLINE f #-}
f :: Int -> Int
f = g   -- here we should remember the stack under which g was evaluated

{-# NOINLINE g #-}
g :: Int -> Int
g x = x + 1

main =  return $! f 3

