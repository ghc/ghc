
-- This one triggers the bug reported in #22028, which
-- was in a test for #1092
-- The problem is that the rule
--      forall w. f (\v->w) = w
-- erroneously matches the call
--      f id
-- And that caused an assertion error.

module Foo where

f :: (Int -> Int) -> Int
{-# NOINLINE f #-}
f g = g 4
{-# RULES "f" forall w. f (\v->w) = w  #-}

h1 = f (\v -> v)   -- Rule should not fire
h2 = f id          -- Rule should not fire
h3 = f (\v -> 3)   -- Rule should fire
