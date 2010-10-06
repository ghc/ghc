
-- This one triggers the bug reported in Trac #1092
-- The problem is that the rule 
--	forall w. f (\v->w) = w 
-- erroneously matches the call
--	f id
--
-- Lint catches the error

module Foo where

f :: (Int -> Int) -> Int
{-# NOINLINE f #-}
f g = g 4
{-# RULES 
  "f" forall w. f (\v->w) = w 
 #-}

h = f id
