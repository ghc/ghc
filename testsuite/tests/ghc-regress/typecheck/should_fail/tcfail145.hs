{-# OPTIONS -fglasgow-exts #-}

-- This fails, because the type in the pattern doesn't exactly match
-- the context type.  We don't do subsumption in patterns any more.

module Foo where

foo :: (forall c. c -> c) -> [Char]
foo (f :: forall a. [a] -> [a]) = f "foo"

