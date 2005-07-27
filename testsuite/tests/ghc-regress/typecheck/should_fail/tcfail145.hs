{-# OPTIONS -fglasgow-exts #-}

-- This fails, because the type in the pattern doesn't exactly match
-- the context type.  We don't do subsumption in patterns any more.

module Foo where

foo :: (forall a. a -> a) -> [b]
foo (f :: forall a. [a] -> [a]) = f undefined

