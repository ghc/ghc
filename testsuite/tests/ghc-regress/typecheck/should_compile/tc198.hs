{-# OPTIONS -fglasgow-exts #-}

-- This should work, because the type sig and the type
-- in the pattern match exactly

module Foo where

foo :: (forall a. a -> b) -> b
foo (f :: forall a. a -> b) = f undefined :: b
