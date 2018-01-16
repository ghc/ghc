{-# LANGUAGE PartialTypeSignatures #-}
module NamedWildcardsEnabled where

foo :: Eq a => a -> (a, _)
foo x = (x, x)

test = foo id

-- As id (forall a. a -> a) doesn't implement Eq, the we cannot apply
-- foo with it. This test checks that foo gets the annotated type,
-- including constraints, not just the inferred type.
