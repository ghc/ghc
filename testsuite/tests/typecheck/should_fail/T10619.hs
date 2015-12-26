{-# LANGUAGE RankNTypes #-}

module T10619 where

-- tests that type checking doesn't care about order. all of these
-- should fail and be reported.

foo _ = if True
        then ((\x -> x) :: (forall a. a -> a) -> forall b. b -> b)
        else \y -> y

bar _ = if True
        then \y -> y
        else ((\x -> x) :: (forall a. a -> a) -> forall b. b -> b)

baz True  = (\x -> x) :: (forall a. a -> a) -> forall b. b -> b
baz False = \y -> y

quux False = \y -> y
quux True  = (\x -> x) :: (forall a. a -> a) -> forall b. b -> b
