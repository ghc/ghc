{-# LANGUAGE RankNTypes #-}

--Tests alpha-renaming in with extended type-synonyms

module ShouldCompile where

type Foo x = forall a. a -> x

foo :: Foo (Foo ())
-- foo :: forall a. a -> forall b. b -> ()
--  NOT   forall a. a -> a -> ()
-- foo x = undefined
foo x = foo x

baz = foo 'c' True
