{-# LANGUAGE RankNTypes #-}

--Tests alpha-renaming in with extended type-synonyms

module ShouldCompile where

type Foo x = forall a. a -> x

foo :: Foo (Foo ())
-- foo :: forall a b. a -> b -> ()
--  NOT   forall a. a -> a -> ()
foo =  undefined

baz = foo 'c' True
