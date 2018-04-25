{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- Trac #2494, should generate an error message

module Foo where

foo :: (forall m. Monad m => Maybe (m a) -> Maybe (m a)) -> Maybe a -> Maybe a
foo _ x = x

{-# RULES

"foo/foo"
  forall (f :: forall m. Monad m => Maybe (m a) -> Maybe (m a))
         (g :: forall m. Monad m => Maybe (m b) -> Maybe (m b)) x.
  foo f (foo g x) = foo (f . g) x
 #-}
