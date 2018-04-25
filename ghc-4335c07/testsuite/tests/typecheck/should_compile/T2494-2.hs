{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- Trac #2494, should compile ok

module Foo where

foo :: (forall m. Monad m => Maybe (m a) -> Maybe (m a)) -> Maybe a -> Maybe a
{-# NOINLINE [1] foo #-}
foo _ x = x

{-# RULES

"foo/foo"
  forall (f :: forall m. Monad m => Maybe (m a) -> Maybe (m a))
         (g :: forall m. Monad m => Maybe (m a) -> Maybe (m a)) x.
  foo f (foo g x) = foo (f . g) x
 #-}
