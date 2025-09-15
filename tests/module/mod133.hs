-- Control.Monad re-exports Control.Monad.Fail.
-- This test checks that the subordinate-name test
-- for a class operation (when renaming the instance decl)
-- works correctly.

module ShouldCompile where

import Control.Monad

data Foo a = Foo a

instance Functor Foo where
  fmap = liftM

instance Applicative Foo where
  pure = Foo
  (<*>) = ap

instance Monad Foo where
  return         = pure
  (Foo a) >>= k  = k a

instance MonadFail Foo where
         fail = undefined
