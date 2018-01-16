-- Control.Monad.Except re-exports Control.Monad.Fix.
-- This test checks that the subordinate-name test
-- for a class operation (when renaming the instance decl)
-- works correctly.

module ShouldCompile where

import Control.Monad
import Control.Monad.Except

data Foo a = Foo a

instance Functor Foo where
  fmap = liftM

instance Applicative Foo where
  pure = Foo
  (<*>) = ap

instance Monad Foo where
  return         = pure
  (Foo a) >>= k  = k a

instance MonadFix Foo where
         mfix = undefined
