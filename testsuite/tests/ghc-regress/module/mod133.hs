-- Control.Monad.Error re-exports Control.Monad.Fix.
-- This test checks that the subordinate-name test
-- for a class operation (when renaming the instance decl)
-- works correctly.

module ShouldCompile where

import Control.Monad.Error

data Foo a = Foo a

instance Monad Foo where
  return a       = Foo a
  (Foo a) >>= k  = k a

instance MonadFix Foo where
         mfix = undefined
