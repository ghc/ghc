-- GHC 6.7 at one point said wog's type was:
--
--    wog :: forall t e (m :: * -> *).
--	     (Monad GHC.Prim.Any1, Monad m) =>
--	     t -> Something (m Bool) e
--
-- The stupid 'GHC.Prim.Any1' arose because of type ambiguity
-- which should be reported, and wasn't.

module ShouldFail where

data Something d e = Something{ bar::  d, initializer::e   }

foo :: (Monad m) => Something (m Bool) n
foo = undefined

wog x = foo{bar = return True}

