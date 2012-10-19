{-# LANGUAGE RankNTypes #-}
-- Blew up GHC 5.04, with:
--    Ambiguous type variable(s) `q' in the constraint `Foo q'
--    arising from a function with an overloaded argument type at Foo.hs:7
--	Expected type: Int -> (forall q1. (Foo q1) => q1 -> a) -> a
--	Inferred type: Int -> (q -> a) -> a
--    In the application `GHC.Err.noMethodBindingError "Foo.hs:7|Foo.foo"#'
--
-- Fix is to give wild-card args to the default methods
-- See TcClassDcl.mkDefMethRhs

module ShouldCompile where

class Foo a where
  op :: Eq c => c -> (forall b. Eq b => b->b) -> a -> a

instance Foo Int
