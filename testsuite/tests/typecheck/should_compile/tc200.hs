{-# OPTIONS_GHC -w #-}
{-# LANGUAGE ConstrainedClassMethods #-}  -- Needed for 'baz'

-- A nasty case that crashed GHC 6.4 with a Lint error; 
-- see Note [Multiple instantiation] in GHC.Tc.Gen.Expr

module ShouldCompile where

class C a where
  foo :: Eq b => b -> a -> Int
  baz :: Eq a => Int -> a -> Int

instance C Int where
  baz = foo
