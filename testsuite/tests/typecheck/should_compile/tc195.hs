{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, TypeSynonymInstances #-}

-- This one made GHC 6.4 loop becuause Unify.unify
-- didn't deal correctly with unifying
--	a :=: Foo a
-- where
--	type Foo a = a

module ShouldSucceed where

newtype PRef a = PRef a
type Drop1 a = a
class Ref a r | a -> r  where readRef :: a -> r
instance Ref (PRef a) (Drop1 a) where readRef (PRef v) = v



