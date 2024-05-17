{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE QuantifiedConstraints #-}
module QuantifiedConstraints where

class Foo a where
  fooed :: a

needsParensAroundContext :: (forall x. Foo (f x)) => f Int
needsParensAroundContext = fooed

needsNoParensAroundContext :: Foo (f Int) => f Int
needsNoParensAroundContext = fooed
