{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, RankNTypes,
             ConstraintKinds, QuantifiedConstraints, TypeOperators,
             UndecidableInstances #-}

module T15359a where

class C a b
class a ~ b => D a b

data Dict c where
  Dict :: c => Dict c

foo :: (forall a b. C a b => D a b) => Dict (C a b) -> a -> b
foo Dict x = x
