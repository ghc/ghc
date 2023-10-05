{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, RankNTypes,
             ConstraintKinds, QuantifiedConstraints, TypeOperators #-}

module T15359 where

class C a b

data Dict c where
  Dict :: c => Dict c

foo :: (forall a b. C a b => a ~ b) => Dict (C a b) -> a -> b
foo Dict x = x
