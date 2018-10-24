{-# LANGUAGE MultiParamTypeClasses, PolyKinds, QuantifiedConstraints, RankNTypes #-}

module T15334 where

class C m a
class D m a

f :: (forall a. Eq a => (C m a, D m a)) => m a
f = undefined
