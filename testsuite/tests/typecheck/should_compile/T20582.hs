{-# LANGUAGE QuantifiedConstraints #-}

module T20582 where

f :: (forall a. Ord (m a), forall a. Semigroup a => Eq (m a)) => m Int
f = f
