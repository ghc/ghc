{-# LANGUAGE RankNTypes, DeepSubsumption #-}

module T21548a where

f1 :: (forall a. a -> forall b. b -> b) -> Int
f1 = f1

g1 :: forall p q. p -> q -> q
g1 = g1

foo1 = f1 g1

