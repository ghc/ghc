{-# LANGUAGE ImpredicativeTypes, RankNTypes #-}

module T9730 where

class A a where

class B b where

class C c where
a2b :: (forall a. A a => a) -> (forall b. B b => b)
a2b = undefined

b2c :: (forall b. B b => b) -> (forall c. C c => c)
b2c = undefined

a2c :: (forall a. A a => a) -> (forall c. C c => c)
a2c = b2c . a2b
