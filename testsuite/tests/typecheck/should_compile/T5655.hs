{-# LANGUAGE TypeFamilies, GADTs, ConstraintKinds, RankNTypes #-}
module T5655 where

import GHC.Prim (Constraint)

class Show a => Twice a where twice :: a -> a

instance Twice Int where twice = (*2)

data ETwice where ETwice :: Twice a => a -> ETwice

class E e where
    type C e :: * -> Constraint
    ap :: (forall a. C e a => a -> r) -> e -> r

instance E ETwice where
    type C ETwice = Twice
    ap f (ETwice a) = f a

f :: (E e, C e ~ Twice) => e -> ETwice
f = ap (ETwice . twice)

foo :: ETwice
foo = ETwice (5 :: Int)

bar :: IO ()
bar = ap print (f foo)
