{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes, ConstraintKinds, TypeFamilies #-}

module T9090 where

import GHC.Exts (Constraint)

type family F (c :: Constraint) :: Constraint
type instance F (Eq a) = Eq a

-- checks
f :: Eq b => (forall a. F (Eq a) => f a -> Bool) -> f b -> Bool
f _ = error "urk" -- g x = g x

-- checks
f' :: Eq b => (forall a. Eq a => f a -> Bool) -> f b -> Bool
f' = f

-- checks, so GHC seems to think that both types are interchangeable
f'' :: Eq b => (forall a. F (Eq a) => f a -> Bool) -> f b -> Bool
f'' = f'

-- checks
test' y = f' (\ (Just x) -> x /= x) y

-- fails
test y = f (\ (Just x) -> x /= x) y

-- fails too, unsurprisingly
test'' y = f'' (\ (Just x) -> x /= x) y
