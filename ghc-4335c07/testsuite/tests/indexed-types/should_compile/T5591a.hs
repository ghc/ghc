{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
module T5591a where

data a :=: b where 
    Refl :: a :=: a

subst :: a :=: b -> f a -> f b
subst Refl = id 

-- Then this doesn't work (error message at the bottom):

inj1 :: forall f a b. f a :=: f b -> a :=: b
inj1 Refl = Refl

-- But one can still construct it with a trick that Oleg used in the context of 
-- Leibniz equality:

type family Arg fa

type instance Arg (f a) = a

newtype Helper fa fa' = Helper { runHelper :: Arg fa :=: Arg fa' }

inj2 :: forall f a b. f a :=: f b -> a :=: b
inj2 p = runHelper (subst p (Helper Refl :: Helper (f a) (f a)))
