{-# LANGUAGE TypeFamilies #-}
module FancyContextsWithoutExtension2 where

type family Indexed a :: * -> Constraint
type instance Indexed Int = Show
type instance Indexed Bool = Num

f :: (Indexed Int a) => a -> a
f = undefined