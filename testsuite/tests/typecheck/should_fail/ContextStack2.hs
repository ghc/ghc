{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

module ContextStack2 where

type family TF a :: *
type instance TF (a,b) = (TF a, TF b)

t :: (a ~ TF (a,Int)) => Int
t = undefined
