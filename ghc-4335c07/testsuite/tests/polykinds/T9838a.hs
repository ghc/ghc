{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module T9838a where

type EqShow a = (Eq a, Show a)

type family F a
type instance F Int = Bool