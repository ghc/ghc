{-# LANGUAGE TypeFamilies #-}
module GhciKinds where

type family F a :: *
type instance F [a] = a -> F a
type instance F Int = Bool
