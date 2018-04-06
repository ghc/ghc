{-# LANGUAGE PolyKinds, DataKinds, TypeInType, TypeOperators #-}
module T14580 where
import Data.Kind

type Cat ob = ob -> ob -> Type
data ISO' :: Cat i -> i -> i -> Type
type ISO cat a b = ISO' cat a b -> Type
type (a <--> b) iso cat = ISO (iso :: cat a b)
