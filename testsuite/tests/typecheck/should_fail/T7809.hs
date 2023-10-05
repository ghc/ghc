{-# LANGUAGE RankNTypes, TypeFamilies, LiberalTypeSynonyms #-}
module T7809 where

type PolyId = (forall a. a -> a)

type family F a

foo :: F PolyId
foo = undefined