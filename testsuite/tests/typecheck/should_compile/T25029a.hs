{-# LANGUAGE RankNTypes, TypeFamilies #-}

module T25029a where

type family F a

f :: forall a. (forall t. (F t ~ Int) => a -> Int) -> Int
f x = error "urk"

g :: Int
g = f id
