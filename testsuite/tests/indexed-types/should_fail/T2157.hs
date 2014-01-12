{-# LANGUAGE TypeFamilies, LiberalTypeSynonyms #-}

module T2157 where

type S a b = a
type family F a :: * -> *
type instance F a = S a
