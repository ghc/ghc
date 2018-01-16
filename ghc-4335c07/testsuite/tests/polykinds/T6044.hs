{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, KindSignatures #-}

module T6044 where

type family Foo (a :: k) :: Maybe k
type instance Foo a = Just a
