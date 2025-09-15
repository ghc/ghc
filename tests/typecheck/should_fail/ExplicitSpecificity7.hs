{-# LANGUAGE RankNTypes, StandaloneKindSignatures, DataKinds, PolyKinds, TypeApplications #-}

module ExplicitSpecificity7 where

type Foo :: forall a {b}. a -> b -> b
type Foo x y = y

type Bar = Foo @Bool @Int True 42
