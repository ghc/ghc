{-# LANGUAGE RankNTypes, DataKinds #-}
module T17355 where

import GHC.Generics
import GHC.Records

data Foo = Foo { poly :: forall a. a -> a }

instance Generic (forall a . a)
instance HasField "myPoly" Foo (forall a. a -> a) where
    getField (Foo x) = x
