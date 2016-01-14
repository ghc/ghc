{-# LANGUAGE TypeFamilyDependencies #-}
module T10836 where

type family Foo a = r | r -> a where
    Foo Int  = Int
    Foo Bool = Int

type family Bar a = r | r -> a where
    Bar Int  = Int
    Bar Bool = Int
