{-# LANGUAGE TypeFamilies, PolyKinds, ScopedTypeVariables #-}
module T7404 where

type family Foo (x :: *) (y :: x)
