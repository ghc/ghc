{-# LANGUAGE DataKinds, TypeData #-}
module T24027(Foo(Foo1, Foo2)) where

import qualified T24027_aux

type data Foo = Foo1 | Foo2 | Foo3
