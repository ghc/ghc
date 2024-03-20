module T24553 where

import GHC.Exts

type Foo :: * -> forall r. TYPE r -> *
newtype Foo m a = MkFoo ()

type Bar = Foo :: forall r. * -> TYPE r -> *
