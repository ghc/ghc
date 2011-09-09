module BindKindName where

type Foo (a :: DontExistKind) = a
