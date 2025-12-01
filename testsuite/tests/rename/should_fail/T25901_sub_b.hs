{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_b where

import GHC.Generics (Generic (data ..))  -- should not bring the associated type Rep into scope

data Foo = Foo deriving Generic

type FooRep = Rep Foo  -- error: Rep is not in scope