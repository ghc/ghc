{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module T22696a where

import GHC.TypeLits

newtype Foo = Foo Int

class Bar a where
  bar :: a

instance (TypeError (Text "Boo")) => Bar Foo where
  bar = undefined

newtype Baz = Baz Foo

deriving instance TypeError (Text "Boo") => Bar Baz
