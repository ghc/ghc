{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

import GHC.TypeLits

newtype Baz = Baz Foo
  deriving Bar

newtype Foo = Foo Int

class Bar a where
  bar :: a

instance (TypeError (Text "Boo")) => Bar Foo where
  bar = undefined
