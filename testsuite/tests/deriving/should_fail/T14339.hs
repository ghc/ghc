{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Bug where

import GHC.TypeLits

newtype Foo = Foo Int

class Bar a where
  bar :: a

instance (TypeError (Text "Boo")) => Bar Foo where
  bar = undefined

newtype Baz = Baz Foo
  deriving Bar

-- We derive:
--
--  instance TypeError (Text "Boo") => Bar Baz
--
-- And error out due to the TypeError. See also
-- deriving/should_compile/T22696a, which uses StandaloneDeriving to write a
-- valid instance with a TypeError constraint in its instance context.
