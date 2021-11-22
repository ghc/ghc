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

-- Apparently we derive
--  instance TypeError (Text "Boo") => Bar Baz
--
-- Is that really what we want?  It defers the type
-- error... surely we should use standalone deriving
-- if that is what we want?
-- See GHC.Tc.Validity.validDerivPred and #22696.