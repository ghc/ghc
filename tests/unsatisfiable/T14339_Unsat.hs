{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module T14339_Unsat where

import GHC.TypeError

newtype Foo = Foo Int

class Bar a where
  bar :: a

instance (Unsatisfiable (Text "Boo")) => Bar Foo where
  bar = undefined

newtype Baz1 = Baz1 Foo


-- should be ok
deriving instance Unsatisfiable (Text "Shouldn't see this") => Bar Baz1

-- should emit the error "Boo"
newtype Baz2 = Baz2 Foo
  deriving Bar
