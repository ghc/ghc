{-# LANGUAGE DefaultSignatures, TypeFamilies #-}

module DefaultAssociatedTypes where

-- | Documentation for Foo.
class Foo a where
  -- | Documentation for bar and baz.
  bar, baz :: a -> String

  -- | Doc for Qux
  type Qux a :: *

  -- | Doc for default Qux
  type Qux a = [a]
