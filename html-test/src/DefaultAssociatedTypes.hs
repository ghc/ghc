{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}

module DefaultAssociatedTypes where

import Data.Kind (Type)

-- | Documentation for Foo.
class Foo a where
  -- | Documentation for bar and baz.
  bar, baz :: a -> String

  -- | Doc for Qux
  type Qux a :: Type

  -- | Doc for default Qux
  type Qux a = [a]
