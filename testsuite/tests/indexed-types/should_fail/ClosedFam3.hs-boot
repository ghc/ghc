{-# LANGUAGE TypeFamilies, PolyKinds, TopLevelKindSignatures #-}

module ClosedFam3 where

import Data.Kind (Type)

type family Foo a where
  Foo Int = Bool

type family Bar a where
  Bar Int = Bool
  Bar Double = Char

type Baz :: k -> Type
type family Baz a where
  Baz Int = Bool
