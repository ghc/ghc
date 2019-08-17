{-# LANGUAGE TypeFamilies, PolyKinds #-}

module ClosedFam3 where

import Data.Kind (Type)

type family Foo a where
  Foo Int = Bool

type family Bar a where
  Bar Int = Bool
  Bar Double = Char

type family Baz (a :: k) :: Type where
  Baz Int = Bool
