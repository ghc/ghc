{-# LANGUAGE TypeFamilies, PolyKinds #-}

module ClosedFam3 where

type family Foo a where
  Foo Int = Bool

type family Bar a where
  Bar Int = Bool
  Bar Double = Char

type family Baz (a :: k) where
  Baz Int = Bool