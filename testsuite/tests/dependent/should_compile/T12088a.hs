{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module T12088a where

import Data.Kind
import GHC.TypeLits

type family Open a
type instance Open Bool = Nat
type instance Open Float = Type
type instance Open Char = F Float

type F :: forall a -> Open a
type family F a
type instance F Bool = 42
type instance F Float = [Nat]
type instance F Char = '[0, 1]
