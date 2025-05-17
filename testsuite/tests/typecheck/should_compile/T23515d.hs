{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T23515d where

import Data.Kind (Type)

-- With warning
type family Foo a :: k
type instance Foo Int = Bool

-- Fixed version
type family Foo' a :: k
type instance Foo' @Type Int = Bool