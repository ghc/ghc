{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example4_Fixed where

import Data.Kind (Type)

-- Fixed version with explicit kind application
type family Foo a :: k
type instance Foo @Type Int = Bool