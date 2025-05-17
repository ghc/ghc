{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example1_Fixed where

import Data.Kind (Type)

-- Fixed version with explicit kind application
type family F a :: k
type instance F @Type Int = Char
type instance F @(Type->Type) Int = Maybe