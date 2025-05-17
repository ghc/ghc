{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T23515a where

import Data.Kind (Type)

type family F a :: k
type instance F Int = Char
type instance F Int = Maybe

-- Fixed version
type family G a :: k
type instance G @Type Int = Char
type instance G @(Type->Type) Int = Maybe