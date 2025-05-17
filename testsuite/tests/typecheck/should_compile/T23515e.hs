{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T23515e where

import Data.Kind (Type)

-- With warning
type family F a where
  F (a -> _) = Maybe a

-- Fixed version
type family G a where
  G ((a :: Type) -> _) = Maybe a