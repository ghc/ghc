{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example5_Fixed where

import Data.Kind (Type)

-- Fixed version with explicit kind annotation in pattern
type family F a where
  F ((a :: Type) -> _) = Maybe a