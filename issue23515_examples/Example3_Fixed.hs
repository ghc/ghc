{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example3_Fixed where

import Data.Kind (Type)

-- Fixed version with explicit kind annotation in pattern
type family N' a where
  N' (t (a :: Type)) = [a]
  N' a               = ()