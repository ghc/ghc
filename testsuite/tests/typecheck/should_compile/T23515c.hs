{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T23515c where

import Data.Kind (Type)

-- With warning
type family N' a where
  N' (t a) = [a]
  N' a     = ()

-- Fixed version
type family N'' a where
  N'' (t (a :: Type)) = [a]
  N'' a               = ()