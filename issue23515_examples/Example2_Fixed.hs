{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, UndecidableInstances #-}

module Example2_Fixed where

import Data.Kind (Type)
import Data.Type.Bool (type (&&))

-- Fixed version with explicit kind annotation in patterns
type (==) :: k -> k -> Bool
type family a == b where
  f (a::k) == g (b::k) = f == g && a == b
  a == a = 'True
  _ == _ = 'False