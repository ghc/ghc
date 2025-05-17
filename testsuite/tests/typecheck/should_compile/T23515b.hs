{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, UndecidableInstances #-}

module T23515b where

import Data.Kind (Type)
import Data.Type.Bool (type (&&))

-- With warning
type (==) :: k -> k -> Bool
type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False

-- Fixed version
type (==') :: k -> k -> Bool
type family a ==' b where
  f (a::k) ==' g (b::k) = f ==' g && a ==' b
  a ==' a = 'True
  _ ==' _ = 'False