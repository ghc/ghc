{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, UndecidableInstances #-}

module Example2 where

import Data.Type.Bool (type (&&))

-- Current behavior
type (==) :: k -> k -> Bool
type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False

-- With proposed change
{-
type (==') :: k -> k -> Bool
type family a ==' b where
  f (a::k) ==' g (b::k) = f ==' g && a ==' b
  a ==' a = 'True
  _ ==' _ = 'False
-}