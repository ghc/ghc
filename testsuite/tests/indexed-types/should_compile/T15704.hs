{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T15704 where

import Data.Kind

data family D :: k

type family F (a :: k) :: Type

type instance F D = Int
type instance F (D a) = Char
