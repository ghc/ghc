{-# LANGUAGE TypeFamilies #-}
module T17067 where

import Data.Kind

data family D1 a
data family D2 :: Type -> Type

type family F a
type instance F (D1 a) = a
type instance F (D2 a) = a
