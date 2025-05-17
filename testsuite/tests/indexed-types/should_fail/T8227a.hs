{-# LANGUAGE TypeFamilies #-}
module T8227a where

import Data.Kind (Type)

type family V a :: Type

type instance V Double             = Double
type instance V (a -> (b :: Type)) = V b
