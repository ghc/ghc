{-# LANGUAGE TypeFamilies #-}
module T8227a where

type family V a :: *

type instance V Double    = Double
type instance V (a -> b)   = V b