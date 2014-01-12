{-# LANGUAGE TypeFamilies #-}
module T8227
  ( 
    absoluteToParam
  ) where

import T8227a

type family Scalar a :: *
type instance Scalar (a -> v) = a -> Scalar v

arcLengthToParam :: Scalar (V p) -> p -> Scalar (V p) -> Scalar (V p)
arcLengthToParam = undefined

absoluteToParam :: Scalar (V a) -> a -> Scalar (V a)
absoluteToParam eps seg = arcLengthToParam eps eps

{-

Scalar (V a) ~ Scalar (V p0)
Scalar (V a) ~ p0
Scalar (V a) ~ Scalar (V p0) -> Scalar (V p0)


Scalar (V a) ~ t0
Scalar (V p0) ~ t0
Scalar (V a) ~ p0
Scalar (V a) ~ t0 -> t0

Scalar (V a) ~ t0
Scalar (V t0) ~ t0
Scalar (V a) ~ t0 -> t0


-}


