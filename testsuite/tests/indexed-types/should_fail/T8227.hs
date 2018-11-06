{-# LANGUAGE TypeFamilies #-}
module T8227
  ( 
    absoluteToParam
  ) where

import Data.Kind (Type)
import T8227a

type family Scalar a :: Type
type instance Scalar (a -> v) = a -> Scalar v

arcLengthToParam :: Scalar (V p) -> p -> Scalar (V p) -> Scalar (V p)
arcLengthToParam = undefined

absoluteToParam :: Scalar (V a) -> a -> Scalar (V a)
absoluteToParam eps seg = arcLengthToParam eps eps

{-

Scalar (V a) ~ Scalar (V p0)
Scalar (V a) ~ p0
Scalar (V a) ~ Scalar (V p0) -> Scalar (V p0)

--->
Scalar (V a)  ~ fuv0
Scalar (V p0) ~ fuv1
fuv0 ~ fuv1
fuv0 ~ p0
fuv0 ~ fuv1 -> fuv1

---> p0 := fuv0

Scalar (V a)    ~ fuv0    (CFunEqCan)
Scalar (V fuv0) ~ fuv1    (CFunEqCan)
fuv0 ~ fuv1
p0 ~ fuv0
fuv0 ~ fuv1 -> fuv1


-}


