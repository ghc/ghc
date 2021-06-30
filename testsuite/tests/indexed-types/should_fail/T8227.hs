{-# LANGUAGE TypeFamilies #-}
module T8227
  (
    absoluteToParam
  ) where

import Data.Kind (Type)
import T8227a

{-
type family V a :: Type

type instance V Double    = Double
type instance V (a -> b)   = V b
-}

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


{-
Once upon a time, we reported errors with
     Couldn't match expected type: Scalar (V a)
                  with actual type: Scalar (V (Scalar (V a)))
                                    -> Scalar (V (Scalar (V a)))
Now, it's
     Couldn't match type: Scalar (V a)
                     with: t0 -> t0
      Expected: Scalar (V a)
        Actual: Scalar (V (t0 -> t0)) -> Scalar (V (t0 -> t0))
The old message is a bit better. But the only way we can get to the old
message is to allow a wanted to rewrite a wanted. This is a bad idea
in general, so we accept the error message regression. The new message
isn't wrong, and perhaps in some ways its simplicity is actually an
improvement over the previous one.
-}
