{-# LANGUAGE DataKinds, TypeFamilies, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module T23156 where

import Prelude
import GHC.TypeLits
import Data.Kind

type BooleanOf2 :: Type -> Type
type family BooleanOf2 a

type instance BooleanOf2 Double = Double

-- Needs to be a type family, changing this to a datatype makes it fast
type TensorOf2 :: Nat -> Type -> Type
type family TensorOf2 k a

type instance TensorOf2 n Double = Double


-- With GHC 9.4 and 9.6, typechecking was
-- exponential in the size of this tuple
type ADReady r =
  (  BooleanOf2 r ~ BooleanOf2 (TensorOf2 1 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 2 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 3 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 4 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 5 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 6 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 7 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 8 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 9 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 10 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 11 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 12 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 13 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 14 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 15 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 16 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 17 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 18 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 19 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 20 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 21 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 22 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 23 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 24 r)
    , BooleanOf2 r ~ BooleanOf2 (TensorOf2 25 r)
  )

f :: forall r . (ADReady r) => ()
f = undefined

-- This uses a lot of memory
g :: _ => ()
g = f

-- This is fine
g' = f @Double
