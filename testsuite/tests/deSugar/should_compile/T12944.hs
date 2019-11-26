{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O #-}

module T12944 () where

import Data.Kind (Type)

class AdditiveGroup v where
  (^+^) :: v -> v -> v
  negateV :: v -> v
  (^-^) :: v -> v -> v
  v ^-^ v' = v ^+^ negateV v'

class AdditiveGroup v => VectorSpace v where
  type Scalar v :: Type
  (*^) :: Scalar v -> v -> v

data Poly1 a = Poly1 a a

data IntOfLog poly a = IntOfLog !a !(poly a)

instance Num a => AdditiveGroup (Poly1 a) where
    {-# INLINE (^+^) #-}
    {-# INLINE negateV #-}
    Poly1 a b ^+^ Poly1 a' b' = Poly1 (a + a') (b + b')
    negateV (Poly1 a b) = Poly1 (negate a) (negate b)

instance (AdditiveGroup (poly a), Num a) => AdditiveGroup (IntOfLog poly a) where
    {-# INLINE (^+^) #-}
    {-# INLINE negateV #-}
    IntOfLog k p ^+^ IntOfLog k' p' = IntOfLog (k + k') (p ^+^ p')
    negateV (IntOfLog k p) = IntOfLog (negate k) (negateV p)
    {-# SPECIALISE instance Num a => AdditiveGroup (IntOfLog Poly1 a) #-}
        -- This pragmas caused the crash

instance (VectorSpace (poly a), Scalar (poly a) ~ a, Num a) => VectorSpace (IntOfLog poly a) where
    type Scalar (IntOfLog poly a) = a
    s *^ IntOfLog k p = IntOfLog (s * k) (s *^ p)
