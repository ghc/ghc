{-# LANGUAGE GADTs, PolyKinds, TypeFamilies #-}
module FamilyTuple (Tuple(Tup2), C(Refines)) where

import Data.Kind (Type)

-- | A tuple family with one instance for pairs.
data family Tuple :: k
data instance Tuple (a :: Type) (b :: Type) = Tup2 a b

-- | A family whose constructor fixes the first component of its index.
data family C a
data instance C (a, b) where
    Refines :: C (Int, b)
