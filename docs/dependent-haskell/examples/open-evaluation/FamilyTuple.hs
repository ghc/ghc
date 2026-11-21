{-# LANGUAGE PolyKinds, TypeFamilies #-}
module FamilyTuple (Tuple(Tup2)) where

import Data.Kind (Type)

-- | A tuple family with one instance for pairs.
data family Tuple :: k
data instance Tuple (a :: Type) (b :: Type) = Tup2 a b
