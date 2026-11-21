{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeFamilies #-}
module TupleProvider (Tuple(Tup0, Tup1, Tup2), At(..), fields, C(MkC, Refines), N(MkN)) where

import Data.Kind (Type)

data family Tuple :: k
data instance Tuple = Tup0
data instance Tuple (a :: Type) = Tup1 a
data instance Tuple (a :: Type) (b :: Type) = Tup2 a b

data At (x :: k) = At

fields :: Tuple a b -> (a, b)
fields (Tup2 a b) = (a, b)

data family C a
data instance C (a, b) where
    MkC :: Show b => b -> C (a, b)
    Refines :: C (Int, b)

data family N a
newtype instance N Int = MkN Bool
