{-# LANGUAGE KindSignatures, TypeFamilies #-}
module T9580a( Dimensional ) where

import Data.Kind (Type)

data family Dimensional var :: Type -> Type
newtype instance Dimensional Int v = Quantity' v
