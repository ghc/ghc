{-# LANGUAGE KindSignatures, TypeFamilies #-}
module T9580a( Dimensional ) where

data family Dimensional var :: * -> *
newtype instance Dimensional Int v = Quantity' v
