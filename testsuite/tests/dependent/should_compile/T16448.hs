{-# Language TypeFamilies #-}
module T16448 where
import Data.Kind
type family Knd (h :: k) :: Type
type family Foo (h :: k) (d :: Knd h) :: Type
data X
type instance Knd X = Type
type instance Foo X (d :: Type) = X
