{-# LANGUAGE  TypeFamilies #-}
module T23512a where
import GHC.Types

type family F1 a :: k
type instance F1 Int = Any :: j -> j

data family D :: k -> Type
data instance D :: k -> Type
