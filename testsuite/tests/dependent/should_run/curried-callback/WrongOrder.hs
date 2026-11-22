{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module WrongOrder where
import Data.Type.Equality ((:~:)(Refl))
swapped :: forall a b. foreach (cmp :: a -> a -> b) -> foreach (x :: a) -> foreach (y :: a)
        -> cmp x y :~: cmp y x
swapped _ _ _ = Refl
