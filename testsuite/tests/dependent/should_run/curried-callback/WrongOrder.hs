{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module WrongOrder where
import Data.Type.Equality ((:~:)(Refl))
swapped :: foreach (cmp :: Bool -> Bool -> Ordering) -> foreach (x :: Bool) -> foreach (y :: Bool)
        -> cmp x y :~: cmp y x
swapped _ _ _ = Refl
