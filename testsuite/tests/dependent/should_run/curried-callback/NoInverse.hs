{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NoInverse where

import Data.Type.Equality ((:~:)(Refl))

noInverse :: foreach (cmp :: Bool -> Bool -> Ordering) -> foreach (x :: Bool)
          -> foreach (y :: Bool) -> foreach (z :: Bool)
          -> cmp x y :~: cmp x z -> y :~: z
noInverse _ _ _ _ Refl = Refl
