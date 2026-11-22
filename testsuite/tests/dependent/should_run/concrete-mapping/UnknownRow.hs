{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module UnknownRow where

import Data.Type.Equality ((:~:)(Refl))
import Row

-- A grouped alternative still inspects the receiver.
alwaysEmpty :: foreach (p :: Int -> Bool) -> foreach (r :: Row Bool Bool Int)
            -> Prelude.fmap p r :~: ('Empty :: Row Bool Bool Bool)
alwaysEmpty p r = Refl
