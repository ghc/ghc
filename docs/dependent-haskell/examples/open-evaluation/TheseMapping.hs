{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module TheseMapping (mapBoth) where

import Data.Type.Equality ((:~:)(Refl))
import These (These(..))

-- | Mapping the second field preserves the label and applies the given predicate.
mapBoth :: forall a. foreach (p :: a -> Bool)
        -> foreach (label :: Char) -> foreach (x :: a)
        -> fmap p ('These label x) :~: 'These label (p x)
mapBoth _ _ _ = Refl
