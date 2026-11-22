{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module TheseMapping (mapAt, mapBoth) where

import Data.Type.Equality ((:~:)(Refl))
import These (These(..))

-- | Mapping the second field preserves the label and applies the given function.
mapBoth :: forall a b. foreach (p :: a -> b)
        -> foreach (label :: Char) -> foreach (x :: a)
        -> fmap p ('These label x) :~: 'These label (p x)
mapBoth _ _ _ = Refl

-- | Fix the first argument of a runtime function and map it over the second field.
mapAt :: forall a b c label. foreach (f :: a -> b -> c)
      -> foreach (pivot :: a) -> foreach (tag :: label) -> foreach (x :: b)
      -> Prelude.fmap (f pivot) ('These tag x) :~: 'These tag (f pivot x)
mapAt _ _ _ _ = Refl
