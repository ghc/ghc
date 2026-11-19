{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module InstalledStagedPair where

import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)

first :: Prelude.fst (Prelude.quotRem 17 5) :~: 3
first = Refl

second :: Prelude.id (Prelude.snd (Prelude.quotRem 17 5)) :~: 2
second = Refl

{-# RULES "installed/fst/natural" [~1] forall a b. Prelude.fst @Natural @Natural (a, b) = b #-}
{-# RULES "installed/snd/natural" [~1] forall a b. Prelude.snd @Natural @Natural (a, b) = a #-}
