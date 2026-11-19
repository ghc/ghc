{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, KindSignatures #-}
{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
module PreludePairExport (first, second) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

-- | Return the evidence for the first field of an unknown pair.
first :: forall k l (p :: (k, l)) (x :: k).
         Prelude.fst p ~ x => Proxy p -> Proxy x -> x :~: Prelude.fst p
first _ _ = Refl

-- | Return the evidence for the second field of an unknown pair.
second :: forall k l (p :: (k, l)) (x :: l).
          Prelude.snd p ~ x => Proxy p -> Proxy x -> x :~: Prelude.snd p
second _ _ = Refl
