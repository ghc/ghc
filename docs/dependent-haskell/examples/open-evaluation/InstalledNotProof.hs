{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module InstalledNotProof (negateHead) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import InstalledNotSource (NegateAll)

-- | The head is False for every tail, including a tail unknown to this module.
negateHead :: forall (xs :: [Bool]). Proxy xs
           -> NegateAll ('True ': xs) :~: ('False ': NegateAll xs)
negateHead _ = Refl
