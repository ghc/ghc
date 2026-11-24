{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NoListTuplePuns, TypeOperators #-}
module BookAnyWitnessProof (Evidence(..), packHeadPreservesProof) where

import Data.Type.Equality ((:~:)(Refl))
import BookAnyWitness

-- | The promoted result contains the same proof for every payload.
packHeadPreservesProof :: forall (proof :: Evidence True).
                          packHead AtTrue proof :~: 'AnyWitness (Here Refl) proof
packHeadPreservesProof = Refl
