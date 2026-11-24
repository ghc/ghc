{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NoListTuplePuns, TypeOperators #-}
module BookAnyWitnessProof (Evidence(..), packHeadPreservesProof, decodeThere) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List)
import BookAnyWitness

-- | The promoted result contains the same proof for every payload.
packHeadPreservesProof :: forall (proof :: Evidence True).
                          packHead AtTrue proof :~: 'AnyWitness (Here Refl) proof
packHeadPreservesProof = Refl

-- | Decoding a tail witness keeps its position and the original predicate proof.
decodeThere :: forall (x :: Bool) (y :: Bool) (xs :: List Bool).
               foreach (r :: Any ((:~:) x) xs) -> foreach (d :: Evidence x) ->
               witnessToAny ('AnyWitness (There r) d :: AnyWitness Evidence (y : xs))
               :~: There (witnessToAny ('AnyWitness r d :: AnyWitness Evidence xs) :: Any Evidence xs)
decodeThere _ _ = Refl
