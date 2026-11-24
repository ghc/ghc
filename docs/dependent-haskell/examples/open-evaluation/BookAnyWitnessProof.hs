{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, NoListTuplePuns, ScopedTypeVariables, TypeOperators #-}
module BookAnyWitnessProof (Evidence(..), packHeadPreservesProof, decodeThere, membershipRoundTrip) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List)
import HaskLib (rewrite)
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

-- | Encoding a decoded witness preserves its list position and proof.
membershipRoundTrip :: forall {x :: Bool} (d :: Evidence x) {xs :: List Bool}.
                       foreach (r :: Any ((:~:) x) xs) ->
                       anyToWitness (witnessToAny ('AnyWitness r d :: AnyWitness Evidence xs))
                       :~: 'AnyWitness r d
membershipRoundTrip (Here equality) = case equality of Refl -> Refl
membershipRoundTrip (There r) = rewrite (membershipRoundTrip @d r) Refl
