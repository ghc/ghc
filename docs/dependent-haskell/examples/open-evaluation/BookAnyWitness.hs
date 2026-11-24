{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE NoListTuplePuns, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
-- The proof module reads packHead's Core unfolding, including in GHCi.
{-# OPTIONS_GHC -fno-omit-interface-pragmas -fobject-code #-}
-- | The head case of the Lists chapter's existential predicate witness.
-- Adapted from PLFH under CC BY 4.0. See REFERENCES.md.
module BookAnyWitness (Any(..), AnyWitness(..), Evidence(..), Flag(..), packHead) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List, Matchability)

-- | Evidence that the predicate holds for the first list element.
type Any :: forall {m :: Matchability}. (Bool %m -> Type) -> List Bool -> Type
data Any p xs where
  Here :: forall {m :: Matchability} (p :: Bool %m -> Type) x xs.
          p x -> Any p (x : xs)

-- | An existential member and the proof of the predicate at that member.
type AnyWitness :: forall {m :: Matchability}. (Bool %m -> Type) -> List Bool -> Type
data AnyWitness p xs where
  AnyWitness :: forall {m :: Matchability} {p :: Bool %m -> Type} {x} {xs}.
                Any ((:~:) x) xs -> p x -> AnyWitness p xs

-- | A flag indexed by the Boolean it represents.
data Flag b where
  AtTrue :: Flag True

-- | A predicate proof that retains a Boolean payload.
data Evidence (b :: Bool) = EvidenceBit Bool

-- | Package the proof and its head membership under an existential index.
{-# INLINE packHead #-}
packHead :: Flag b -> Evidence b -> AnyWitness Evidence (b : [])
packHead AtTrue proof = AnyWitness (Here Refl) proof
