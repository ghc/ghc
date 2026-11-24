{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE NoListTuplePuns, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
-- The proof module reads packHead's Core unfolding, including in GHCi.
{-# OPTIONS_GHC -fwrite-source-equations -fno-omit-interface-pragmas -fobject-code #-}
-- | The Lists chapter's existential predicate witness for a Boolean list.
-- Adapted from PLFH under CC BY 4.0. See REFERENCES.md.
module BookAnyWitness (Any(..), AnyWitness(..), Evidence(..), Flag(..), packHead, anyToWitness, witnessToAny) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (List, Matchability)

-- | Evidence of the list position where the predicate holds.
type Any :: forall {m :: Matchability}. (Bool %m -> Type) -> List Bool -> Type
data Any p xs where
  Here :: forall {m :: Matchability} (p :: Bool %m -> Type) x xs.
          p x -> Any p (x : xs)
  There :: forall {m :: Matchability} (p :: Bool %m -> Type) xs x.
           Any p xs -> Any p (x : xs)

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

-- | Package the selected list position with its predicate evidence.
anyToWitness :: forall {m :: Matchability} {p :: Bool %m -> Type} {xs}.
                Any p xs -> AnyWitness p xs
anyToWitness (Here proof) = AnyWitness (Here Refl) proof
anyToWitness (There r) = case anyToWitness r of
  AnyWitness member proof -> AnyWitness (There member) proof

-- | Recover predicate evidence at its original list position.
witnessToAny :: forall {m :: Matchability} {p :: Bool %m -> Type} {xs}.
                AnyWitness p xs -> Any p xs
witnessToAny (AnyWitness (Here Refl) proof) = Here proof
witnessToAny (AnyWitness (There member) proof) =
  There (witnessToAny (AnyWitness member proof))
