{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeIndexedGadtFieldsProvider where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import PackedVecLibrary (Nat(..), Vec(..))

type Shape :: forall (n :: Nat) -> Vec Bool n -> Type
data Shape n xs where
    ConsSeen :: forall (h :: Bool) (m :: Nat) (t :: Vec Bool m).
                Shape ('S m) ('VCons h t)

{-# INLINE inspectCons #-}
inspectCons :: foreach (xs :: Vec Bool n) -> Maybe (Shape n xs)
inspectCons xs = case xs of
    VNil -> Nothing
#ifdef WRONG_HEAD
    VCons h (t :: Vec Bool m) -> Just (ConsSeen @'False @m @t)
#else
    VCons h (t :: Vec Bool m) -> Just (ConsSeen @h @m @t)
#endif

{-# INLINE inspectAnonymous #-}
inspectAnonymous :: foreach (xs :: Vec Bool n) -> Maybe (Shape n xs)
inspectAnonymous xs = case xs of
    VNil -> Nothing
    VCons _ _ -> Just ConsSeen

{-# INLINE inspectEquations #-}
inspectEquations :: foreach (xs :: Vec Bool n) -> Maybe (Shape n xs)
inspectEquations VNil = Nothing
inspectEquations (VCons _ _) = Just ConsSeen

data Hold (n :: Nat) where
    Hold :: Vec Bool m -> Hold n

type Held :: forall (n :: Nat). Hold n -> Type
data Held x where
    Held :: Held ('Hold xs)

{-# INLINE inspectHold #-}
inspectHold :: foreach (x :: Hold n) -> Held x
inspectHold x = case x of
    Hold _ -> Held

{-# INLINE inspectKnown #-}
inspectKnown :: foreach (xs :: Vec Bool ('S n)) -> Shape ('S n) xs
inspectKnown xs = case xs of
    VCons h (t :: Vec Bool m) -> ConsSeen @h @m @t

#ifdef WRONG_SCRUTINEE
other :: foreach (xs :: Vec Bool n) -> foreach (ys :: Vec Bool n)
      -> Maybe (Shape n xs, Shape n ys)
other xs _ = case xs of
    VNil -> Nothing
    VCons h (t :: Vec Bool m) -> Just (ConsSeen @h @m @t, ConsSeen @h @m @t)
#endif

type family Fields (n :: Nat) :: Type where
    Fields 'Z = Maybe Bool
    Fields ('S n) = Either Bool (Fields n)

-- | Transport payloads between retained indices under a nominal proof.
transportFields :: foreach (from :: Nat) -> foreach (to :: Nat) ->
                   from :~: to -> Fields from -> Fields to
transportFields _ _ Refl payload = payload

-- | Open the zero branch of Fields after inspecting the equality.
{-# INLINE simplifyFields #-}
simplifyFields :: foreach (n :: Nat) -> n :~: 'Z -> Maybe Bool -> Fields n
simplifyFields _ proof payload = case proof of
    Refl -> payload

-- | Refine Fields through a successor equality carrying another retained index.
stepFields :: foreach (from :: Nat) -> foreach (to :: Nat) ->
              from :~: 'S to -> Either Bool (Fields to) -> Fields from
stepFields _ _ Refl payload = payload

#ifdef WRONG_FAMILY_ENDPOINT
wrongFields :: foreach (n :: Nat) -> n :~: 'Z -> Fields n
wrongFields _ Refl = Left True
#endif

#ifdef MISSING_FAMILY_MATCH
missingMatch :: foreach (from :: Nat) -> foreach (to :: Nat) ->
                from :~: to -> Fields from -> Fields to
missingMatch _ _ _ payload = payload
#endif
