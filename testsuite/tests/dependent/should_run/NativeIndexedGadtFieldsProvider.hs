{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeApplications #-}
module NativeIndexedGadtFieldsProvider where

import Data.Kind (Type)
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
