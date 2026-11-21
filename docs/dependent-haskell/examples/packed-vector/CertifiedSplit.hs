{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module CertifiedSplit
    ( SplitAtValue, splitAtValue
    , CheckedSplitAtValue, SplitAtWidth(..), checkedSplitAtValue
    , CheckedSplitBoth, SplitBothAtWidth(..), checkedSplitBoth
    ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import qualified PackedVecLibrary as P

-- | Store both pieces and the equality that relates them to the input.
type SplitAtValue a (n :: P.Nat) (m :: P.Nat) (v :: P.Vec a (P.Add n m)) =
    some (left :: P.Vec a n) | (some (right :: P.Vec a m) | P.appendVec left right :~: v)

-- | Split a vector and return evidence for its reconstruction.
splitAtValue :: forall a m. foreach (cut :: P.Nat) -> foreach (v :: P.Vec a (P.Add cut m))
             -> SplitAtValue a cut m v
splitAtValue P.Z v = Pair P.VNil (Pair v Refl)
splitAtValue (P.S k) (P.VCons x xs) =
    case splitAtValue @a @m k xs of
        Pair left (Pair right premise) ->
            Pair (P.VCons x left) (Pair right (case premise of Refl -> Refl))

-- | Matching this constructor identifies the total with the sum of the two widths.
type SplitAtWidth :: forall a -> P.Nat -> P.Nat -> forall total -> P.Vec a total -> Type
data SplitAtWidth a cut suffix total v where
    SplitAtWidth :: forall a cut suffix (v :: P.Vec a (P.Add cut suffix)).
               SplitAtValue a cut suffix v -> SplitAtWidth a cut suffix (P.Add cut suffix) v

-- | Store the runtime suffix and a split at its validated width.
type CheckedSplitAtValue a (cut :: P.Nat) (total :: P.Nat) (v :: P.Vec a total) =
    some (suffix :: P.Nat) | SplitAtWidth a cut suffix total v

-- | Check both lengths before inspecting the vector.
checkedSplitAtValue :: forall a. foreach (cut :: P.Nat) -> foreach (total :: P.Nat)
                    -> foreach (v :: P.Vec a total) -> Maybe (CheckedSplitAtValue a cut total v)
checkedSplitAtValue cut total v = case P.cutRemainder cut total of
    Nothing -> Nothing
    Just (Pair suffix Refl) ->
        Just (Pair suffix (SplitAtWidth (splitAtValue cut v)))

-- | Store the reconstruction proof for each of two vectors at the same width.
type SplitBothAtWidth :: forall a -> forall b -> P.Nat -> P.Nat -> forall total -> P.Vec a total -> P.Vec b total -> Type
data SplitBothAtWidth a b cut suffix total xs ys where
    SplitBothAtWidth :: forall a b cut suffix
        (xs :: P.Vec a (P.Add cut suffix)) (ys :: P.Vec b (P.Add cut suffix)).
        SplitAtValue a cut suffix xs -> SplitAtValue b cut suffix ys ->
        SplitBothAtWidth a b cut suffix (P.Add cut suffix) xs ys

-- | Retain the common suffix length selected by the bounds check.
type CheckedSplitBoth a b (cut :: P.Nat) (total :: P.Nat) (xs :: P.Vec a total) (ys :: P.Vec b total) =
    some (suffix :: P.Nat) | SplitBothAtWidth a b cut suffix total xs ys

-- | Split vectors of independent element types at the same checked cut.
checkedSplitBoth :: forall a b. foreach (cut :: P.Nat) -> foreach (total :: P.Nat)
                 -> foreach (xs :: P.Vec a total) -> foreach (ys :: P.Vec b total)
                 -> Maybe (CheckedSplitBoth a b cut total xs ys)
checkedSplitBoth cut total xs ys = case P.cutRemainder cut total of
    Nothing -> Nothing
    Just (Pair suffix Refl) ->
        Just (Pair suffix (SplitBothAtWidth
            (splitAtValue cut xs) (splitAtValue cut ys)))
