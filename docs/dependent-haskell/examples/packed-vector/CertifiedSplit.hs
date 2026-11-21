{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module CertifiedSplit (SplitAtValue, splitAtValue, CheckedSplitAtValue, SplitAtWidth(..), checkedSplitAtValue) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import qualified PackedVecLibrary as P

-- | Store both pieces and the equality that relates them to the input.
type SplitAtValue a (n :: P.Nat) (m :: P.Nat) (v :: P.Vec a (P.Add n m)) =
    some (left :: P.Vec a n) | (some (right :: P.Vec a m) | P.appendVec left right :~: v)

-- | Keep the whole input as the suffix at zero.
splitZero :: forall a m. foreach (v :: P.Vec a m) -> SplitAtValue a 'P.Z m v
splitZero v = let proof :: P.appendVec ('P.VNil :: P.Vec a 'P.Z) v :~: v
                  proof = Refl
              in Pair P.VNil (Pair v proof)

-- | Split a vector and return evidence for its reconstruction.
splitAtValue :: forall a m. foreach (cut :: P.Nat) -> foreach (v :: P.Vec a (P.Add cut m))
             -> SplitAtValue a cut m v
splitAtValue P.Z v = splitZero @a @m v
splitAtValue (P.S k) v = splitSucc @a @m k v

-- | Open the induction result and put the original head on its prefix.
splitSucc :: forall a m. foreach (k :: P.Nat) -> foreach (v :: P.Vec a ('P.S (P.Add k m)))
          -> SplitAtValue a ('P.S k) m v
splitSucc k v = case v of
    P.VCons x xs -> case splitAtValue @a @m k xs of
        Pair left (Pair right premise) ->
            Pair (P.VCons x left) (Pair right (stepProof @a @k @m x left right xs premise))

-- | Lift the tail equality through a shared vector head.
stepProof :: forall a p q.
     foreach (x :: a) -> foreach (left :: P.Vec a p)
  -> foreach (right :: P.Vec a q) -> foreach (tailVec :: P.Vec a (P.Add p q))
  -> (P.appendVec left right :~: tailVec)
  -> P.appendVec ('P.VCons x left) right :~: 'P.VCons x tailVec
stepProof _ _ _ _ Refl = Refl

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
        Just (Pair suffix (SplitAtWidth @a @cut @suffix (splitAtValue @a @suffix cut v)))
