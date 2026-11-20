{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module CertifiedSplit (SplitAtValue, splitAtValue) where

import Data.Type.Equality ((:~~:)(HRefl))
import GHC.Sigma (Sigma(Pair))
import qualified PackedVecLibrary as P

-- | Store both pieces and the equality that relates them to the input.
type SplitAtValue a (n :: P.Nat) (m :: P.Nat) (total :: P.Nat) (v :: P.Vec a total) =
    some (left :: P.Vec a n) | (some (right :: P.Vec a m) | P.appendVec left right :~~: v)

-- | Keep the whole input as the suffix at zero.
splitZero :: forall a m. foreach (v :: P.Vec a m) -> SplitAtValue a 'P.Z m m v
splitZero v = let proof :: P.appendVec ('P.VNil :: P.Vec a 'P.Z) v :~~: v
                  proof = HRefl
              in Pair P.VNil (Pair v proof)

-- | Split a vector and return evidence for its reconstruction.
splitAtValue :: forall a m. foreach (cut :: P.Nat) -> foreach (v :: P.Vec a (P.Add cut m))
             -> SplitAtValue a cut m (P.Add cut m) v
splitAtValue P.Z v = splitZero @a @m v
splitAtValue (P.S k) v = splitSucc @a @m k v

-- | Open the induction result and put the original head on its prefix.
splitSucc :: forall a m. foreach (k :: P.Nat) -> foreach (v :: P.Vec a ('P.S (P.Add k m)))
          -> SplitAtValue a ('P.S k) m ('P.S (P.Add k m)) v
splitSucc k v = case v of
    P.VCons x xs -> case splitAtValue @a @m k xs of
        Pair left (Pair right premise) ->
            Pair (P.VCons x left) (Pair right (stepProof @a @k @m x left right xs premise))

-- | Lift the tail equality through a shared vector head.
stepProof :: forall a p q.
     foreach (x :: a) -> foreach (left :: P.Vec a p)
  -> foreach (right :: P.Vec a q) -> foreach (tailVec :: P.Vec a (P.Add p q))
  -> (P.appendVec left right :~~: tailVec)
  -> P.appendVec ('P.VCons x left) right :~~: 'P.VCons x tailVec
stepProof _ _ _ _ HRefl = HRefl
