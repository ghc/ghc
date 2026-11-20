{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, PolyKinds, ScopedTypeVariables, StandaloneKindSignatures, TypeApplications #-}
module NativeIndexedConsProof (Seen(..), see, inlineNil, inlineCons, writtenNil, writtenCons) where

import Data.Kind (Type)
import qualified PackedVecLibrary as P

type Seen :: forall (n :: P.Nat). P.Vec Bool n -> Type
data Seen input where
    SeenNil :: Seen @'P.Z 'P.VNil
    SeenCons :: forall (n :: P.Nat) (x :: Bool) (xs :: P.Vec Bool n).
                Seen @('P.S n) ('P.VCons x xs)

seenCons :: forall n. foreach (x :: Bool) -> foreach (xs :: P.Vec Bool n) ->
            Seen ('P.VCons x xs)
seenCons x xs = SeenCons @n @x @xs

{-# INLINE see #-}
see :: forall (n :: P.Nat). foreach (input :: P.Vec Bool n) -> Seen input
see input = case input of
    P.VNil -> SeenNil
    P.VCons x xs -> seenCons x xs

{-# NOINLINE seeOperand #-}
seeOperand :: forall n. foreach (input :: P.Vec Bool n) -> Seen input
seeOperand input = see input

{-# INLINE inspectNil #-}
inspectNil :: foreach (input :: P.Vec Bool 'P.Z) -> Seen input
inspectNil input = case input of P.VNil -> SeenNil

inlineNil :: Seen 'P.VNil
inlineNil = inspectNil P.VNil

inlineCons :: forall n. foreach (x :: Bool) -> foreach (xs :: P.Vec Bool n) ->
              Seen ('P.VCons x xs)
inlineCons x xs = see @('P.S n) (P.VCons x xs)

writtenNil :: Seen 'P.VNil
writtenNil = seeOperand @'P.Z P.VNil

writtenCons :: forall n. foreach (x :: Bool) -> foreach (xs :: P.Vec Bool n) ->
               Seen ('P.VCons x xs)
writtenCons x xs = seeOperand @('P.S n) (P.VCons x xs)
