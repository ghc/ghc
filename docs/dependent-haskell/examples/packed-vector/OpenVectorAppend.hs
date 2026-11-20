{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module OpenVectorAppend (appendEmpty, appendEmptyAt, appendCons) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

-- | Appending an empty vector preserves the exact right operand.
appendEmpty :: forall a (n :: P.Nat) (xs :: P.Vec a n).
               P.appendVec 'P.VNil xs :~: xs
appendEmpty = Refl

-- | Check the same law for the vector supplied at runtime.
appendEmptyAt :: forall a (n :: P.Nat). foreach (xs :: P.Vec a n)
              -> P.appendVec 'P.VNil xs :~: xs
appendEmptyAt _ = Refl

-- | Appending a cons preserves its head and appends its tail to the right.
appendCons :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a)
                     (xs :: P.Vec a n) (ys :: P.Vec a m).
              Proxy xs -> Proxy ys ->
              P.appendVec ('P.VCons x xs) ys :~: 'P.VCons x (P.appendVec xs ys)
appendCons _ _ = Refl
