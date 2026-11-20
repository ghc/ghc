{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module OpenVectorAppend (appendEmpty, appendEmptyAt) where

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
