{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds #-}
module NestedSignature (PackedPair, PackedKind) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Vector

type PackedPair a = exists (n :: Nat). exists (m :: Nat). (Vec n a, Vec m a)
type PackedKind = exists (k :: Type). exists (v :: k). Proxy v
