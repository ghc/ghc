{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeApplications #-}
module NativeIndexedOperandWire (tagCons) where

import Data.Proxy (Proxy(..))
import qualified PackedVecLibrary as P

{-# NOINLINE tag #-}
tag :: forall n. foreach (value :: P.Vec Bool n) -> (P.Vec Bool n, Proxy value)
tag value = (value, Proxy)

{-# INLINE tagCons #-}
tagCons :: forall (n :: P.Nat). foreach (x :: Bool) -> foreach (xs :: P.Vec Bool n)
        -> (P.Vec Bool ('P.S n), Proxy ('P.VCons x xs))
tagCons x xs = tag @('P.S n) (P.VCons x xs)
