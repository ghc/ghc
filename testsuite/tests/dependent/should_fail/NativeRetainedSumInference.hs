{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module NativeRetainedSumInference where

import Data.Proxy (Proxy(..))
import GHC.Sigma (Sigma(Pair))

type family Hide (index :: Bool) :: Bool

type Visible (n :: Bool) (b :: Bool) = some (tag :: Bool) | Proxy '(n, b)
type Opaque (n :: Bool) (b :: Bool) = some (tag :: Bool) | Proxy '(Hide n, b)

visible :: forall n. foreach (b :: Bool) -> Visible n b
visible b = Pair b Proxy

opaque :: forall n. foreach (b :: Bool) -> Opaque n b
opaque b = Pair b Proxy

forwardVisible :: foreach (n :: Bool) -> foreach (b :: Bool) -> Visible n b
forwardVisible n b = visible b

forwardWritten :: foreach (n :: Bool) -> foreach (b :: Bool) -> Opaque n b
forwardWritten n b = opaque @n b

forwardOpaque :: foreach (n :: Bool) -> foreach (b :: Bool) -> Opaque n b
forwardOpaque n b = opaque b
