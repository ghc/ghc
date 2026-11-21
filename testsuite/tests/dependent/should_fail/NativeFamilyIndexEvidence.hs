{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module NativeFamilyIndexEvidence where

import Data.Proxy (Proxy(..))

type family Width (b :: Bool) :: Bool
data Hidden (b :: Bool) where
  Hide :: Proxy p -> Hidden (Width p)

retain :: forall b. foreach (x :: Hidden b) -> Proxy x
retain _ = Proxy

ok :: forall (p :: Bool). foreach (x :: Proxy p) -> Proxy ('Hide x)
ok x = retain @(Width p) (Hide x)

wrong :: forall (p :: Bool) (q :: Bool). (Width p ~ Width q)
      => foreach (x :: Proxy p) -> foreach (y :: Proxy q) -> Proxy ('Hide x)
wrong _ y = retain @(Width p) (Hide y)
