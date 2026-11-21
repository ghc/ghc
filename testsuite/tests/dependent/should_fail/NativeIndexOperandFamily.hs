{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module NativeIndexOperandFamily where

import Data.Proxy (Proxy(..))

type family Width (b :: Bool) :: Bool
data Hidden (b :: Bool) where
  Hide :: Proxy p -> Hidden (Width p)

retain :: forall b. foreach (x :: Hidden b) -> Proxy x
retain _ = Proxy

same :: forall p. foreach (x :: Proxy p) -> Proxy ('Hide x)
same x = retain @(Width p) (Hide x)

wrong :: forall p. foreach (x :: Proxy p) -> foreach (y :: Proxy p) -> Proxy ('Hide x)
wrong x y = retain @(Width p) (Hide y)
