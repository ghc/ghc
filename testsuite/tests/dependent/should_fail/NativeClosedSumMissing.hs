{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module NativeClosedSumMissing where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedSumLib

missing :: NativeClosedSumLib.unavailable '[2, 3, 5] :~: 0
missing = Refl
