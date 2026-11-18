{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedSumAmbiguous where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedSumLib

ambiguous :: NativeClosedSumLib.ambiguous '[2, 3, 5] :~: 0
ambiguous = Refl
