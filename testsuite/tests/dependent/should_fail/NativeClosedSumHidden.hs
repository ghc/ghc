{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedSumHidden where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedSumLib

hidden :: NativeClosedSumLib.hiddenRequirement '[2, 3, 5] :~: 0
hidden = Refl
