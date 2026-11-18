{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell, TypeOperators #-}
module NativeClosedSumLocal where

import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
import qualified NativeClosedSumLib
import NativeClosedSumLib (Local (..))

instance Local Natural where
    localWitness = id

$(pure [])

local :: NativeClosedSumLib.local '[2, 3, 5] :~: 0
local = Refl
