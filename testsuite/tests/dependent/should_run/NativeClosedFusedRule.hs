{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedFusedRule where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedFusedRuleLib as P

proof :: P.first (P.make 5) :~: 7
proof = Refl
