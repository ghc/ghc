{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedIndexedVecEmpty where

import Data.Type.Equality ((:~:)(Refl))
import qualified IndexedVec as V
import qualified NativeClosedIndexFailures as P

bad :: V.safeHead (P.empty 0) :~: 3
bad = Refl
