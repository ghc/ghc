{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}

module NativeClosedCompoundListBottom where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedCompoundListsLib as P

bad :: P.ignoreCompound (P.bottomElement 1) :~: 7
bad = Refl
