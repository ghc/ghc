{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeClosedComputedSiblingsMismatch where

import Data.Type.Equality ((:~:)(Refl))

bad :: Prelude.max (Prelude.sum '[1, 2]) (Prelude.head '[ 'x']) :~: 3
bad = Refl
