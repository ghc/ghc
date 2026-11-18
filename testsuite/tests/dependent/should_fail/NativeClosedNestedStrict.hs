{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedNestedStrict where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Lib

bad :: Lib.empty (Prelude.reverse (Lib.badElement '[1])) :~: '[]
bad = Refl
