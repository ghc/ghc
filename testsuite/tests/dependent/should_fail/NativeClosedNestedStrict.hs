{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedNestedStrict where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Lib

-- Summing the chosen argument demands its exceptional second element.
bad :: Prelude.sum (Prelude.const (Lib.badElement '[1]) (Prelude.reverse '[1])) :~: 0
bad = Refl
