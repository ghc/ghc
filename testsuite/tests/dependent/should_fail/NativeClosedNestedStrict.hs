{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedNestedStrict where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Lib

-- Both const and empty ignore a value. The nested call still checks its result.
bad :: Lib.empty (Prelude.const (Prelude.reverse '[1]) (Lib.badElement '[1])) :~: '[]
bad = Refl
