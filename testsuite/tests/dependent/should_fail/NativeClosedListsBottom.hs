{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedListsBottom where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Lib

bad :: Lib.badElement '[1] :~: '[1, 2]
bad = Refl
