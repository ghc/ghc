{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedComputedEmpty where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Lib

wrong :: Prelude.and (Lib.empty '[]) :~: 'True
wrong = Refl
