{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeClosedNestedDependencies where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Inner
import qualified NativeOpenRetryLib as Outer

answer :: Outer.total (Inner.reverseNaturals '[2, 3, 5]) :~: 10
answer = Refl
