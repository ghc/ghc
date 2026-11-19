{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module NativeClosedSaturatedMissing where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Inner
import qualified NativeClosedSumLib as Outer

answer :: Outer.unavailablePair 'x' (Inner.badElement '[1]) :~: 0
answer = Refl
