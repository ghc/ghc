{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module NativeClosedNestedMissing where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedListsLib as Inner
import qualified NativeClosedSumLib as Outer

answer :: Outer.unavailable (Inner.badElement '[1]) :~: 0
answer = Refl
