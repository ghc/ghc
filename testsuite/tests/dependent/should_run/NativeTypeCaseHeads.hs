{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitNamespaces, KindSignatures, RankNTypes, Modifiers #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module NativeTypeCaseHeads where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (M)
import qualified NativeTypeCaseHeadProvider as Only (type C, type T)
import NativeTypeCaseHeadProvider (D(..))

type Choose a = case a of { Bool -> Int; Ordering -> Char }
bool :: Choose Bool :~: Int
bool = Refl
ordering :: Choose Ordering :~: Char
ordering = Refl

data T = T
type Same (a :: T) = case a of { T -> Int }
type Explicit (a :: T) = case a of { 'T -> Char }
type TypeOnly a = case a of { Only.T -> Bool }
type BoolAlias = Bool
type ArrowAlias = Int %M -> Bool
type ArrowHead a = case a of { ArrowAlias -> Ordering }
arrowHead :: ArrowHead ArrowAlias :~: Ordering
arrowHead = Refl
type Alias a = case a of { BoolAlias -> Int }
type ClassOnly a = case a of { Only.C x -> x }
type DataFirst (a :: D) = case a of { C -> Bool }

collisions ::
  ( Same 'T :~: Int, Explicit 'T :~: Char, TypeOnly Only.T :~: Bool
  , ClassOnly (Only.C Int) :~: Int, DataFirst 'C :~: Bool, Alias Bool :~: Int )
collisions = (Refl, Refl, Refl, Refl, Refl, Refl)

ordinary :: T -> Bool
ordinary value = case value of { T -> True }

type family Ordinary a where
  Ordinary a = Bool

ordinaryFamily :: forall a. Ordinary a :~: Bool
ordinaryFamily = Refl
