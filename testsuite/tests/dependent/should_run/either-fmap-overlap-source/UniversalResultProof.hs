{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module UniversalResultProof (MappedResult, fmapRight, originalRuntime) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import ResultFunctor

type MappedResult :: forall e a. (a %U -> Type) -> Result e a -> Result e Type
type MappedResult f x = translate f x

fmapRight :: forall e a (f :: a %U -> Type) (x :: a).
             Proxy f -> Proxy x ->
             translate f ('Succeeded x :: Result e a) :~: ('Succeeded (f x) :: Result e Type)
fmapRight _ _ = Refl

originalRuntime :: Result e a -> Result e Type
originalRuntime = translate (const (error "original payload was forced"))
{-# NOINLINE originalRuntime #-}
