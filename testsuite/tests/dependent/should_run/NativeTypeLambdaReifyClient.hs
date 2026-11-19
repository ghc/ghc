{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NativeTypeLambdaReifyClient where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaReify (CheckedOnce)

boolean :: CheckedOnce Bool 'True Char :~: (Proxy 'True, Char)
boolean = Refl

ordering :: CheckedOnce Ordering 'GT Int :~: (Proxy 'GT, Int)
ordering = Refl
