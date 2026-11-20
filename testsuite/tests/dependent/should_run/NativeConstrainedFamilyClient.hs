{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module NativeConstrainedFamilyClient where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Allowed, Collects(..), Defaulted(..), Pair(..))

assumed :: Collects c => Proxy c -> Proxy (Elem c)
assumed _ = Proxy

resolved :: Allowed a => Elem [a] :~: a
resolved = Refl

ground :: Elem [Bool] :~: Bool
ground = Refl

defaulted :: Def Int :~: Char
defaulted = Refl

permuted :: Swapped Int Bool :~: Char
permuted = Refl
