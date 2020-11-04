{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications,
             TypeFamilies, PolyKinds, DataKinds, FlexibleInstances,
             MultiParamTypeClasses, FlexibleContexts, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module InstanceGivenOverlap2 where

import Data.Proxy

class P a
class Q a
class R a b

newtype Tagged (t :: k) a = Tagged a

type family F a
type instance F (Tagged @Bool t a) = [a]

instance P x => Q [x]
instance (x ~ y) => R y [x]

wob :: forall a b. (Q [b], R b a) => a -> Int
wob = undefined

it'sABoolNow :: forall (t :: Bool). Int
it'sABoolNow = undefined

class HasBoolKind t
instance k ~ Bool => HasBoolKind (t :: k)

it'sABoolLater :: forall t. HasBoolKind t => Int
it'sABoolLater = undefined

g :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g _ x = it'sABoolNow @t + wob x

g2 :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g2 _ x = wob x + it'sABoolNow @t

g3 :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g3 _ x = it'sABoolLater @t + wob x

g4 :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g4 _ x = wob x + it'sABoolLater @t
