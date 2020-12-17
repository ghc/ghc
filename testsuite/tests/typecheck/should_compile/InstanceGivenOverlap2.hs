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

-- This test 'g' used to pass, prior to my fix of #18929
-- But it is /extremely/ delicate, relying on inhibiting constraint
-- solving because of overlapping Givens (couldMatchLater)
-- We are content for it to fail now; see #18929
-- If it starts to pass in some later universe, that's fine too
g :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g _ x = it'sABoolNow @t + wob x

{- Notes about 'g'
   [G] Q (F (Tagged @Bool t a))
   [W] Q [beta]    ==> Q [a]  ==>{instance}  P a
   [W] R beta [a]  ==>{instance}   beta ~ a
-}

{-  Commenting out these because they all fail
    in the same way as 'g'

g2 :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g2 _ x = wob x + it'sABoolNow @t

g3 :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g3 _ x = it'sABoolLater @t + wob x

g4 :: forall t a. Q (F (Tagged t a)) => Proxy t -> [a] -> _
g4 _ x = wob x + it'sABoolLater @t
-}
