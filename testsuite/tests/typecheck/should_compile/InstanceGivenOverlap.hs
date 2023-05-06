{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, FlexibleContexts, AllowAmbiguousTypes #-}

module InstanceGivenOverlap where

-- See Note [Instance and Given overlap] in GHC.Tc.Solver.Dict.
-- This tests the Note when the Wanted contains a type family.

class P a
class Q a
class R a b

instance P x => Q [x]
instance (x ~ y) => R y [x]

type family F a b where
  F [a] a = a

wob :: forall a b. (Q [F a b], R b a) => a -> Int
wob = undefined

g :: forall a. Q [a] => [a] -> Int
g x = wob x
