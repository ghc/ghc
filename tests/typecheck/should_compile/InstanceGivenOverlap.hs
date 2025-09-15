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

g :: forall c. Q [c] => [c] -> Int
g x = wob x   -- Instantiate wob @[c] @beta

{- Constraint solving for g

[G] Q [c]
[W] Q [F [c] beta]   -- Do NOT fire Q [x] instance
[W] R beta [c]
--> instance for R
[G] Q [c]
[W] Q [F [c] beta]
[W] beta ~ c
-->
[G] Q [c]
[W] Q [F [c] c]
--> Eqn for F
[G] Q [c]
[W] Q [c]
--> done

c ~ F [c] beta

-}
