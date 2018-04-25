{-# LANGUAGE TypeFamilyDependencies, DataKinds, UndecidableInstances, PolyKinds,
             MultiParamTypeClasses, FlexibleInstances #-}

module T6018fail where

import T6018Afail -- defines G, identical to F
import T6018Cfail -- imports H from T6018Bfail, defines some equations for H
import T6018Dfail -- imports H from T6018Bfail, defines conflicting eqns

type family F a b c = (result :: *) | result -> a b c
type instance F Int  Char Bool = Bool
type instance F Char Bool Int  = Int
type instance F Bool Int  Char = Int

type instance G Bool Int  Char = Int

type family I a b c = r | r -> a b
type instance I Int  Char Bool = Bool
type instance I Int  Int  Int  = Bool
type instance I Bool Int  Int  = Int

-- Id is injective...
type family Id a = result | result -> a
type instance Id a = a

-- ...but despite that we disallow a call to Id
type family IdProxy a = r | r -> a
type instance IdProxy a = Id a

data N = Z | S N

-- P is not injective, although the user declares otherwise. This
-- should be rejected on the grounds of calling a type family in the
-- RHS.
type family P (a :: N) (b :: N) = (r :: N) | r -> a b
type instance P  Z    m = m
type instance P (S n) m = S (P n m)

-- this is not injective - not all injective type variables mentioned
-- on LHS are mentioned on RHS
type family J a b c = r | r -> a b
type instance J Int b c = Char

-- same as above, but tyvar is now nested inside a tycon
type family K (a :: N) (b :: N) = (r :: N) | r -> a b
type instance K (S n) m = S m

-- Make sure we look through type synonyms to catch errors
type MaybeSyn a = Id a
type family L a = r | r -> a
type instance L a = MaybeSyn a

-- These should fail because given the RHS kind
-- there is no way to determine LHS kind
class PolyKindVarsC a where
    type PolyKindVarsF a = (r :: k) | r -> a

instance PolyKindVarsC '[] where
    type PolyKindVarsF '[] = '[]

type family PolyKindVars (a :: k0) = (r :: k1) | r -> a
type instance PolyKindVars '[] = '[]

-- This should fail because there is no way to determine k from the RHS
type family Fc (a :: k) (b :: k) = r | r -> k
type instance Fc a b = Int

-- This should fail because there is no way to determine a, b and k from the RHS
type family Gc (a :: k) (b :: k) = r | r -> a b
type instance Gc a b = Int

-- fails because injectivity is not compositional in this case
type family F1 a = r | r -> a
type instance F1 [a]       = Maybe (GF1 a)
type instance F1 (Maybe a) = Maybe (GF2 a)

type family GF1 a = r | r -> a
type instance GF1 Int = Bool

type family GF2 a = r | r -> a
type instance GF2 Int = Bool

type family HF1 a
type instance HF1 Bool = Bool

type family W1 a = r | r -> a
type instance W1 [a] = a

type family W2 a = r | r -> a
type instance W2 [a] = W2 a

-- not injective because of infinite types
type family Z1 a = r | r -> a
type instance Z1 [a]       = (a, a)
type instance Z1 (Maybe b) = (b, [b])

type family G1 a = r | r -> a
type instance G1 [a]       = [a]
type instance G1 (Maybe b) = [(b, b)]

type family G3 a b = r | r -> b
type instance G3 a Int  = (a, Int)
type instance G3 a Bool = (Bool, a)

type family G4 a b = r | r -> a b
type instance G4 a b = [a]

type family G5 a = r | r -> a
type instance G5 [a] = [GF1 a] -- GF1 injective
type instance G5 Int = [Bool]

type family G6 a = r | r -> a
type instance G6 [a]  = [HF1 a] -- HF1 not injective
type instance G6 Bool = Int

type family G7a a b (c :: k) = r | r -> a b
type family G7 a b (c :: k) = r | r -> a b c
type instance G7 a b c = [G7a a b c]

class C a b where
    type FC a (b :: *) = r | r -> b
    type instance FC a b = b

instance C Int Char where
    type FC Int Char = Bool

-- this should fail because the default instance conflicts with one of the
-- earlier instances
instance C Int Bool {- where
    type FC Int Bool = Bool-}

-- and this should fail because it violates "bare variable in the RHS"
-- restriction
instance C Char a
