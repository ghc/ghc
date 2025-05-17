{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeAbstractions          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module T6018 where

import GHC.TypeLits (Nat)

{-
barapp2 :: Int
barapp2 = bar 1

bar :: Bar a -> Bar a
bar x = x

type family Bar a = r | r -> a where
    Bar Int  = Bool
    Bar Bool = Int
    Bar Bool = Char

type family F a b c = (result :: k) | result -> a b c

type family FClosed a b c = result | result -> a b c where
    FClosed Int  Char Bool = Bool
    FClosed Char Bool Int  = Int
    FClosed Bool Int  Char = Char
-}
{-
g6_use :: [Char]
g6_use = g6_id "foo"

g6_id :: G6 a -> G6 a
g6_id x = x

type family G6 a = r | r -> a
type instance G6 [a]  = [Gi a]
type family Gi a = r | r -> a
type instance Gi Int = Char
-}

import Data.Kind (Type)
import T6018a -- defines G, identical to F

type family F a b c = (result :: k) | result -> a b c
type instance F @Type Int  Char Bool = Bool
type instance F @Type Char Bool Int  = Int
type instance F @Type Bool Int  Char = Char


type instance G Bool Int  Char = Char

type family I (a :: k) b (c :: k) = r | r -> a b
type instance I Int  Char Bool = Bool
type instance I Int  Char Int  = Bool
type instance I Bool Int  Int  = Int

-- this is injective - a type variable introduced in the LHS is not mentioned on
-- RHS but we don't claim injectivity in that argument.
type family J a (b :: k) = r | r -> a
type instance J Int b = Char

type MaybeSyn a = Maybe a
newtype MaybeNew a = MaybeNew (Maybe a)

-- make sure we look through type synonyms...
type family K a = r | r -> a
type instance K a = MaybeSyn a

-- .. but not newtypes
type family M a = r | r -> a
type instance M (Maybe a)    = MaybeSyn a
type instance M (MaybeNew a) = MaybeNew a

-- Closed type families

-- these are simple conversions from open type families. They should behave the
-- same
type family FClosed a b c = result | result -> a b c where
    FClosed Int  Char Bool = Bool
    FClosed Char Bool Int  = Int
    FClosed Bool Int  Char = Char

type family IClosed (a :: Type) (b :: Type) (c :: Type) = r | r -> a b where
    IClosed Int  Char Bool = Bool
    IClosed Int  Char Int  = Bool
    IClosed Bool Int  Int  = Int

type family JClosed a (b :: k) = r | r -> a where
    JClosed Int b = Char

type family KClosed a = r | r -> a where
    KClosed a = MaybeSyn a

-- Here the last equation might return both Int and Char but we have to
-- recognize that it is not possible due to equation overlap
type family Bak a = r | r -> a where
     Bak Int  = Char
     Bak Char = Int
     Bak a    = a

-- This is similar, except that the last equation contains concrete type.  Since
-- it is overlapped it should be dropped with a warning
type family Foo a = r | r -> a where
    Foo Int  = Bool
    Foo Bool = Int
    Foo Bool = Bool

-- this one was tricky in the early implementation of injectivity.  Now it is
-- identical to the above but we still keep it as a regression test.
type family Bar a = r | r -> a where
    Bar Int  = Bool
    Bar Bool = Int
    Bar Bool = Char

-- Now let's use declared type families. All the below definitions should work

-- No ambiguity for any of the arguments - all are injective
f :: F a b c -> F a b c
f x = x

-- From 1st instance of F: a ~ Int, b ~ Char, c ~ Bool
fapp :: Bool
fapp = f True

-- now the closed variant of F
fc :: FClosed a b c -> FClosed a b c
fc x = x

fcapp :: Bool
fcapp = fc True

-- The last argument is not injective so it must be instantiated
i :: I a b Int -> I a b Int
i x = x

-- From 1st instance of I: a ~ Int, b ~ Char
iapp :: Bool
iapp = i True

-- again, closed variant of I
ic :: IClosed a b Int -> IClosed a b Int
ic x = x

icapp :: Bool
icapp = ic True

-- Now we have to test weird closed type families:
bak :: Bak a -> Bak a
bak x = x

bakapp1 :: Char
bakapp1 = bak 'c'

bakapp2 :: Double
bakapp2 = bak 1.0

bakapp3 :: ()
bakapp3 = bak ()

foo :: Foo a -> Foo a
foo x = x

fooapp1 :: Bool
fooapp1 = foo True

bar :: Bar a -> Bar a
bar x = x

barapp1 :: Bool
barapp1 = bar True

barapp2 :: Int
barapp2 = bar 1

-- Declarations below test more liberal RHSs of injectivity annotations:
-- permitting variables to appear in different order than the one in which they
-- were declared.
type family H a b = r | r -> b a
type family Hc a b = r | r -> b a where
  Hc a b = a b
class Hcl a b where
  type Ht a b = r | r -> b a

-- repeated tyvars in the RHS of injectivity annotation: no warnings or errors
-- (consistent with behaviour for functional dependencies)
type family Jx a b = r | r -> a a
type family Jcx a b = r | r -> a a where
  Jcx a b = a b
class Jcl a b where
  type Jt a b = r | r -> a a

type family Kx a b = r | r -> a b b
type family Kcx a b = r | r -> a b b where
  Kcx a b = a b
class Kcl a b where
  type Kt a b = r | r -> a b b

-- Declaring kind injectivity. Here we only claim that knowing the RHS
-- determines the LHS kind but not the type.
type L :: k1 -> k2
type family L @k1 a = r | r -> k1 where
    L @_ @Type 'True  = Int
    L @_ @Type 'False = Int
    L @_ @Nat Maybe   = 3
    L @_ @Nat IO      = 3

data KProxy (a :: Type) = KProxy
type family KP (kproxy :: KProxy k) = r | r -> k
type instance KP ('KProxy :: KProxy Bool) = Int
type instance KP ('KProxy :: KProxy Type) = Char

kproxy_id :: KP ('KProxy :: KProxy k) -> KP ('KProxy :: KProxy k)
kproxy_id x = x

kproxy_id_use = kproxy_id 'a'

-- Now test some awkward cases from The Injectivity Paper.  All should be
-- accepted.
type family Gx a
type family Hx a
type family Gi a = r | r -> a
type instance Gi Int = Char
type family Hi a = r | r -> a

type family F2 a = r | r -> a
type instance F2 [a]       = [Gi a]
type instance F2 (Maybe a) = Hi a -> Int

type family F4 a = r | r -> a
type instance F4 [a]       = (Gx a, a,   a,    a)
type instance F4 (Maybe a) = (Hx a, a, Int, Bool)

type family G2 a b = r | r -> a b
type instance G2 a    Bool = (a, a)
type instance G2 Bool b    = (b, Bool)

type family G6 a = r | r -> a
type instance G6 [a]  = [Gi a]
type instance G6 Bool = Int

g6_id :: G6 a -> G6 a
g6_id x = x

g6_use :: [Char]
g6_use = g6_id "foo"

-- A sole exception to "bare variables in the RHS" rule
type family Id (a :: k) = (result :: k) | result -> a
type instance Id a = a

-- This makes sure that over-saturated type family applications at the top-level
-- are accepted.
type family IdProxy (a :: k) b = r | r -> a
type instance IdProxy (a :: Type -> Type) b = (Id a) b

-- make sure we look through type synonyms properly
type IdSyn a = Id a
type family IdProxySyn (a :: k) b = r | r -> a
type instance IdProxySyn (a :: Type -> Type) b = (IdSyn a) b

-- this has bare variable in the RHS but all LHS variables are also bare so it
-- should be accepted
type family Fa (a :: k) (b :: k) = (r :: k2) | r -> k
type instance Fa @k @k a b = a

-- Taken from #9587. This exposed a bug in the solver.
type family Arr (repr :: Type -> Type) (a :: Type) (b :: Type)
  = (r :: Type) | r -> repr a b

class ESymantics repr where
    int :: Int  -> repr Int
    add :: repr Int  -> repr Int -> repr Int

    lam :: (repr a -> repr b) -> repr (Arr repr a b)
    app :: repr (Arr repr a b) -> repr a -> repr b

te4 = let c3 = lam (\f -> lam (\x -> f `app` (f `app` (f `app` x))))
      in (c3 `app` (lam (\x -> x `add` int 14))) `app` (int 0)

-- This used to fail during development
class Manifold' a where
    type Base  a = r | r -> a
    project :: a -> Base a
    unproject :: Base a -> a

id' :: forall a. ( Manifold' a ) => Base a -> Base a
id' = project . unproject
