{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Equality
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- Definition of propositional equality @(:~:)@. Pattern-matching on a variable
-- of type @(a :~: b)@ produces a proof that @a ~ b@.
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------



module Data.Type.Equality (
  -- * The equality types
  (:~:)(..), type (~~),

  -- * Working with equality
  sym, trans, castWith, gcastWith, apply, inner, outer,

  -- * Inferring equality from other types
  TestEquality(..),

  -- * Boolean type-level equality
  type (==)
  ) where

import Data.Maybe
import GHC.Enum
import GHC.Show
import GHC.Read
import GHC.Base
import Data.Type.Bool

-- | Lifted, homogeneous equality. By lifted, we mean that it can be
-- bogus (deferred type error). By homogeneous, the two types @a@
-- and @b@ must have the same kind.
class a ~~ b => (a :: k) ~ (b :: k) | a -> b, b -> a
  -- See Note [The equality types story] in TysPrim
  -- NB: All this class does is to wrap its superclass, which is
  --     the "real", inhomogeneous equality; this is needed when
  --     we have a Given (a~b), and we want to prove things from it
  -- NB: Not exported, as (~) is magical syntax. That's also why there's
  -- no fixity.

instance {-# INCOHERENT #-} a ~~ b => a ~ b
  -- See Note [The equality types story] in TysPrim
  -- If we have a Wanted (t1 ~ t2), we want to immediately
  -- simplify it to (t1 ~~ t2) and solve that instead
  --
  -- INCOHERENT because we want to use this instance eagerly, even when
  -- the tyvars are partially unknown.

infix 4 :~:

-- | Propositional equality. If @a :~: b@ is inhabited by some terminating
-- value, then the type @a@ is the same as the type @b@. To use this equality
-- in practice, pattern-match on the @a :~: b@ to get out the @Refl@ constructor;
-- in the body of the pattern-match, the compiler knows that @a ~ b@.
--
-- @since 4.7.0.0
data a :~: b where  -- See Note [The equality types story] in TysPrim
  Refl :: a :~: a

-- with credit to Conal Elliott for 'ty', Erik Hesselink & Martijn van
-- Steenbergen for 'type-equality', Edward Kmett for 'eq', and Gabor Greif
-- for 'type-eq'

-- | Symmetry of equality
sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl

-- | Transitivity of equality
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

-- | Type-safe cast, using propositional equality
castWith :: (a :~: b) -> a -> b
castWith Refl x = x

-- | Generalized form of type-safe cast using propositional equality
gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

-- | Apply one equality to another, respectively
apply :: (f :~: g) -> (a :~: b) -> (f a :~: g b)
apply Refl Refl = Refl

-- | Extract equality of the arguments from an equality of a applied types
inner :: (f a :~: g b) -> (a :~: b)
inner Refl = Refl

-- | Extract equality of type constructors from an equality of applied types
outer :: (f a :~: g b) -> (f :~: g)
outer Refl = Refl

deriving instance Eq   (a :~: b)
deriving instance Show (a :~: b)
deriving instance Ord  (a :~: b)

instance a ~ b => Read (a :~: b) where
  readsPrec d = readParen (d > 10) (\r -> [(Refl, s) | ("Refl",s) <- lex r ])

instance a ~ b => Enum (a :~: b) where
  toEnum 0 = Refl
  toEnum _ = errorWithoutStackTrace "Data.Type.Equality.toEnum: bad argument"

  fromEnum Refl = 0

instance a ~ b => Bounded (a :~: b) where
  minBound = Refl
  maxBound = Refl

-- | This class contains types where you can learn the equality of two types
-- from information contained in /terms/. Typically, only singleton types should
-- inhabit this class.
class TestEquality f where
  -- | Conditionally prove the equality of @a@ and @b@.
  testEquality :: f a -> f b -> Maybe (a :~: b)

instance TestEquality ((:~:) a) where
  testEquality Refl Refl = Just Refl

-- | A type family to compute Boolean equality. Instances are provided
-- only for /open/ kinds, such as @*@ and function kinds. Instances are
-- also provided for datatypes exported from base. A poly-kinded instance
-- is /not/ provided, as a recursive definition for algebraic kinds is
-- generally more useful.
type family (a :: k) == (b :: k) :: Bool
infix 4 ==

{-
This comment explains more about why a poly-kinded instance for (==) is
not provided. To be concrete, here would be the poly-kinded instance:

type family EqPoly (a :: k) (b :: k) where
 EqPoly a a = True
 EqPoly a b = False
type instance (a :: k) == (b :: k) = EqPoly a b

Note that this overlaps with every other instance -- if this were defined,
it would be the only instance for (==).

Now, consider
data Nat = Zero | Succ Nat

Suppose I want
foo :: (Succ n == Succ m) ~ True => ((n == m) :~: True)
foo = Refl

This would not type-check with the poly-kinded instance. `Succ n == Succ m`
quickly becomes `EqPoly (Succ n) (Succ m)` but then is stuck. We don't know
enough about `n` and `m` to reduce further.

On the other hand, consider this:

type family EqNat (a :: Nat) (b :: Nat) where
 EqNat Zero     Zero     = True
 EqNat (Succ n) (Succ m) = EqNat n m
 EqNat n        m        = False
type instance (a :: Nat) == (b :: Nat) = EqNat a b

With this instance, `foo` type-checks fine. `Succ n == Succ m` becomes `EqNat
(Succ n) (Succ m)` which becomes `EqNat n m`. Thus, we can conclude `(n == m)
~ True` as desired.

So, the Nat-specific instance allows strictly more reductions, and is thus
preferable to the poly-kinded instance. But, if we introduce the poly-kinded
instance, we are barred from writing the Nat-specific instance, due to
overlap.

Even better than the current instance for * would be one that does this sort
of recursion for all datatypes, something like this:

type family EqStar (a :: *) (b :: *) where
 EqStar Bool Bool = True
 EqStar (a,b) (c,d) = a == c && b == d
 EqStar (Maybe a) (Maybe b) = a == b
 ...
 EqStar a b = False

The problem is the (...) is extensible -- we would want to add new cases for
all datatypes in scope. This is not currently possible for closed type
families.
-}

-- all of the following closed type families are local to this module
type family EqStar (a :: *) (b :: *) where
  EqStar a a = 'True
  EqStar a b = 'False

-- This looks dangerous, but it isn't. This allows == to be defined
-- over arbitrary type constructors.
type family EqArrow (a :: k1 -> k2) (b :: k1 -> k2) where
  EqArrow a a = 'True
  EqArrow a b = 'False

type family EqBool a b where
  EqBool 'True  'True  = 'True
  EqBool 'False 'False = 'True
  EqBool a      b      = 'False

type family EqOrdering a b where
  EqOrdering 'LT 'LT = 'True
  EqOrdering 'EQ 'EQ = 'True
  EqOrdering 'GT 'GT = 'True
  EqOrdering a   b   = 'False

type EqUnit (a :: ()) (b :: ()) = 'True

type family EqList a b where
  EqList '[]        '[]        = 'True
  EqList (h1 ': t1) (h2 ': t2) = (h1 == h2) && (t1 == t2)
  EqList a          b          = 'False

type family EqMaybe a b where
  EqMaybe 'Nothing   'Nothing  = 'True
  EqMaybe ('Just x) ('Just y)  = x == y
  EqMaybe a         b          = 'False

type family Eq2 a b where
  Eq2 '(a1, b1) '(a2, b2) = a1 == a2 && b1 == b2

type family Eq3 a b where
  Eq3 '(a1, b1, c1) '(a2, b2, c2) = a1 == a2 && b1 == b2 && c1 == c2

type family Eq4 a b where
  Eq4 '(a1, b1, c1, d1) '(a2, b2, c2, d2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

type family Eq5 a b where
  Eq5 '(a1, b1, c1, d1, e1) '(a2, b2, c2, d2, e2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2

type family Eq6 a b where
  Eq6 '(a1, b1, c1, d1, e1, f1) '(a2, b2, c2, d2, e2, f2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2

type family Eq7 a b where
  Eq7 '(a1, b1, c1, d1, e1, f1, g1) '(a2, b2, c2, d2, e2, f2, g2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2

type family Eq8 a b where
  Eq8 '(a1, b1, c1, d1, e1, f1, g1, h1) '(a2, b2, c2, d2, e2, f2, g2, h2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2

type family Eq9 a b where
  Eq9 '(a1, b1, c1, d1, e1, f1, g1, h1, i1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2

type family Eq10 a b where
  Eq10 '(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2

type family Eq11 a b where
  Eq11 '(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2

type family Eq12 a b where
  Eq12 '(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2

type family Eq13 a b where
  Eq13 '(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 && m1 == m2

type family Eq14 a b where
  Eq14 '(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 && m1 == m2 && n1 == n2

type family Eq15 a b where
  Eq15 '(a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) '(a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 && i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 && m1 == m2 && n1 == n2 && o1 == o2

-- these all look to be overlapping, but they are differentiated by their kinds
type instance a == b = EqStar a b
type instance a == b = EqArrow a b
type instance a == b = EqBool a b
type instance a == b = EqOrdering a b
type instance a == b = EqUnit a b
type instance a == b = EqList a b
type instance a == b = EqMaybe a b
type instance a == b = Eq2 a b
type instance a == b = Eq3 a b
type instance a == b = Eq4 a b
type instance a == b = Eq5 a b
type instance a == b = Eq6 a b
type instance a == b = Eq7 a b
type instance a == b = Eq8 a b
type instance a == b = Eq9 a b
type instance a == b = Eq10 a b
type instance a == b = Eq11 a b
type instance a == b = Eq12 a b
type instance a == b = Eq13 a b
type instance a == b = Eq14 a b
type instance a == b = Eq15 a b
