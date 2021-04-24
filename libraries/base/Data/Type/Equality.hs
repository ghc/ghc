{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy              #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Equality
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- Definition of propositional equality @(':~:')@. Pattern-matching on a variable
-- of type @(a ':~:' b)@ produces a proof that @a '~' b@.
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------



module Data.Type.Equality (
  -- * The equality types
  type (~),
  type (~~),
  (:~:)(..),
  (:~~:)(..),

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

infix 4 :~:, :~~:

-- | Propositional equality. If @a :~: b@ is inhabited by some terminating
-- value, then the type @a@ is the same as the type @b@. To use this equality
-- in practice, pattern-match on the @a :~: b@ to get out the @Refl@ constructor;
-- in the body of the pattern-match, the compiler knows that @a ~ b@.
--
-- @since 4.7.0.0
data a :~: b where  -- See Note [The equality types story] in GHC.Builtin.Types.Prim
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

-- | Extract equality of the arguments from an equality of applied types
inner :: (f a :~: g b) -> (a :~: b)
inner Refl = Refl

-- | Extract equality of type constructors from an equality of applied types
outer :: (f a :~: g b) -> (f :~: g)
outer Refl = Refl

-- | @since 4.7.0.0
deriving instance Eq   (a :~: b)

-- | @since 4.7.0.0
deriving instance Show (a :~: b)

-- | @since 4.7.0.0
deriving instance Ord  (a :~: b)

-- | @since 4.7.0.0
deriving instance a ~ b => Read (a :~: b)

-- | @since 4.7.0.0
instance a ~ b => Enum (a :~: b) where
  toEnum 0 = Refl
  toEnum _ = errorWithoutStackTrace "Data.Type.Equality.toEnum: bad argument"

  fromEnum Refl = 0

-- | @since 4.7.0.0
deriving instance a ~ b => Bounded (a :~: b)

-- | Kind heterogeneous propositional equality. Like ':~:', @a :~~: b@ is
-- inhabited by a terminating value if and only if @a@ is the same type as @b@.
--
-- @since 4.10.0.0
type (:~~:) :: k1 -> k2 -> Type
data a :~~: b where
   HRefl :: a :~~: a

-- | @since 4.10.0.0
deriving instance Eq   (a :~~: b)
-- | @since 4.10.0.0
deriving instance Show (a :~~: b)
-- | @since 4.10.0.0
deriving instance Ord  (a :~~: b)

-- | @since 4.10.0.0
deriving instance a ~~ b => Read (a :~~: b)

-- | @since 4.10.0.0
instance a ~~ b => Enum (a :~~: b) where
  toEnum 0 = HRefl
  toEnum _ = errorWithoutStackTrace "Data.Type.Equality.toEnum: bad argument"

  fromEnum HRefl = 0

-- | @since 4.10.0.0
deriving instance a ~~ b => Bounded (a :~~: b)

-- | This class contains types where you can learn the equality of two types
-- from information contained in /terms/. Typically, only singleton types should
-- inhabit this class.
class TestEquality f where
  -- | Conditionally prove the equality of @a@ and @b@.
  testEquality :: f a -> f b -> Maybe (a :~: b)

-- | @since 4.7.0.0
instance TestEquality ((:~:) a) where
  testEquality Refl Refl = Just Refl

-- | @since 4.10.0.0
instance TestEquality ((:~~:) a) where
  testEquality HRefl HRefl = Just Refl

infix 4 ==

-- | A type family to compute Boolean equality.
type (==) :: k -> k -> Bool
type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False

-- The idea here is to recognize equality of *applications* using
-- the first case, and of *constructors* using the second and third
-- ones. It would be wonderful if GHC recognized that the
-- first and second cases are compatible, which would allow us to
-- prove
--
-- a ~ b => a == b
--
-- but it (understandably) does not.
--
-- It is absolutely critical that the three cases occur in precisely
-- this order. In particular, if
--
-- a == a = 'True
--
-- came first, then the type application case would only be reached
-- (uselessly) when GHC discovered that the types were not equal.
--
-- One might reasonably ask what's wrong with a simpler version:
--
-- type family (a :: k) == (b :: k) where
--  a == a = True
--  a == b = False
--
-- Consider
-- data Nat = Zero | Succ Nat
--
-- Suppose I want
-- foo :: (Succ n == Succ m) ~ True => ((n == m) :~: True)
-- foo = Refl
--
-- This would not type-check with the simple version. `Succ n == Succ m`
-- is stuck. We don't know enough about `n` and `m` to reduce the family.
-- With the recursive version, `Succ n == Succ m` reduces to
-- `Succ == Succ && n == m`, which can reduce to `'True && n == m` and
-- finally to `n == m`.
