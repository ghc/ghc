{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Coercion
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  not portable
--
-- Definition of representational equality ('Coercion').
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------

module Data.Type.Coercion
  ( Coercion(..)
  , coerceWith
  , gcoerceWith
  , sym
  , trans
  , repr
  , TestCoercion(..)
  ) where

import qualified Data.Type.Equality as Eq
import Data.Maybe
import GHC.Enum
import GHC.Show
import GHC.Read
import GHC.Base

-- | Representational equality. If @Coercion a b@ is inhabited by some terminating
-- value, then the type @a@ has the same underlying representation as the type @b@.
--
-- To use this equality in practice, pattern-match on the @Coercion a b@ to get out
-- the @Coercible a b@ instance, and then use 'coerce' to apply it.
--
-- @since 4.7.0.0
data Coercion a b where
  Coercion :: Coercible a b => Coercion a b

-- with credit to Conal Elliott for 'ty', Erik Hesselink & Martijn van
-- Steenbergen for 'type-equality', Edward Kmett for 'eq', and Gabor Greif
-- for 'type-eq'

-- | Type-safe cast, using representational equality
coerceWith :: Coercion a b -> a -> b
coerceWith Coercion x = coerce x

-- | Generalized form of type-safe cast using representational equality
--
-- @since 4.10.0.0
gcoerceWith :: Coercion a b -> (Coercible a b => r) -> r
gcoerceWith Coercion x = x

-- | Symmetry of representational equality
sym :: Coercion a b -> Coercion b a
sym Coercion = Coercion

-- | Transitivity of representational equality
trans :: Coercion a b -> Coercion b c -> Coercion a c
trans Coercion Coercion = Coercion

-- | Convert propositional (nominal) equality to representational equality
repr :: (a Eq.:~: b) -> Coercion a b
repr Eq.Refl = Coercion

-- | @since 4.7.0.0
deriving instance Eq   (Coercion a b)

-- | @since 4.7.0.0
deriving instance Show (Coercion a b)

-- | @since 4.7.0.0
deriving instance Ord  (Coercion a b)

-- | @since 4.7.0.0
deriving instance Coercible a b => Read (Coercion a b)

-- | @since 4.7.0.0
instance Coercible a b => Enum (Coercion a b) where
  toEnum 0 = Coercion
  toEnum _ = errorWithoutStackTrace "Data.Type.Coercion.toEnum: bad argument"

  fromEnum Coercion = 0

-- | @since 4.7.0.0
deriving instance Coercible a b => Bounded (Coercion a b)

-- | This class contains types where you can learn the coercibility of two
-- types from information contained in /terms/.
--
-- The result should be @Just Coercion@ if and only if the types applied to @f@
-- are coercible:
--
-- @testCoercion (x :: f a) (y :: f b) = Just Coercion@ ⟺ Coercible a b@
--
-- === __Guidance on use with singleton types__
--
-- It was previously recommend that only singleton types should inhabit this
-- class. However, it is unclear what the singleton types should represent.
--
-- Normally, a singleton type has one value per *type*: types quotiented by
-- nominal equivalence, i.e. just types.
--
-- An example of this would be:
--
-- @
--     data Nom0 :: Type -> Type where
--       Nom0Int  :: Nom0 Int
--       Nom0Bool :: Nom0 Bool
-- @
--
-- Which, to make relationship to the next part more clear, let's rewrite more
-- verbosely as:
--
-- @
--     data Nom1 :: Type -> Type where
--       Nom1Int  :: forall a. a ~ Int  => Nom1 a
--       Nom1Bool :: forall a. a ~ Bool => Nom1 a
-- @
--
-- But arguably, given that we are testing for coercibility, it would be better
-- to have one value per *representation*: types quotiented by representational
-- equivalence, i.e. equivalence classes of types that are
-- pairwise-'Coercible'.
--
-- An example of this would be:
--
-- @
--     data Rep :: Type -> Type where
--       RepInt  :: forall a. a `Coercible` Int  => Rep a
--       RepBool :: forall a. a `Coercible` Bool => Rep a
-- @
--
-- The goal of this would be to have laws similar to that for 'TestEquality'
-- when used with singletons:
--
-- @testCoercion (x :: f a) (y :: f b) = Just Refl ⟺ a `Coercible` b ⟺ coerce x = coerce y@
--
-- @isJust (testCoercion x y) = coerce x == coerce y@
--
-- However, GHC today thinks the argument to 'Rep' must have a nominal role,
-- and therefore the above doesn't work.
--
-- Regardless, singleton types are not required, however, and so the latter
-- would-be laws are not in fact valid in general.
class TestCoercion f where
  -- | Conditionally prove the representational equality of @a@ and @b@.
  testCoercion :: f a -> f b -> Maybe (Coercion a b)

-- | @since 4.7.0.0
instance TestCoercion ((Eq.:~:) a) where
  testCoercion Eq.Refl Eq.Refl = Just Coercion

-- | @since 4.10.0.0
instance TestCoercion ((Eq.:~~:) a) where
  testCoercion Eq.HRefl Eq.HRefl = Just Coercion

-- | @since 4.7.0.0
instance TestCoercion (Coercion a) where
  testCoercion Coercion Coercion = Just Coercion
