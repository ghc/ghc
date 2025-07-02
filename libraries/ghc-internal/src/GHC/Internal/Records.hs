{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Records
-- Copyright   :  (c) Adam Gundry 2015-2024
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'HasField' class used by the
-- @OverloadedRecordDot@ extension.  See the
-- <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/hasfield.html
-- user's guide> for more details.
--
-----------------------------------------------------------------------------

module GHC.Internal.Records
       ( HasField(..)
       , SetField(..)
       , Field
       ) where

import GHC.Internal.Types (TYPE, Constraint, LiftedRep, type (~))

-- | Constraint representing the fact that the field @x@ belongs to
-- the record type @r@ and has field type @a@.  This will be solved
-- automatically, but manual instances may be provided as well.

--   HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
--   getField :: forall {k} {r_rep} {a_rep} (x::k) (r :: TYPE r_rep) (a :: TYPE a_rep) . HasField x r a => r -> a
-- NB: The {k} means that k is an 'inferred' type variable, and
--     hence not provided in visible type applications.  Thus you
--     say     getField @"foo"
--     not     getField @Symbol @"foo"
type HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
class HasField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  getField :: r -> a

-- | Constraint representing the fact that a field @x@ of type @a@ can be
-- updated in the record type @r@.
--
-- This will be solved automatically for built-in records where the field is
-- in scope, but manual instances may be provided as well.
--
-- Instances of this class are subject to the following laws, for every record
-- value @r@ and field @x@:
--
-- > modifyField @x id r === r or ⊥
-- > (modifyField @x g . modifyField @x f) r === modifyField @x (g . f) r
-- > setField @x v r == modifyField @x (\ _ -> v) r
--
-- Where a 'HasField' instance is available as well as an instance of this
-- class, they must together satisfy the laws defined on 'Field'.
type SetField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
class SetField x (r :: TYPE r_rep) (a :: TYPE a_rep) | x r -> a where
  -- | Change the value stored in the field @x@ of the record @r@.
  modifyField :: (a -> a) -> r -> r
  default modifyField :: (r_rep ~ LiftedRep, a_rep ~ LiftedRep, HasField x r a) => (a -> a) -> r -> r
  modifyField = modifyFieldLifted (setField @x)
    where
      -- Trick to overcome GHC typechecker limitations
      -- If you try to write that code directly, GHC typechchecker
      -- would fail on (f _) call, claiming that f's argument
      -- has no fixed runtime representation, even if there is
      -- a_rep ~ LiftedRep at the list of given constaintgs
      modifyFieldLifted :: forall r a. HasField x r a => (a -> r -> r) -> (a -> a) -> r -> r
      modifyFieldLifted setter f r = setter (f (getField @x r)) r

  -- | Update function to set the field @x@ in the record @r@.
  setField :: a -> r -> r
  default setField :: (a_rep ~ LiftedRep) => a -> r -> r
  setField v = modifyField @x (\ _ -> v)

  {-# MINIMAL modifyField | setField #-}

-- | Constraint representing the fact that a field @x@ of type @a@ can be
--  selected from or updated in the record @r@.
--
-- Where both 'HasField' and 'SetField' instances are defined for the
-- same type, they must satisfy the following laws:
--
-- For every @r@ which has the field @x@
-- (that is, wherever 'getField @x r' is defined):
--
-- > getField @x (setField @x v r) === v
-- > setField @x (getField @x r) r === r
--
-- For every @r@ which does not have the field @x@
-- (that is, wherever 'getField @x r' is not defined):
--
-- > getField @x (setField @x v r) === ⊥
-- > setField @x (getField @x r) r === r or ⊥
type Field :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
type Field x r a = (HasField x r a, SetField x r a)