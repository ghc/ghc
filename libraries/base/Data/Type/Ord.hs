{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Ord
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- Basic operations on type-level Orderings.
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------

module Data.Type.Ord (
  Compare, TypeOrdering
  , type (<=), type (<=?)
  , type (>=), type (>=?)
  , type (>), type (>?)
  , type (<), type (<?)
  , Max, Min

  ) where

import GHC.Show(Show(..))
import Data.Bool
import Data.Ord
import Data.Eq

-- | 'Compare' branches on the kind of its arguments to either compare by
-- 'Symbol' or 'Nat'.
type family Compare (a :: k) (b :: k) :: Ordering

-- | Ordering data type for type literals that provides proof of their ordering.
data TypeOrdering a b where
  TypeLt :: Compare a b ~ 'LT => TypeOrdering a b
  TypeEq :: TypeOrdering a a
  TypeGt :: Compare a b ~ 'GT => TypeOrdering a b

deriving instance Show (TypeOrdering a b)
deriving instance Eq   (TypeOrdering a b)


infix 4 <=?, <=, >=?, >=, <?, <, >?, >

type x <= y = (x <=? y) ~ 'True
type family (m :: k) <=? (n :: k) :: Bool
type instance m <=? n = OrdCond (Compare m n) 'True 'True 'False

type x >= y = (x >=? y) ~ 'True
type family (m :: k) >=? (n :: k) :: Bool where
  m >=? n = OrdCond (Compare m n) 'False 'True 'True

type x > y = (x >? y) ~ 'True
type family (m :: k) >? (n :: k) :: Bool where
  m >? n = OrdCond (Compare m n) 'False 'False 'True

type x < y = (x >? y) ~ 'True
type family (m :: k) <? (n :: k) :: Bool where
  m <? n = OrdCond (Compare m n) 'True 'False 'False

type family Max (m :: k) (n :: k) :: Bool where
  Max m n = OrdCond (Compare m n) n n m

type family Min (m :: k) (n :: k) :: Bool where
  Min m n = OrdCond (Compare m n) m m n


type family OrdCond (o :: Ordering) (lt :: k) (eq :: k) (gt :: k) where
  OrdCond 'LT lt eq gt = lt
  OrdCond 'EQ lt eq gt = eq
  OrdCond 'GT lt eq gt = gt
