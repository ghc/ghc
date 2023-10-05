{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Ord
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  not portable
--
-- Basic operations on type-level Orderings.
--
-- @since 4.16.0.0
-----------------------------------------------------------------------------

module Data.Type.Ord (
  Compare, OrderingI(..)
  , type (<=), type (<=?)
  , type (>=), type (>=?)
  , type (>), type (>?)
  , type (<), type (<?)
  , Max, Min
  , OrdCond
  ) where

import GHC.Show(Show(..))
import GHC.TypeError
import GHC.TypeLits.Internal
import GHC.TypeNats.Internal
import GHC.Types (type (~))
import Data.Bool
import Data.Char(Char)
import Data.Eq
import Data.Ord

-- | 'Compare' branches on the kind of its arguments to either compare by
-- 'Symbol' or 'Nat'.
--
-- @since 4.16.0.0
type Compare :: k -> k -> Ordering
type family Compare (a :: k) (b :: k) :: Ordering

type instance Compare (a :: Natural) b = CmpNat    a b
type instance Compare (a :: Symbol)  b = CmpSymbol a b
type instance Compare (a :: Char)    b = CmpChar   a b

-- | Ordering data type for type literals that provides proof of their ordering.
--
-- @since 4.16.0.0
data OrderingI a b where
  LTI :: Compare a b ~ 'LT => OrderingI a b
  EQI :: Compare a a ~ 'EQ => OrderingI a a
  GTI :: Compare a b ~ 'GT => OrderingI a b

deriving instance Show (OrderingI a b)
deriving instance Eq   (OrderingI a b)

infix 4 <=?, <=, >=?, >=, <?, <, >?, >

-- | Comparison (<=) of comparable types, as a constraint.
--
-- @since 4.16.0.0
type x <= y = Assert (x <=? y) (LeErrMsg x y)
type LeErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " <= " ':<>: 'ShowType y)

-- | Comparison (>=) of comparable types, as a constraint.
--
-- @since 4.16.0.0
type x >= y = Assert (x >=? y) (GeErrMsg x y)
type GeErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " >= " ':<>: 'ShowType y)

-- | Comparison (<) of comparable types, as a constraint.
--
-- @since 4.16.0.0
type x < y = Assert (x <? y) (LtErrMsg x y)
type LtErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " < " ':<>: 'ShowType y)

-- | Comparison (>) of comparable types, as a constraint.
--
-- @since 4.16.0.0
type x > y = Assert (x >? y) (GtErrMsg x y)
type GtErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " > " ':<>: 'ShowType y)

-- | Comparison (<=) of comparable types, as a function.
--
-- @since 4.16.0.0
type (<=?) :: k -> k -> Bool
type m <=? n = OrdCond (Compare m n) 'True 'True 'False

-- | Comparison (>=) of comparable types, as a function.
--
-- @since 4.16.0.0
type (>=?) :: k -> k -> Bool
type m >=? n = OrdCond (Compare m n) 'False 'True 'True

-- | Comparison (<) of comparable types, as a function.
--
-- @since 4.16.0.0
type (<?) :: k -> k -> Bool
type m <? n = OrdCond (Compare m n) 'True 'False 'False

-- | Comparison (>) of comparable types, as a function.
--
-- @since 4.16.0.0
type (>?) :: k -> k -> Bool
type m >? n = OrdCond (Compare m n) 'False 'False 'True


-- | Maximum between two comparable types.
--
-- @since 4.16.0.0
type Max :: k -> k -> k
type Max m n = OrdCond (Compare m n) n n m

-- | Minimum between two comparable types.
--
-- @since 4.16.0.0
type Min :: k -> k -> k
type Min m n = OrdCond (Compare m n) m m n


-- | A case statement on 'Ordering'.
--
-- @OrdCond c l e g@ is @l@ when @c ~ LT@, @e@ when @c ~ EQ@, and @g@ when
-- @c ~ GT@.
--
-- @since 4.16.0.0
type OrdCond :: Ordering -> k -> k -> k -> k
type family OrdCond o lt eq gt where
  OrdCond 'LT lt eq gt = lt
  OrdCond 'EQ lt eq gt = eq
  OrdCond 'GT lt eq gt = gt
