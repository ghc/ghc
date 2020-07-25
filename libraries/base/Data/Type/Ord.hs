{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
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
-- Stability   :  experimental
-- Portability :  not portable
--
-- Basic operations on type-level Orderings.
--
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
type Compare :: k -> k -> Ordering
type family Compare a b

-- | Ordering data type for type literals that provides proof of their ordering.
data TypeOrdering a b where
  TypeLt :: Compare a b ~ 'LT => TypeOrdering a b
  TypeEq :: TypeOrdering a a
  TypeGt :: Compare a b ~ 'GT => TypeOrdering a b

deriving instance Show (TypeOrdering a b)
deriving instance Eq   (TypeOrdering a b)


infix 4 <=?, <=, >=?, >=, <?, <, >?, >

-- | Comparison (<=) of comparable types, as a constraint.
type x <= y = (x <=? y) ~ 'True

-- | Comparison (>=) of comparable types, as a constraint.
type x >= y = (x >=? y) ~ 'True

-- | Comparison (<) of comparable types, as a constraint.
type x < y = (x >? y) ~ 'True

-- | Comparison (>) of comparable types, as a constraint.
type x > y = (x >? y) ~ 'True


-- | Comparison (<=) of comparable types, as a function.
type (<=?) :: k -> k -> Bool
type family m <=? n
type instance m <=? n = OrdCond (Compare m n) 'True 'True 'False

-- | Comparison (>=) of comparable types, as a function.
type (>=?) :: k -> k -> Bool
type family m >=? n
type instance m >=? n = OrdCond (Compare m n) 'False 'True 'True

-- | Comparison (<) of comparable types, as a function.
type (<?) :: k -> k -> Bool
type family m <? n
type instance m <? n = OrdCond (Compare m n) 'True 'False 'False

-- | Comparison (>) of comparable types, as a function.
type (>?) :: k -> k -> Bool
type family m >? n
type instance m >? n = OrdCond (Compare m n) 'False 'False 'True


-- | Maximum between two comparable types.
type Max :: k -> k -> k
type family Max m n
type instance Max m n = OrdCond (Compare m n) n n m

-- | Minimum between two comparable types.
type Min :: k -> k -> k
type family Min m n
type instance Min m n = OrdCond (Compare m n) m m n


type OrdCond :: Ordering -> k -> k -> k -> k
type family OrdCond o lt eq gt where
  OrdCond 'LT lt eq gt = lt
  OrdCond 'EQ lt eq gt = eq
  OrdCond 'GT lt eq gt = gt
