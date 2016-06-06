{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A logically uninhabited data type, used to indicate that a given
-- term should not exist.
--
-- @since 4.8.0.0
----------------------------------------------------------------------------
module Data.Void
    ( Void
    , absurd
    , vacuous
    ) where

import Control.Exception
import Data.Data
import Data.Ix
import GHC.Generics

-- | Uninhabited data type
--
-- @since 4.8.0.0
data Void deriving (Generic)

deriving instance Data Void

-- | @since 4.8.0.0
instance Eq Void where
    _ == _ = True

-- | @since 4.8.0.0
instance Ord Void where
    compare _ _ = EQ

-- | Reading a 'Void' value is always a parse error, considering
-- 'Void' as a data type with no constructors.
-- | @since 4.8.0.0
instance Read Void where
    readsPrec _ _ = []

-- | @since 4.8.0.0
instance Show Void where
    showsPrec _ = absurd

-- | @since 4.8.0.0
instance Ix Void where
    range _     = []
    index _     = absurd
    inRange _   = absurd
    rangeSize _ = 0

-- | @since 4.8.0.0
instance Exception Void

-- | Since 'Void' values logically don't exist, this witnesses the
-- logical reasoning tool of \"ex falso quodlibet\".
--
-- @since 4.8.0.0
absurd :: Void -> a
absurd a = case a of {}

-- | If 'Void' is uninhabited then any 'Functor' that holds only
-- values of type 'Void' is holding no values.
--
-- @since 4.8.0.0
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd
