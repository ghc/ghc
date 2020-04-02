{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE Safe #-}

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
import Data.Semigroup (Semigroup(..), stimesIdempotent)

-- $setup
-- >>> import Prelude

-- | Uninhabited data type
--
-- @since 4.8.0.0
data Void deriving
  ( Eq      -- ^ @since 4.8.0.0
  , Data    -- ^ @since 4.8.0.0
  , Generic -- ^ @since 4.8.0.0
  , Ord     -- ^ @since 4.8.0.0
  , Read    -- ^ Reading a 'Void' value is always a parse error, considering
            -- 'Void' as a data type with no constructors.
            --
            -- @since 4.8.0.0
  , Show    -- ^ @since 4.8.0.0
  )

-- | @since 4.8.0.0
instance Ix Void where
    range _     = []
    index _     = absurd
    inRange _   = absurd
    rangeSize _ = 0

-- | @since 4.8.0.0
instance Exception Void

-- | @since 4.9.0.0
instance Semigroup Void where
    a <> _ = a
    stimes = stimesIdempotent

-- | Since 'Void' values logically don't exist, this witnesses the
-- logical reasoning tool of \"ex falso quodlibet\".
--
-- >>> let x :: Either Void Int; x = Right 5
-- >>> :{
-- case x of
--     Right r -> r
--     Left l  -> absurd l
-- :}
-- 5
--
-- @since 4.8.0.0
absurd :: Void -> a
absurd a = case a of {}

-- | If 'Void' is uninhabited then any 'Functor' that holds only
-- values of type 'Void' is holding no values.
-- It is implemented in terms of @fmap absurd@.
--
-- @since 4.8.0.0
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd
