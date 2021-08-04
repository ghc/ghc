{-# LANGUAGE Safe #-}

-- |
-- Module      :  Data.Hashable.Generic
-- SPDX-License-Identifier : BSD-3-Clause
-- Stability   :  provisional
-- Portability :  GHC >= 7.4
--
-- Hashable support for GHC generics.
--
-- @since 1.3.0.0
module Data.Hashable.Generic
    (
    -- * Implementation using Generics.
      genericHashWithSalt
    , genericLiftHashWithSalt
    -- * Constraints
    , GHashable (..)
    , One
    , Zero
    , HashArgs (..)
    ) where

import Data.Hashable.Generic.Instances ()
import Data.Hashable.Class
