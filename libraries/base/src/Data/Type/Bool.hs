{-# LANGUAGE Safe #-}

{-# LANGUAGE ExplicitNamespaces #-}

-- |
--
-- Module      :  Data.Type.Bool
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  not portable
--
-- Basic operations on type-level Booleans.
--
-- @since 4.7.0.0

module Data.Type.Bool
    (If,
     type (&&),
     type (||),
     Not
     ) where

import GHC.Internal.Data.Type.Bool
