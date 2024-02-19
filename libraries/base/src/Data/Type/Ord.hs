{-# LANGUAGE Safe #-}

{-# LANGUAGE ExplicitNamespaces #-}

-- |
--
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

module Data.Type.Ord
    (Compare,
     OrderingI(..),
     type (<=),
     type (<=?),
     type (>=),
     type (>=?),
     type (>),
     type (>?),
     type (<),
     type (<?),
     Max,
     Min,
     OrdCond
     ) where

import GHC.Internal.Data.Type.Ord
