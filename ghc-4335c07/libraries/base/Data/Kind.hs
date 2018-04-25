{-# LANGUAGE Trustworthy, ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Kind
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- Basic kinds
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

module Data.Kind ( Type, Constraint, type (*), type (★) ) where

import GHC.Types
