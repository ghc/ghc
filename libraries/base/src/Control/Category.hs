{-# LANGUAGE Safe #-}

-- |
-- Module      :  Control.Category
-- Copyright   :  (c) Ashley Yakeley 2007
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ashley@semantic.org
-- Stability   :  stable
-- Portability :  portable
--

module Control.Category
  ( -- * Class
    Category(..)

    -- * Combinators
  , (<<<)
  , (>>>)

  -- $namingConflicts
  ) where

import GHC.Internal.Control.Category

-- $namingConflicts
--
-- == A note on naming conflicts
--
-- The methods from 'Category' conflict with 'Prelude.id' and 'Prelude..' from the
-- prelude; you will likely want to either import this module qualified, or hide the
-- prelude functions:
--
-- @
-- import "Prelude" hiding (id, (.))
-- @
