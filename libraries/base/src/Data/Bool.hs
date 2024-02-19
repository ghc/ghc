{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Bool
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The 'Bool' type and related functions.
--

module Data.Bool
    (-- *  Booleans
     Bool(..),
     -- **  Operations
     (&&),
     (||),
     not,
     otherwise,
     bool
     ) where

import GHC.Internal.Data.Bool