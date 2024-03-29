{-# LANGUAGE Unsafe #-}

-- |
--
-- Module      :  Control.Monad.ST.Unsafe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This module provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-- Unsafe API.
--

module Control.Monad.ST.Unsafe
    (-- *  Unsafe operations
     unsafeInterleaveST,
     unsafeDupableInterleaveST,
     unsafeIOToST,
     unsafeSTToIO
     ) where

import GHC.Internal.Control.Monad.ST.Imp
