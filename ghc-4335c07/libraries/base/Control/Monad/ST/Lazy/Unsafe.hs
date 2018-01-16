{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Lazy.Unsafe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This module presents an identical interface to "Control.Monad.ST",
-- except that the monad delays evaluation of state operations until
-- a value depending on them is required.
--
-- Unsafe API.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy.Unsafe (
        -- * Unsafe operations
        unsafeInterleaveST,
        unsafeIOToST
    ) where

import Control.Monad.ST.Lazy.Imp

