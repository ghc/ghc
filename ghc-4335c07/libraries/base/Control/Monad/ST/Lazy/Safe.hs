{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Lazy.Safe
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
-- Safe API only.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy.Safe {-# DEPRECATED "Safe is now the default, please use Control.Monad.ST.Lazy instead" #-} (
        -- * The 'ST' monad
        ST,
        runST,
        fixST,

        -- * Converting between strict and lazy 'ST'
        strictToLazyST, lazyToStrictST,

        -- * Converting 'ST' To 'IO'
        RealWorld,
        stToIO,
    ) where

import Control.Monad.ST.Lazy.Imp

