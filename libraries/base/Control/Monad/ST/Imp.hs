{-# LANGUAGE Unsafe #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Imp
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This library provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Imp (
        -- * The 'ST' Monad
        ST,             -- abstract, instance of Functor, Monad, Typeable.
        runST,
        fixST,

        -- * Converting 'ST' to 'IO'
        RealWorld,              -- abstract
        stToIO,

        -- * Unsafe operations
        unsafeInterleaveST,
        unsafeIOToST,
        unsafeSTToIO
    ) where

import GHC.ST           ( ST, runST, fixST, unsafeInterleaveST )
import GHC.Base         ( RealWorld )
import GHC.IO           ( stToIO, unsafeIOToST, unsafeSTToIO )
