{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST
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
-- References (variables) that can be used within the @ST@ monad are
-- provided by "Data.STRef", and arrays are provided by
-- "Data.Array.ST".

-----------------------------------------------------------------------------

module Control.Monad.ST (
        -- * The 'ST' Monad
        ST,             -- abstract, instance of Functor, Monad, Typeable.
        runST,
        fixST,

        -- * Converting 'ST' to 'IO'
        RealWorld,              -- abstract
        stToIO,

        -- * Unsafe Functions
        unsafeInterleaveST,
        unsafeIOToST,
        unsafeSTToIO
    ) where

import Control.Monad.ST.Safe
import qualified Control.Monad.ST.Unsafe as U

{-# DEPRECATED unsafeInterleaveST, unsafeIOToST, unsafeSTToIO "Please import from Control.Monad.ST.Unsafe instead; This will be removed in the next release" #-} -- deprecated in 7.2

{-# INLINE unsafeInterleaveST #-}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = U.unsafeInterleaveST

{-# INLINE unsafeIOToST #-}
unsafeIOToST :: IO a -> ST s a
unsafeIOToST = U.unsafeIOToST

{-# INLINE unsafeSTToIO #-}
unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO = U.unsafeSTToIO

