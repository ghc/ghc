{-# LANGUAGE CPP #-}
#if sh_SAFE_DEFAULT
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Unsafe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Lazy
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
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy (
          module Control.Monad.ST.Lazy.Safe
#if !sh_SAFE_DEFAULT
        -- * Unsafe Functions
        , unsafeInterleaveST
        , unsafeIOToST
#endif
    ) where

import Control.Monad.ST.Lazy.Safe
#if !sh_SAFE_DEFAULT
import qualified Control.Monad.ST.Lazy.Unsafe as U

{-# DEPRECATED unsafeInterleaveST, unsafeIOToST
              "Please import from Control.Monad.ST.Lazy.Unsafe instead; This will be removed in the next release"
 #-}

{-# INLINE unsafeInterleaveST #-}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = U.unsafeInterleaveST

{-# INLINE unsafeIOToST #-}
unsafeIOToST :: IO a -> ST s a
unsafeIOToST = U.unsafeIOToST
#endif

