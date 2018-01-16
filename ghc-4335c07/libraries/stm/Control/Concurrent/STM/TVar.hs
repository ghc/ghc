{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TVar: Transactional variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TVar (
        -- * TVars
        TVar,
        newTVar,
        newTVarIO,
        readTVar,
        readTVarIO,
        writeTVar,
        modifyTVar,
        modifyTVar',
        swapTVar,
#ifdef __GLASGOW_HASKELL__
        registerDelay,
#endif
        mkWeakTVar
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Conc
import GHC.Weak
#else
import Control.Sequential.STM
#endif

-- Like 'modifyIORef' but for 'TVar'.
-- | Mutate the contents of a 'TVar'. /N.B./, this version is
-- non-strict.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)
{-# INLINE modifyTVar #-}


-- | Strict version of 'modifyTVar'.
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x
{-# INLINE modifyTVar' #-}


-- Like 'swapTMVar' but for 'TVar'.
-- | Swap the contents of a 'TVar' for a new value.
swapTVar :: TVar a -> a -> STM a
swapTVar var new = do
    old <- readTVar var
    writeTVar var new
    return old
{-# INLINE swapTVar #-}


-- | Make a 'Weak' pointer to a 'TVar', using the second argument as
-- a finalizer to run when 'TVar' is garbage-collected
--
-- @since 2.4.3
mkWeakTVar :: TVar a -> IO () -> IO (Weak (TVar a))
mkWeakTVar t@(TVar t#) (IO finalizer) = IO $ \s ->
    case mkWeak# t# t finalizer s of (# s1, w #) -> (# s1, Weak w #)
