{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM.TVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
--
-- TVar: Transactional variables
--
-----------------------------------------------------------------------------

module LwConc.PTM.TVar (
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
	-- registerDelay
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import LwConc.Substrate
#else
import Control.Sequential.PTM
#endif

#if ! (MIN_VERSION_base(4,2,0))
readPVarIO = atomically . readPVar
#endif

type TVar = PVar

newTVar :: a -> PTM (TVar a)
newTVar 	= newPVar

newTVarIO :: a -> IO (TVar a)
newTVarIO = newPVarIO

readTVar :: TVar a -> PTM a
readTVar 	= readPVar

writeTVar :: TVar a -> a -> PTM ()
writeTVar = writePVar

readTVarIO :: TVar a -> IO a
readTVarIO = readPVarIO

-- Like 'modifyIORef' but for 'TVar'.
-- | Mutate the contents of a 'TVar'. /N.B./, this version is
-- non-strict.
modifyTVar :: TVar a -> (a -> a) -> PTM ()
modifyTVar var f = do
    x <- readPVar var
    writePVar var (f x)
{-# INLINE modifyTVar #-}


-- | Strict version of 'modifyTVar'.
modifyTVar' :: TVar a -> (a -> a) -> PTM ()
modifyTVar' var f = do
    x <- readPVar var
    writePVar var $! f x
{-# INLINE modifyTVar' #-}


-- Like 'swapTMVar' but for 'TVar'.
-- | Swap the contents of a 'TVar' for a new value.
swapTVar :: TVar a -> a -> PTM a
swapTVar var new = do
    old <- readPVar var
    writePVar var new
    return old
{-# INLINE swapTVar #-}

