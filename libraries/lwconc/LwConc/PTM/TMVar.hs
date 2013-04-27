{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM.TMVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
--
-- TMVar: Transactional MVars, for use in the PTM monad
-- (GHC only)
--
-----------------------------------------------------------------------------

module LwConc.PTM.TMVar (
#ifdef __GLASGOW_HASKELL__
	-- * TMVars
	TMVar,
	newTMVar,
	newEmptyTMVar,
	newTMVarIO,
	newEmptyTMVarIO,
	takeTMVar,
	putTMVar,
	readTMVar,
	tryReadTMVar,
	swapTMVar,
	tryTakeTMVar,
	tryPutTMVar,
	isEmptyTMVar
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import LwConc.Substrate

import Data.Typeable (Typeable)

newtype TMVar a = TMVar (PVar (Maybe a)) deriving (Eq, Typeable)
{- ^
A 'TMVar' is a synchronising variable, used
for communication between concurrent threads.  It can be thought of
as a box, which may be empty or full.
-}

-- |Create a 'TMVar' which contains the supplied value.
newTMVar :: a -> PTM (TMVar a)
newTMVar a = do
  t <- newPVar (Just a)
  return (TMVar t)

-- |@IO@ version of 'newTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTMVarIO :: a -> IO (TMVar a)
newTMVarIO a = do
  t <- newPVarIO (Just a)
  return (TMVar t)

-- |Create a 'TMVar' which is initially empty.
newEmptyTMVar :: PTM (TMVar a)
newEmptyTMVar = do
  t <- newPVar Nothing
  return (TMVar t)

-- |@IO@ version of 'newEmptyTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = do
  t <- newPVarIO Nothing
  return (TMVar t)

-- |Return the contents of the 'TMVar'.  If the 'TMVar' is currently
-- empty, the transaction will 'retry'.  After a 'takeTMVar',
-- the 'TMVar' is left empty.
takeTMVar :: TMVar a -> PTM a
takeTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> retry
    Just a  -> do writePVar t Nothing; return a

-- | A version of 'takeTMVar' that does not 'retry'.  The 'tryTakeTMVar'
-- function returns 'Nothing' if the 'TMVar' was empty, or @'Just' a@ if
-- the 'TMVar' was full with contents @a@.  After 'tryTakeTMVar', the
-- 'TMVar' is left empty.
tryTakeTMVar :: TMVar a -> PTM (Maybe a)
tryTakeTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writePVar t Nothing; return (Just a)

-- |Put a value into a 'TMVar'.  If the 'TMVar' is currently full,
-- 'putTMVar' will 'retry'.
putTMVar :: TMVar a -> a -> PTM ()
putTMVar (TMVar t) a = do
  m <- readPVar t
  case m of
    Nothing -> do writePVar t (Just a); return ()
    Just _  -> retry

-- | A version of 'putTMVar' that does not 'retry'.  The 'tryPutTMVar'
-- function attempts to put the value @a@ into the 'TMVar', returning
-- 'True' if it was successful, or 'False' otherwise.
tryPutTMVar :: TMVar a -> a -> PTM Bool
tryPutTMVar (TMVar t) a = do
  m <- readPVar t
  case m of
    Nothing -> do writePVar t (Just a); return True
    Just _  -> return False

-- | This is a combination of 'takeTMVar' and 'putTMVar'; ie. it
-- takes the value from the 'TMVar', puts it back, and also returns
-- it.
readTMVar :: TMVar a -> PTM a
readTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> retry
    Just a  -> return a

-- | A version of 'readTMVar' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTMVar :: TMVar a -> PTM (Maybe a)
tryReadTMVar (TMVar t) = readPVar t

-- |Swap the contents of a 'TMVar' for a new value.
swapTMVar :: TMVar a -> a -> PTM a
swapTMVar (TMVar t) new = do
  m <- readPVar t
  case m of
    Nothing -> retry
    Just old -> do writePVar t (Just new); return old

-- |Check whether a given 'TMVar' is empty.
isEmptyTMVar :: TMVar a -> PTM Bool
isEmptyTMVar (TMVar t) = do
  m <- readPVar t
  case m of
    Nothing -> return True
    Just _  -> return False
#endif
