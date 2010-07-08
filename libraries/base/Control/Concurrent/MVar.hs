{-# OPTIONS_GHC -XNoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.MVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Synchronising variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.MVar
        (
          -- * @MVar@s
          MVar          -- abstract
        , newEmptyMVar  -- :: IO (MVar a)
        , newMVar       -- :: a -> IO (MVar a)
        , takeMVar      -- :: MVar a -> IO a
        , putMVar       -- :: MVar a -> a -> IO ()
        , readMVar      -- :: MVar a -> IO a
        , swapMVar      -- :: MVar a -> a -> IO a
        , tryTakeMVar   -- :: MVar a -> IO (Maybe a)
        , tryPutMVar    -- :: MVar a -> a -> IO Bool
        , isEmptyMVar   -- :: MVar a -> IO Bool
        , withMVar      -- :: MVar a -> (a -> IO b) -> IO b
        , modifyMVar_   -- :: MVar a -> (a -> IO a) -> IO ()
        , modifyMVar    -- :: MVar a -> (a -> IO (a,b)) -> IO b
#ifndef __HUGS__
        , addMVarFinalizer -- :: MVar a -> IO () -> IO ()
#endif
    ) where

#ifdef __HUGS__
import Hugs.ConcBase ( MVar, newEmptyMVar, newMVar, takeMVar, putMVar,
                  tryTakeMVar, tryPutMVar, isEmptyMVar,
                )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.MVar ( MVar, newEmptyMVar, newMVar, takeMVar, putMVar,
                  tryTakeMVar, tryPutMVar, isEmptyMVar, addMVarFinalizer
                )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#else
import Prelude
#endif

import Control.Exception.Base

{-|
  This is a combination of 'takeMVar' and 'putMVar'; ie. it takes the value
  from the 'MVar', puts it back, and also returns it.
-}
readMVar :: MVar a -> IO a
readMVar m =
  mask_ $ do
    a <- takeMVar m
    putMVar m a
    return a

{-|
  Take a value from an 'MVar', put a new value into the 'MVar' and
  return the value taken. Note that there is a race condition whereby
  another process can put something in the 'MVar' after the take
  happens but before the put does.
-}
swapMVar :: MVar a -> a -> IO a
swapMVar mvar new =
  mask_ $ do
    old <- takeMVar mvar
    putMVar mvar new
    return old

{-|
  'withMVar' is a safe wrapper for operating on the contents of an
  'MVar'.  This operation is exception-safe: it will replace the
  original contents of the 'MVar' if an exception is raised (see
  "Control.Exception").
-}
{-# INLINE withMVar #-}
-- inlining has been reported to have dramatic effects; see
-- http://www.haskell.org//pipermail/haskell/2006-May/017907.html
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io =
  mask $ \restore -> do
    a <- takeMVar m
    b <- restore (io a) `onException` putMVar m a
    putMVar m a
    return b

{-|
  A safe wrapper for modifying the contents of an 'MVar'.  Like 'withMVar', 
  'modifyMVar' will replace the original contents of the 'MVar' if an
  exception is raised during the operation.
-}
{-# INLINE modifyMVar_ #-}
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io =
  mask $ \restore -> do
    a  <- takeMVar m
    a' <- restore (io a) `onException` putMVar m a
    putMVar m a'

{-|
  A slight variation on 'modifyMVar_' that allows a value to be
  returned (@b@) in addition to the modified value of the 'MVar'.
-}
{-# INLINE modifyMVar #-}
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io =
  mask $ \restore -> do
    a      <- takeMVar m
    (a',b) <- restore (io a) `onException` putMVar m a
    putMVar m a'
    return b
