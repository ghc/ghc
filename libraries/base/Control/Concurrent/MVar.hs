-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.MVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: MVar.hs,v 1.2 2002/04/24 16:31:37 simonmar Exp $
--
-- MVars: Synchronising variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.MVar
	( MVar		-- abstract
	, newEmptyMVar  -- :: IO (MVar a)
	, newMVar 	-- :: a -> IO (MVar a)
	, takeMVar 	-- :: MVar a -> IO a
	, putMVar  	-- :: MVar a -> a -> IO ()
	, readMVar 	-- :: MVar a -> IO a
	, swapMVar 	-- :: MVar a -> a -> IO a
	, tryTakeMVar   -- :: MVar a -> IO (Maybe a)
	, tryPutMVar    -- :: MVar a -> a -> IO Bool
	, isEmptyMVar	-- :: MVar a -> IO Bool
	, withMVar	-- :: MVar a -> (a -> IO b) -> IO b
	, modifyMVar_ 	-- :: MVar a -> (a -> IO a) -> IO ()
	, modifyMVar 	-- :: MVar a -> (a -> IO (a,b)) -> IO b
	, addMVarFinalizer -- :: MVar a -> IO () -> IO ()
    ) where

#ifdef __HUGS__
import ConcBase	( MVar, newEmptyMVar, newMVar, takeMVar, putMVar,
		  tryTakeMVar, tryPutMVar, isEmptyMVar,
                  readMVar, swapMVar,
		)
import Prelude hiding( catch )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Conc	( MVar, newEmptyMVar, newMVar, takeMVar, putMVar,
		  tryTakeMVar, tryPutMVar, isEmptyMVar, addMVarFinalizer
		)
#endif

import Control.Exception as Exception

#ifdef __HUGS__
-- This is as close as Hugs gets to providing throw
throw :: Exception -> IO a
throw = throwIO
#endif

#ifdef __GLASGOW_HASKELL__
readMVar :: MVar a -> IO a
readMVar m =
  block $ do
    a <- takeMVar m
    putMVar m a
    return a

swapMVar :: MVar a -> a -> IO a
swapMVar mvar new = modifyMVar mvar (\old -> return (new,old))
#endif

-- put back the same value, return something
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io = 
  block $ do
    a <- takeMVar m
    b <- Exception.catch (unblock (io a))
      	    (\e -> do putMVar m a; throw e)
    putMVar m a
    return b

-- put back a new value, return ()
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io = 
  block $ do
    a  <- takeMVar m
    a' <- Exception.catch (unblock (io a))
      	    (\e -> do putMVar m a; throw e)
    putMVar m a'

-- put back a new value, return something
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io = 
  block $ do
    a      <- takeMVar m
    (a',b) <- Exception.catch (unblock (io a))
      	        (\e -> do putMVar m a; throw e)
    putMVar m a'
    return b
