-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SampleVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Sample variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.SampleVar
       (
	 -- * Sample Variables
         SampleVar,         -- :: type _ =
 
	 newEmptySampleVar, -- :: IO (SampleVar a)
         newSampleVar,      -- :: a -> IO (SampleVar a)
	 emptySampleVar,    -- :: SampleVar a -> IO ()
	 readSampleVar,	    -- :: SampleVar a -> IO a
	 writeSampleVar,    -- :: SampleVar a -> a -> IO ()
	 isEmptySampleVar,  -- :: SampleVar a -> IO Bool

       ) where

import Prelude

import Control.Concurrent.MVar

-- |
-- Sample variables are slightly different from a normal 'MVar':
-- 
--  * Reading an empty 'SampleVar' causes the reader to block.
--    (same as 'takeMVar' on empty 'MVar')
-- 
--  * Reading a filled 'SampleVar' empties it and returns value.
--    (same as 'takeMVar')
-- 
--  * Writing to an empty 'SampleVar' fills it with a value, and
--    potentially, wakes up a blocked reader (same as for 'putMVar' on
--    empty 'MVar').
--
--  * Writing to a filled 'SampleVar' overwrites the current value.
--    (different from 'putMVar' on full 'MVar'.)

type SampleVar a
 = MVar (Int,		-- 1  == full
			-- 0  == empty
			-- <0 no of readers blocked
          MVar a)

-- |Build a new, empty, 'SampleVar'
newEmptySampleVar :: IO (SampleVar a)
newEmptySampleVar = do
   v <- newEmptyMVar
   newMVar (0,v)

-- |Build a 'SampleVar' with an initial value.
newSampleVar :: a -> IO (SampleVar a)
newSampleVar a = do
   v <- newEmptyMVar
   putMVar v a
   newMVar (1,v)

-- |If the SampleVar is full, leave it empty.  Otherwise, do nothing.
emptySampleVar :: SampleVar a -> IO ()
emptySampleVar v = do
   (readers, var) <- takeMVar v
   if readers > 0 then do
     takeMVar var
     putMVar v (0,var)
    else
     putMVar v (readers,var)

-- |Wait for a value to become available, then take it and return.
readSampleVar :: SampleVar a -> IO a
readSampleVar svar = do
--
-- filled => make empty and grab sample
-- not filled => try to grab value, empty when read val.
--
   (readers,val) <- takeMVar svar
   putMVar svar (readers-1,val)
   takeMVar val

-- |Write a value into the 'SampleVar', overwriting any previous value that
-- was there.
writeSampleVar :: SampleVar a -> a -> IO ()
writeSampleVar svar v = do
--
-- filled => overwrite
-- not filled => fill, write val
--
   (readers,val) <- takeMVar svar
   case readers of
     1 -> 
       swapMVar val v >> 
       putMVar svar (1,val)
     _ -> 
       putMVar val v >> 
       putMVar svar (min 1 (readers+1), val)

-- | Returns 'True' if the 'SampleVar' is currently empty.
--
-- Note that this function is only useful if you know that no other
-- threads can be modifying the state of the 'SampleVar', because
-- otherwise the state of the 'SampleVar' may have changed by the time
-- you see the result of 'isEmptySampleVar'.
--
isEmptySampleVar :: SampleVar a -> IO Bool
isEmptySampleVar svar = do
   (readers,val) <- readMVar svar
   return (readers == 0)

