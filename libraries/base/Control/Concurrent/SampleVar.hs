-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SampleVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: SampleVar.hs,v 1.2 2002/04/24 16:31:37 simonmar Exp $
--
-- Sample variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.SampleVar
       (
         SampleVar,         -- :: type _ =
 
	 newEmptySampleVar, -- :: IO (SampleVar a)
         newSampleVar,      -- :: a -> IO (SampleVar a)
	 emptySampleVar,    -- :: SampleVar a -> IO ()
	 readSampleVar,	    -- :: SampleVar a -> IO a
	 writeSampleVar	    -- :: SampleVar a -> a -> IO ()

       ) where

import Prelude

import Control.Concurrent.MVar

-- Sample variables are slightly different from a normal MVar:
-- 
--  * Reading an empty SampleVar causes the reader to block.
--    (same as takeMVar on empty MVar)
-- 
--  * Reading a filled SampleVar empties it and returns value.
--    (same as takeMVar)
-- 
--  * Writing to an empty SampleVar fills it with a value, and
--    potentially, wakes up a blocked reader (same as for putMVar on
--    empty MVar).
--
--  * Writing to a filled SampleVar overwrites the current value.
--    (different from putMVar on full MVar.)

type SampleVar a
 = MVar (Int,		-- 1  == full
			-- 0  == empty
			-- <0 no of readers blocked
          MVar a)

-- Initally, a SampleVar is empty/unfilled.

newEmptySampleVar :: IO (SampleVar a)
newEmptySampleVar = do
   v <- newEmptyMVar
   newMVar (0,v)

newSampleVar :: a -> IO (SampleVar a)
newSampleVar a = do
   v <- newEmptyMVar
   putMVar v a
   newMVar (1,v)

emptySampleVar :: SampleVar a -> IO ()
emptySampleVar v = do
   (readers, var) <- takeMVar v
   if readers >= 0 then
     putMVar v (0,var)
    else
     putMVar v (readers,var)

--
-- filled => make empty and grab sample
-- not filled => try to grab value, empty when read val.
--
readSampleVar :: SampleVar a -> IO a
readSampleVar svar = do
   (readers,val) <- takeMVar svar
   putMVar svar (readers-1,val)
   takeMVar val

--
-- filled => overwrite
-- not filled => fill, write val
--
writeSampleVar :: SampleVar a -> a -> IO ()
writeSampleVar svar v = do
   (readers,val) <- takeMVar svar
   case readers of
     1 -> 
       swapMVar val v >> 
       putMVar svar (1,val)
     _ -> 
       putMVar val v >> 
       putMVar svar (min 1 (readers+1), val)
